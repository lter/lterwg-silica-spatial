#!/usr/bin/env python3
"""Fail-closed quota gate for every Earth Engine batch-task submission.

Run this command immediately before a launcher.  An approved receipt is valid
for 15 minutes, for one exact workflow/task count, and may be consumed only
once.  Launchers must call ``consume_preflight_receipt`` before starting tasks.
Read-only status checks and local planning do not require a receipt.
"""

from __future__ import annotations

import argparse
import hashlib
import json
import math
import re
import statistics
import subprocess
import sys
from datetime import datetime, timedelta, timezone
from pathlib import Path
from typing import Any, Iterable
from urllib.parse import quote
from zoneinfo import ZoneInfo


DEFAULT_PROJECT = "silica-synthesis"
DEFAULT_MONTHLY_LIMIT_HOURS = 1_000.0
MONTHLY_STOP_FRACTION = 0.70
MAX_BATCH_FRACTION = 0.10
MAX_SINGLE_TASK_FRACTION = 0.025
MAX_ACTIVE_TASKS = 5
MAX_EFFECTIVE_PIXELS_PER_TASK = 100_000_000
RECEIPT_VALID_MINUTES = 15
ACTIVE_STATES = {"READY", "RUNNING", "PENDING", "CANCEL_REQUESTED", "CANCELLING"}
COMPLETED_USAGE_METRIC = "earthengine.googleapis.com/project/cpu/usage_time"
IN_PROGRESS_USAGE_METRIC = (
    "earthengine.googleapis.com/project/cpu/in_progress_usage_time"
)
WORKFLOW_PATTERN = re.compile(r"^[a-z0-9][a-z0-9_-]*$")


def utc_now() -> datetime:
    return datetime.now(timezone.utc)


def iso_utc(value: datetime) -> str:
    return value.astimezone(timezone.utc).isoformat(timespec="seconds").replace(
        "+00:00", "Z"
    )


def month_start_utc(now: datetime, timezone_name: str) -> datetime:
    local = now.astimezone(ZoneInfo(timezone_name))
    return local.replace(
        day=1,
        hour=0,
        minute=0,
        second=0,
        microsecond=0,
    ).astimezone(timezone.utc)


def percentile_nearest_rank(values: list[float], probability: float) -> float:
    if not values:
        return 0.0
    ordered = sorted(values)
    rank = max(1, math.ceil(probability * len(ordered)))
    return ordered[rank - 1]


def canonical_checksum(payload: dict[str, Any]) -> str:
    unsigned = {key: value for key, value in payload.items() if key != "checksum"}
    canonical = json.dumps(
        unsigned,
        sort_keys=True,
        separators=(",", ":"),
        ensure_ascii=True,
    )
    return hashlib.sha256(canonical.encode("utf-8")).hexdigest()


def task_description(task: dict[str, Any]) -> str:
    return str(
        task.get("description")
        or task.get("metadata", {}).get("description")
        or task.get("config", {}).get("description")
        or ""
    )


def task_state(task: dict[str, Any]) -> str:
    return str(
        task.get("state")
        or task.get("metadata", {}).get("state")
        or ""
    ).upper()


def task_eecu_seconds(task: dict[str, Any]) -> float:
    candidates = (
        task.get("batch_eecu_usage_seconds"),
        task.get("batchEecuUsageSeconds"),
        task.get("metadata", {}).get("batch_eecu_usage_seconds"),
        task.get("metadata", {}).get("batchEecuUsageSeconds"),
    )
    for value in candidates:
        if value is not None:
            try:
                return max(0.0, float(value))
            except (TypeError, ValueError):
                pass
    return 0.0


def summarize_tasks(
    tasks: Iterable[dict[str, Any]],
    description_prefix: str,
) -> dict[str, Any]:
    task_list = list(tasks)
    active = [task for task in task_list if task_state(task) in ACTIVE_STATES]
    matching = [
        task
        for task in task_list
        if task_description(task).startswith(description_prefix)
    ]
    matching_hours = [
        task_eecu_seconds(task) / 3_600
        for task in matching
        if task_eecu_seconds(task) > 0
    ]
    expensive = sorted(
        (
            {
                "description": task_description(task),
                "state": task_state(task),
                "eecu_hours": round(task_eecu_seconds(task) / 3_600, 3),
            }
            for task in matching
            if task_eecu_seconds(task) > 0
        ),
        key=lambda row: row["eecu_hours"],
        reverse=True,
    )[:10]
    return {
        "active_task_count": len(active),
        "active_task_eecu_hours": round(
            sum(task_eecu_seconds(task) for task in active) / 3_600,
            6,
        ),
        "active_state_counts": {
            state: sum(task_state(task) == state for task in active)
            for state in sorted({task_state(task) for task in active})
        },
        "matching_task_count": len(matching),
        "matching_tasks_with_usage": len(matching_hours),
        "historical_eecu_hours": {
            "median": round(statistics.median(matching_hours), 3)
            if matching_hours
            else None,
            "p90": round(percentile_nearest_rank(matching_hours, 0.90), 3)
            if matching_hours
            else None,
            "max": round(max(matching_hours), 3) if matching_hours else None,
        },
        "highest_cost_matching_tasks": expensive,
    }


def monitoring_value_seconds(point: dict[str, Any]) -> float:
    value = point.get("value", {})
    for key in ("doubleValue", "int64Value"):
        if key in value:
            return float(value[key])
    return 0.0


def query_monitoring_metric_seconds(
    session: Any,
    project: str,
    metric_type: str,
    start: datetime,
    end: datetime,
) -> float:
    endpoint = (
        "https://monitoring.googleapis.com/v3/projects/"
        f"{quote(project, safe='')}/timeSeries"
    )
    params = {
        "filter": f'metric.type="{metric_type}"',
        "interval.startTime": iso_utc(start),
        "interval.endTime": iso_utc(end),
        "aggregation.alignmentPeriod": "86400s",
        "aggregation.perSeriesAligner": "ALIGN_SUM",
        "aggregation.crossSeriesReducer": "REDUCE_SUM",
        "view": "FULL",
        "pageSize": 1000,
    }
    seconds = 0.0
    while True:
        response = session.get(endpoint, params=params, timeout=60)
        response.raise_for_status()
        payload = response.json()
        for series in payload.get("timeSeries", []):
            seconds += sum(
                monitoring_value_seconds(point)
                for point in series.get("points", [])
            )
        token = payload.get("nextPageToken")
        if not token:
            return seconds
        params["pageToken"] = token


def fetch_live_inputs(
    project: str,
    description_prefix: str,
    timezone_name: str,
    now: datetime | None = None,
) -> tuple[dict[str, float], dict[str, Any], dict[str, str]]:
    try:
        import ee
        from google.auth.transport.requests import AuthorizedSession
    except ImportError as exc:
        raise RuntimeError(
            "The Earth Engine Python API and Google auth packages are required."
        ) from exc

    checked_at = now or utc_now()
    ee.Initialize(project=project)
    credentials = ee.data.get_persistent_credentials().with_quota_project(project)
    session = AuthorizedSession(credentials)
    start = month_start_utc(checked_at, timezone_name)
    completed_seconds = query_monitoring_metric_seconds(
        session,
        project,
        COMPLETED_USAGE_METRIC,
        start,
        checked_at,
    )
    in_progress_seconds = query_monitoring_metric_seconds(
        session,
        project,
        IN_PROGRESS_USAGE_METRIC,
        start,
        checked_at,
    )
    tasks = ee.data.getTaskList()
    task_summary = summarize_tasks(tasks, description_prefix)
    # The completed and in-progress metrics overlap for successful work: the
    # latter is emitted incrementally while a request runs, while the former is
    # emitted when it succeeds. Use the larger month-to-date stream, never
    # their sum, and retain both values for auditability.
    monitoring = {
        "completed_eecu_hours": completed_seconds / 3_600,
        "in_progress_eecu_hours": in_progress_seconds / 3_600,
        "currently_active_task_eecu_hours": task_summary[
            "active_task_eecu_hours"
        ],
        "observed_eecu_hours": max(
            completed_seconds,
            in_progress_seconds,
        )
        / 3_600,
    }
    interval = {
        "start_utc": iso_utc(start),
        "end_utc": iso_utc(checked_at),
        "timezone": timezone_name,
    }
    return monitoring, task_summary, interval


def evaluate_preflight(
    *,
    proposed_task_count: int,
    monthly_limit_hours: float,
    monitoring: dict[str, float],
    task_summary: dict[str, Any],
    max_task_area_km2: float | None,
    scale_m: float | None,
    time_slices_per_task: int,
) -> dict[str, Any]:
    blockers: list[str] = []
    warnings: list[str] = []
    observed_hours = float(monitoring["observed_eecu_hours"])
    stop_hours = monthly_limit_hours * MONTHLY_STOP_FRACTION
    max_batch_hours = monthly_limit_hours * MAX_BATCH_FRACTION
    max_single_task_hours = monthly_limit_hours * MAX_SINGLE_TASK_FRACTION
    active_count = int(task_summary["active_task_count"])
    historical = task_summary["historical_eecu_hours"]
    p90_hours = historical.get("p90")
    max_hours = historical.get("max")
    projected_hours = (
        float(p90_hours) * proposed_task_count if p90_hours is not None else None
    )
    effective_pixels = None
    if max_task_area_km2 is not None and scale_m is not None:
        effective_pixels = (
            max_task_area_km2
            * 1_000_000
            / (scale_m * scale_m)
            * time_slices_per_task
        )

    if observed_hours >= stop_hours:
        blockers.append(
            f"Month-to-date usage is {observed_hours:.1f} EECU-h, at or above "
            f"the {stop_hours:.1f} EECU-h safety stop (70% of the monthly limit)."
        )
    elif observed_hours >= monthly_limit_hours * 0.50:
        warnings.append(
            f"Month-to-date usage has reached {observed_hours:.1f} EECU-h "
            "(at least 50% of the monthly limit)."
        )

    if active_count > MAX_ACTIVE_TASKS:
        blockers.append(
            f"{active_count} Earth Engine tasks are already active; the safety "
            f"maximum is {MAX_ACTIVE_TASKS}."
        )

    if proposed_task_count < 1:
        blockers.append("The proposed task count must be at least one.")

    if p90_hours is None and proposed_task_count > 1:
        blockers.append(
            "This task prefix has no measured EECU history. Launch exactly one "
            "smoke-test task, measure it, then run preflight again."
        )
    elif projected_hours is not None and projected_hours > max_batch_hours:
        blockers.append(
            f"Historical p90 cost projects this batch at {projected_hours:.1f} "
            f"EECU-h, above the {max_batch_hours:.1f} EECU-h per-batch limit."
        )

    if max_hours is not None and float(max_hours) > max_single_task_hours:
        blockers.append(
            f"A matching task has already consumed {float(max_hours):.1f} "
            f"EECU-h, above the {max_single_task_hours:.1f} EECU-h single-task limit."
        )

    if effective_pixels is not None and effective_pixels > MAX_EFFECTIVE_PIXELS_PER_TASK:
        blockers.append(
            f"The largest proposed task represents about {effective_pixels:,.0f} "
            f"pixel-time evaluations, above the {MAX_EFFECTIVE_PIXELS_PER_TASK:,} "
            "safety limit. Split the sites/geometry or coarsen the scale."
        )

    if max_task_area_km2 is None or scale_m is None:
        warnings.append(
            "No geometry-based estimate was available; historical usage and the "
            "one-task unknown-workflow rule remain enforced."
        )

    if proposed_task_count > 10:
        warnings.append(
            f"The request contains {proposed_task_count} tasks; use small batches "
            "even when historical cost is low."
        )

    return {
        "approved": not blockers,
        "blockers": blockers,
        "warnings": warnings,
        "limits": {
            "monthly_limit_eecu_hours": monthly_limit_hours,
            "monthly_stop_eecu_hours": stop_hours,
            "max_batch_eecu_hours": max_batch_hours,
            "max_single_task_eecu_hours": max_single_task_hours,
            "max_active_tasks": MAX_ACTIVE_TASKS,
            "max_effective_pixels_per_task": MAX_EFFECTIVE_PIXELS_PER_TASK,
        },
        "estimates": {
            "historical_p90_projected_batch_eecu_hours": round(
                projected_hours, 3
            )
            if projected_hours is not None
            else None,
            "effective_pixels_per_largest_task": round(effective_pixels)
            if effective_pixels is not None
            else None,
        },
    }


def build_receipt(
    *,
    project: str,
    workflow: str,
    description_prefix: str,
    proposed_task_count: int,
    site_count: int | None,
    max_task_area_km2: float | None,
    scale_m: float | None,
    time_slices_per_task: int,
    monthly_limit_hours: float,
    monitoring: dict[str, float],
    task_summary: dict[str, Any],
    interval: dict[str, str],
    now: datetime | None = None,
) -> dict[str, Any]:
    issued = now or utc_now()
    decision = evaluate_preflight(
        proposed_task_count=proposed_task_count,
        monthly_limit_hours=monthly_limit_hours,
        monitoring=monitoring,
        task_summary=task_summary,
        max_task_area_km2=max_task_area_km2,
        scale_m=scale_m,
        time_slices_per_task=time_slices_per_task,
    )
    receipt = {
        "schema_version": 1,
        "project": project,
        "workflow": workflow,
        "description_prefix": description_prefix,
        "proposed_task_count": proposed_task_count,
        "site_count": site_count,
        "max_task_area_km2": max_task_area_km2,
        "scale_m": scale_m,
        "time_slices_per_task": time_slices_per_task,
        "issued_at_utc": iso_utc(issued),
        "expires_at_utc": iso_utc(
            issued + timedelta(minutes=RECEIPT_VALID_MINUTES)
        ),
        "monitoring_interval": interval,
        "monitoring": {
            key: round(float(value), 6) for key, value in monitoring.items()
        },
        "task_summary": task_summary,
        "decision": decision,
    }
    receipt["checksum"] = canonical_checksum(receipt)
    return receipt


def consumed_path(receipt_path: Path) -> Path:
    return receipt_path.with_name(receipt_path.name + ".consumed.json")


def validate_preflight_receipt(
    receipt_path: Path,
    *,
    project: str,
    workflow: str,
    description_prefix: str,
    proposed_task_count: int,
    site_count: int | None = None,
    max_task_area_km2: float | None = None,
    scale_m: float | None = None,
    time_slices_per_task: int | None = None,
    now: datetime | None = None,
) -> dict[str, Any]:
    if not receipt_path.exists():
        raise RuntimeError(
            f"Missing GEE preflight receipt: {receipt_path}. Run "
            "tools/gee_colab_helpers/gee_quota_preflight.py first."
        )
    already_consumed = consumed_path(receipt_path)
    if already_consumed.exists():
        raise RuntimeError(
            f"GEE preflight receipt was already consumed: {already_consumed}. "
            "Run a fresh preflight."
        )
    receipt = json.loads(receipt_path.read_text(encoding="utf-8"))
    if receipt.get("checksum") != canonical_checksum(receipt):
        raise RuntimeError("GEE preflight receipt checksum is invalid.")
    if not receipt.get("decision", {}).get("approved"):
        blockers = receipt.get("decision", {}).get("blockers", [])
        raise RuntimeError(
            "GEE preflight did not approve this launch: " + "; ".join(blockers)
        )
    expected = {
        "project": project,
        "workflow": workflow,
        "description_prefix": description_prefix,
        "proposed_task_count": proposed_task_count,
    }
    mismatches = [
        f"{key}={receipt.get(key)!r} (expected {value!r})"
        for key, value in expected.items()
        if receipt.get(key) != value
    ]
    if mismatches:
        raise RuntimeError(
            "GEE preflight receipt does not match this launch: "
            + "; ".join(mismatches)
        )
    optional_expected = {
        "site_count": site_count,
        "max_task_area_km2": max_task_area_km2,
        "scale_m": scale_m,
        "time_slices_per_task": time_slices_per_task,
    }
    optional_mismatches = []
    for key, expected_value in optional_expected.items():
        if expected_value is None:
            continue
        actual_value = receipt.get(key)
        if isinstance(expected_value, float):
            matches = actual_value is not None and math.isclose(
                float(actual_value),
                expected_value,
                rel_tol=1e-9,
                abs_tol=1e-6,
            )
        else:
            matches = actual_value == expected_value
        if not matches:
            optional_mismatches.append(
                f"{key}={actual_value!r} (expected {expected_value!r})"
            )
    if optional_mismatches:
        raise RuntimeError(
            "GEE preflight receipt has the wrong workload dimensions: "
            + "; ".join(optional_mismatches)
        )
    current = now or utc_now()
    expires = datetime.fromisoformat(
        str(receipt["expires_at_utc"]).replace("Z", "+00:00")
    )
    if current >= expires:
        raise RuntimeError(
            f"GEE preflight receipt expired at {iso_utc(expires)}. "
            "Run a fresh preflight."
        )
    return receipt


def consume_preflight_receipt(
    receipt_path: Path,
    *,
    project: str,
    workflow: str,
    description_prefix: str,
    proposed_task_count: int,
    site_count: int | None = None,
    max_task_area_km2: float | None = None,
    scale_m: float | None = None,
    time_slices_per_task: int | None = None,
    now: datetime | None = None,
    start_watchdog: bool = True,
) -> dict[str, Any]:
    receipt = validate_preflight_receipt(
        receipt_path,
        project=project,
        workflow=workflow,
        description_prefix=description_prefix,
        proposed_task_count=proposed_task_count,
        site_count=site_count,
        max_task_area_km2=max_task_area_km2,
        scale_m=scale_m,
        time_slices_per_task=time_slices_per_task,
        now=now,
    )
    consumed_at = now or utc_now()
    marker = {
        "consumed_at_utc": iso_utc(consumed_at),
        "receipt": str(receipt_path),
        "checksum": receipt["checksum"],
        "project": project,
        "workflow": workflow,
        "description_prefix": description_prefix,
        "proposed_task_count": proposed_task_count,
    }
    marker_path = consumed_path(receipt_path)
    marker_path.write_text(
        json.dumps(marker, indent=2, sort_keys=True) + "\n",
        encoding="utf-8",
    )
    if start_watchdog:
        watchdog_script = Path(__file__).with_name("gee_task_watchdog.py")
        watchdog_log = receipt_path.with_name(
            receipt_path.name + ".watchdog.jsonl"
        )
        watchdog_command = [
            sys.executable,
            str(watchdog_script),
            "--project",
            project,
            "--description-prefix",
            description_prefix,
            "--warning-eecu-hours",
            "10",
            "--cancel-eecu-hours",
            str(
                receipt.get("decision", {})
                .get("limits", {})
                .get("max_single_task_eecu_hours", 25)
            ),
            "--log",
            str(watchdog_log),
        ]
        process = subprocess.Popen(
            watchdog_command,
            stdin=subprocess.DEVNULL,
            stdout=subprocess.DEVNULL,
            stderr=subprocess.DEVNULL,
            start_new_session=True,
        )
        marker["watchdog_pid"] = process.pid
        marker["watchdog_log"] = str(watchdog_log)
        marker_path.write_text(
            json.dumps(marker, indent=2, sort_keys=True) + "\n",
            encoding="utf-8",
        )
    return receipt


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--project", default=DEFAULT_PROJECT)
    parser.add_argument("--workflow", required=True)
    parser.add_argument("--description-prefix", required=True)
    parser.add_argument("--proposed-task-count", type=int, required=True)
    parser.add_argument("--site-count", type=int)
    parser.add_argument("--max-task-area-km2", type=float)
    parser.add_argument("--scale-m", type=float)
    parser.add_argument("--time-slices-per-task", type=int, default=1)
    parser.add_argument(
        "--monthly-limit-hours",
        type=float,
        default=DEFAULT_MONTHLY_LIMIT_HOURS,
    )
    parser.add_argument(
        "--timezone",
        default="America/Los_Angeles",
        help="Timezone used to define the monthly accounting boundary.",
    )
    parser.add_argument("--receipt", type=Path, required=True)
    args = parser.parse_args()
    if not WORKFLOW_PATTERN.fullmatch(args.workflow):
        parser.error("--workflow may contain lowercase letters, numbers, _ and -.")
    if args.proposed_task_count < 1:
        parser.error("--proposed-task-count must be positive.")
    if args.site_count is not None and args.site_count < 1:
        parser.error("--site-count must be positive.")
    if args.max_task_area_km2 is not None and args.max_task_area_km2 <= 0:
        parser.error("--max-task-area-km2 must be positive.")
    if args.scale_m is not None and args.scale_m <= 0:
        parser.error("--scale-m must be positive.")
    if (args.max_task_area_km2 is None) != (args.scale_m is None):
        parser.error("--max-task-area-km2 and --scale-m must be supplied together.")
    if args.time_slices_per_task < 1:
        parser.error("--time-slices-per-task must be positive.")
    if args.monthly_limit_hours <= 0:
        parser.error("--monthly-limit-hours must be positive.")
    return args


def print_report(receipt: dict[str, Any], receipt_path: Path) -> None:
    monitoring = receipt["monitoring"]
    summary = receipt["task_summary"]
    decision = receipt["decision"]
    status = "APPROVED" if decision["approved"] else "BLOCKED"
    print(f"GEE QUOTA PREFLIGHT: {status}")
    print(
        "Month-to-date EECU: "
        f"{monitoring['observed_eecu_hours']:.1f} h "
        f"(completed stream {monitoring['completed_eecu_hours']:.1f} h; "
        f"in-progress stream {monitoring['in_progress_eecu_hours']:.1f} h; "
        "overlapping streams are not added)"
    )
    print(
        f"Existing active tasks: {summary['active_task_count']} "
        f"{json.dumps(summary['active_state_counts'], sort_keys=True)}"
    )
    historical = summary["historical_eecu_hours"]
    print(
        "Matching task history (EECU-h): "
        f"median={historical['median']}, p90={historical['p90']}, "
        f"max={historical['max']}"
    )
    for warning in decision["warnings"]:
        print(f"WARNING: {warning}")
    for blocker in decision["blockers"]:
        print(f"BLOCKER: {blocker}")
    print(f"Receipt: {receipt_path}")
    if decision["approved"]:
        print(f"Receipt expires: {receipt['expires_at_utc']} and can be used once.")
    else:
        print("No Earth Engine launcher may accept this blocked receipt.")


def main() -> int:
    args = parse_args()
    checked_at = utc_now()
    monitoring, task_summary, interval = fetch_live_inputs(
        args.project,
        args.description_prefix,
        args.timezone,
        checked_at,
    )
    receipt = build_receipt(
        project=args.project,
        workflow=args.workflow,
        description_prefix=args.description_prefix,
        proposed_task_count=args.proposed_task_count,
        site_count=args.site_count,
        max_task_area_km2=args.max_task_area_km2,
        scale_m=args.scale_m,
        time_slices_per_task=args.time_slices_per_task,
        monthly_limit_hours=args.monthly_limit_hours,
        monitoring=monitoring,
        task_summary=task_summary,
        interval=interval,
        now=checked_at,
    )
    args.receipt.parent.mkdir(parents=True, exist_ok=True)
    args.receipt.write_text(
        json.dumps(receipt, indent=2, sort_keys=True) + "\n",
        encoding="utf-8",
    )
    old_marker = consumed_path(args.receipt)
    if old_marker.exists():
        old_marker.unlink()
    print_report(receipt, args.receipt)
    return 0 if receipt["decision"]["approved"] else 2


if __name__ == "__main__":
    try:
        raise SystemExit(main())
    except Exception as exc:
        print(f"GEE QUOTA PREFLIGHT ERROR: {exc}", file=sys.stderr)
        raise
