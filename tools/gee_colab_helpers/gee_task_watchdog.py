#!/usr/bin/env python3
"""Watch newly launched GEE tasks and cancel any task above its EECU cap."""

from __future__ import annotations

import argparse
import json
import sys
import time
from datetime import datetime, timezone
from pathlib import Path
from typing import Any


ACTIVE_STATES = {"READY", "RUNNING", "PENDING", "CANCEL_REQUESTED", "CANCELLING"}


def utc_now() -> str:
    return datetime.now(timezone.utc).isoformat(timespec="seconds")


def operation_description(operation: dict[str, Any]) -> str:
    return str(operation.get("metadata", {}).get("description", ""))


def operation_state(operation: dict[str, Any]) -> str:
    return str(operation.get("metadata", {}).get("state", "")).upper()


def operation_eecu_hours(operation: dict[str, Any]) -> float:
    metadata = operation.get("metadata", {})
    value = metadata.get(
        "batchEecuUsageSeconds",
        metadata.get("batch_eecu_usage_seconds", 0),
    )
    return max(0.0, float(value or 0)) / 3_600


def append_event(path: Path | None, event: dict[str, Any]) -> None:
    line = json.dumps(event, sort_keys=True)
    print(line, flush=True)
    if path is not None:
        path.parent.mkdir(parents=True, exist_ok=True)
        with path.open("a", encoding="utf-8") as handle:
            handle.write(line + "\n")


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--project", required=True)
    parser.add_argument("--description-prefix", required=True)
    parser.add_argument("--warning-eecu-hours", type=float, default=10.0)
    parser.add_argument("--cancel-eecu-hours", type=float, default=25.0)
    parser.add_argument("--poll-seconds", type=int, default=30)
    parser.add_argument(
        "--wait-for-first-task-seconds",
        type=int,
        default=300,
        help="Wait this long for the launcher to create its first matching task.",
    )
    parser.add_argument("--log", type=Path)
    parser.add_argument(
        "--inspect-once",
        action="store_true",
        help="Report matching active tasks once without cancellation.",
    )
    args = parser.parse_args()
    if args.warning_eecu_hours <= 0 or args.cancel_eecu_hours <= 0:
        parser.error("EECU thresholds must be positive.")
    if args.warning_eecu_hours >= args.cancel_eecu_hours:
        parser.error("--warning-eecu-hours must be below --cancel-eecu-hours.")
    if args.poll_seconds < 10:
        parser.error("--poll-seconds must be at least 10.")
    if args.wait_for_first_task_seconds < 0:
        parser.error("--wait-for-first-task-seconds cannot be negative.")
    return args


def main() -> int:
    args = parse_args()
    try:
        import ee
    except ImportError as exc:
        raise RuntimeError("The Earth Engine Python API is required.") from exc

    ee.Initialize(project=args.project)
    started = time.monotonic()
    saw_matching_task = False
    warned: set[str] = set()
    cancellation_requested: set[str] = set()
    append_event(
        args.log,
        {
            "at_utc": utc_now(),
            "event": "watchdog_started",
            "project": args.project,
            "description_prefix": args.description_prefix,
            "warning_eecu_hours": args.warning_eecu_hours,
            "cancel_eecu_hours": args.cancel_eecu_hours,
        },
    )

    while True:
        operations = ee.data.listOperations()
        matching = [
            operation
            for operation in operations
            if operation_description(operation).startswith(
                args.description_prefix
            )
            and operation_state(operation) in ACTIVE_STATES
        ]
        if matching:
            saw_matching_task = True
        for operation in matching:
            name = str(operation.get("name", ""))
            description = operation_description(operation)
            state = operation_state(operation)
            eecu_hours = operation_eecu_hours(operation)
            if (
                eecu_hours >= args.warning_eecu_hours
                and name not in warned
            ):
                append_event(
                    args.log,
                    {
                        "at_utc": utc_now(),
                        "event": "high_usage_warning",
                        "operation": name,
                        "description": description,
                        "state": state,
                        "eecu_hours": round(eecu_hours, 3),
                    },
                )
                warned.add(name)
            if (
                not args.inspect_once
                and state == "RUNNING"
                and eecu_hours >= args.cancel_eecu_hours
                and name not in cancellation_requested
            ):
                ee.data.cancelOperation(name)
                append_event(
                    args.log,
                    {
                        "at_utc": utc_now(),
                        "event": "cancellation_requested",
                        "operation": name,
                        "description": description,
                        "eecu_hours": round(eecu_hours, 3),
                        "limit_eecu_hours": args.cancel_eecu_hours,
                    },
                )
                cancellation_requested.add(name)

        if args.inspect_once:
            append_event(
                args.log,
                {
                    "at_utc": utc_now(),
                    "event": "inspection_complete",
                    "matching_active_tasks": len(matching),
                },
            )
            return 0
        if saw_matching_task and not matching:
            append_event(
                args.log,
                {
                    "at_utc": utc_now(),
                    "event": "watchdog_complete",
                    "reason": "no_matching_active_tasks",
                },
            )
            return 0
        if (
            not saw_matching_task
            and time.monotonic() - started >= args.wait_for_first_task_seconds
        ):
            append_event(
                args.log,
                {
                    "at_utc": utc_now(),
                    "event": "watchdog_complete",
                    "reason": "no_matching_task_appeared",
                },
            )
            return 0
        time.sleep(args.poll_seconds)


if __name__ == "__main__":
    try:
        raise SystemExit(main())
    except Exception as exc:
        print(f"GEE WATCHDOG ERROR: {exc}", file=sys.stderr)
        raise
