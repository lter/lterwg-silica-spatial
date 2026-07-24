#!/usr/bin/env python3
"""Run four accepted MODIS extractions one watershed at a time.

The preparation script gives each watershed its own cropped rasters and accepted
geometry. This runner keeps that isolation, records timing by watershed and
driver, and resumes from completed driver outputs without repeating them.
"""

from __future__ import annotations

import argparse
import csv
import os
import re
import shutil
import subprocess
import threading
from concurrent.futures import ThreadPoolExecutor, as_completed
from datetime import datetime, timezone
from pathlib import Path


DEFAULT_ROOT = Path("generated_outputs/appeears/targeted-extraction-inputs")
DRIVERS = ("npp", "greenup", "evapo", "snow")
OUTPUT_STEMS = {
    "npp": "si-extract_npp_v061",
    "greenup": "si-extract_greenup_v061",
    "evapo": "si-extract_evapo_v061",
    "snow": "si-extract_snow_v061",
}
DRIVER_FIELDS = (
    "watershed_key",
    "task_name",
    "driver",
    "started_at_utc",
    "completed_at_utc",
    "elapsed_hours",
    "status",
    "output_file",
    "log_file",
    "message",
)
SITE_FIELDS = (
    "watershed_key",
    "task_name",
    "started_at_utc",
    "completed_at_utc",
    "elapsed_hours",
    "status",
    "completed_drivers",
    "message",
)


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(
        description="Run isolated AppEEARS extractions for accepted watersheds."
    )
    parser.add_argument("--run-root", type=Path, default=DEFAULT_ROOT)
    parser.add_argument(
        "--run-label",
        required=True,
        help="Short label used in output filenames and workflow logs.",
    )
    parser.add_argument("--output-date", required=True, help="Output date as YYYYMMDD.")
    parser.add_argument("--start-year", type=int, default=2002)
    parser.add_argument("--end-year", type=int, default=2022)
    parser.add_argument(
        "--exclude-task-pattern",
        action="append",
        default=[],
        help="Reject a matching task name; may be repeated.",
    )
    parser.add_argument(
        "--watershed-key",
        action="append",
        default=[],
        help="Run only this prepared watershed. Repeat to select more than one.",
    )
    parser.add_argument(
        "--workers",
        type=int,
        default=3,
        help="Number of watersheds to process at once. The default is three.",
    )
    parser.add_argument(
        "--reconcile-only",
        action="store_true",
        help="Rebuild timing from completed outputs and logs without running extraction.",
    )
    args = parser.parse_args()
    if not re.fullmatch(r"[0-9]{8}", args.output_date):
        parser.error("--output-date must use YYYYMMDD.")
    if args.start_year > args.end_year:
        parser.error("--start-year cannot be later than --end-year.")
    return args


def utc_now() -> datetime:
    return datetime.now(timezone.utc)


def iso_utc(value: datetime) -> str:
    return value.isoformat().replace("+00:00", "Z")


def parse_time(value: str) -> datetime | None:
    if not value:
        return None
    return datetime.fromisoformat(value.replace("Z", "+00:00"))


def elapsed_hours(start: str, finish: str) -> str:
    start_time = parse_time(start)
    finish_time = parse_time(finish)
    if not start_time or not finish_time:
        return ""
    return f"{(finish_time - start_time).total_seconds() / 3600:.6f}"


def read_csv(path: Path) -> list[dict[str, str]]:
    if not path.exists():
        return []
    with path.open(newline="", encoding="utf-8") as handle:
        return list(csv.DictReader(handle))


def write_csv_atomic(path: Path, rows: list[dict[str, str]], fields: tuple[str, ...]) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    temporary = path.with_suffix(path.suffix + ".tmp")
    with temporary.open("w", newline="", encoding="utf-8") as handle:
        writer = csv.DictWriter(
            handle,
            fieldnames=fields,
            extrasaction="ignore",
            lineterminator="\n",
        )
        writer.writeheader()
        writer.writerows(rows)
    temporary.replace(path)


class TimingLedger:
    def __init__(self, run_root: Path) -> None:
        self.driver_path = run_root / "extraction_driver_timing.csv"
        self.site_path = run_root / "extraction_timing.csv"
        self.driver_rows = {
            (row["watershed_key"], row["driver"]): row
            for row in read_csv(self.driver_path)
        }
        self.site_rows = {
            row["watershed_key"]: row for row in read_csv(self.site_path)
        }
        self.lock = threading.Lock()

    def save(self) -> None:
        driver_rows = sorted(
            self.driver_rows.values(), key=lambda row: (row["watershed_key"], row["driver"])
        )
        site_rows = sorted(self.site_rows.values(), key=lambda row: row["watershed_key"])
        write_csv_atomic(self.driver_path, driver_rows, DRIVER_FIELDS)
        write_csv_atomic(self.site_path, site_rows, SITE_FIELDS)

    def driver_row(self, watershed_key: str, driver: str) -> dict[str, str] | None:
        with self.lock:
            row = self.driver_rows.get((watershed_key, driver))
            return dict(row) if row else None

    def start_site(self, site: dict[str, str]) -> None:
        with self.lock:
            previous = self.site_rows.get(site["watershed_key"], {})
            self.site_rows[site["watershed_key"]] = {
                "watershed_key": site["watershed_key"],
                "task_name": site["task_name"],
                "started_at_utc": previous.get("started_at_utc") or iso_utc(utc_now()),
                "completed_at_utc": "",
                "elapsed_hours": "",
                "status": "in_progress",
                "completed_drivers": previous.get("completed_drivers", "0"),
                "message": "The accepted watershed extraction is running.",
            }
            self.save()

    def finish_site(
        self, site: dict[str, str], status: str, completed_drivers: int, message: str
    ) -> None:
        with self.lock:
            row = self.site_rows[site["watershed_key"]]
            finish = iso_utc(utc_now())
            row.update(
                {
                    "completed_at_utc": finish if status in {"complete", "failed"} else "",
                    "elapsed_hours": elapsed_hours(row["started_at_utc"], finish),
                    "status": status,
                    "completed_drivers": str(completed_drivers),
                    "message": message,
                }
            )
            self.save()

    def start_driver(
        self,
        site: dict[str, str],
        driver: str,
        output_file: Path,
        log_file: Path,
    ) -> str:
        with self.lock:
            previous = self.driver_rows.get((site["watershed_key"], driver), {})
            started = previous.get("started_at_utc") or iso_utc(utc_now())
            self.driver_rows[(site["watershed_key"], driver)] = {
                "watershed_key": site["watershed_key"],
                "task_name": site["task_name"],
                "driver": driver,
                "started_at_utc": started,
                "completed_at_utc": "",
                "elapsed_hours": "",
                "status": "in_progress",
                "output_file": str(output_file),
                "log_file": str(log_file),
                "message": f"The {driver} extraction is running.",
            }
            self.save()
            return started

    def finish_driver(
        self,
        site: dict[str, str],
        driver: str,
        status: str,
        message: str,
    ) -> None:
        with self.lock:
            row = self.driver_rows[(site["watershed_key"], driver)]
            finish = iso_utc(utc_now())
            row.update(
                {
                    "completed_at_utc": finish,
                    "elapsed_hours": elapsed_hours(row["started_at_utc"], finish),
                    "status": status,
                    "message": message,
                }
            )
            self.save()


def output_path(site: dict[str, str], driver: str, run_tag: str) -> Path:
    return Path(site["output_dir"]) / f"{OUTPUT_STEMS[driver]}_{run_tag}.csv"


def reconcile_completed_timing(
    sites: list[dict[str, str]],
    ledger: TimingLedger,
    run_tag: str,
) -> int:
    """Restore timing rows when concurrent watershed workers finish out of order."""
    completed_sites = 0
    with ledger.lock:
        for site in sites:
            driver_times: list[tuple[str, str]] = []
            all_complete = True
            for driver in DRIVERS:
                destination = output_path(site, driver, run_tag)
                log_file = Path(site["site_root"]) / "logs" / f"extract-{driver}.log"
                if not destination.exists() or destination.stat().st_size == 0:
                    all_complete = False
                    continue
                existing = ledger.driver_rows.get((site["watershed_key"], driver), {})
                started = existing.get("started_at_utc", "")
                if not started and log_file.exists():
                    matches = re.findall(
                        rf"\[([^\]]+)\] Starting {re.escape(driver)} extraction\.",
                        log_file.read_text(encoding="utf-8", errors="replace"),
                    )
                    if matches:
                        started = matches[-1]
                completed = iso_utc(
                    datetime.fromtimestamp(destination.stat().st_mtime, timezone.utc)
                )
                ledger.driver_rows[(site["watershed_key"], driver)] = {
                    "watershed_key": site["watershed_key"],
                    "task_name": site["task_name"],
                    "driver": driver,
                    "started_at_utc": started,
                    "completed_at_utc": existing.get("completed_at_utc") or completed,
                    "elapsed_hours": elapsed_hours(
                        started, existing.get("completed_at_utc") or completed
                    ),
                    "status": "complete",
                    "output_file": str(destination),
                    "log_file": str(log_file),
                    "message": f"The {driver} output was written and is ready for QA.",
                }
                driver_times.append(
                    (
                        started,
                        existing.get("completed_at_utc") or completed,
                    )
                )
            if all_complete and len(driver_times) == len(DRIVERS):
                starts = [start for start, _ in driver_times if start]
                finishes = [finish for _, finish in driver_times if finish]
                started = min(starts) if starts else ""
                completed = max(finishes) if finishes else ""
                ledger.site_rows[site["watershed_key"]] = {
                    "watershed_key": site["watershed_key"],
                    "task_name": site["task_name"],
                    "started_at_utc": started,
                    "completed_at_utc": completed,
                    "elapsed_hours": elapsed_hours(started, completed),
                    "status": "complete",
                    "completed_drivers": str(len(DRIVERS)),
                    "message": "All four MODIS driver outputs are ready for watershed-level QA.",
                }
                completed_sites += 1
        ledger.save()
    return completed_sites


def extraction_environment(
    site: dict[str, str],
    driver: str,
    args: argparse.Namespace,
) -> dict[str, str]:
    environment = os.environ.copy()
    environment.update(
        {
            "SILICA_BASE_FILE": site["base_file"],
            "SILICA_WATERSHED_FILE": site["watershed_file"],
            "SILICA_RAW_DRIVER_DIR": site["raw_driver_dir"],
            "SILICA_EXTRACTED_DIR": site["output_dir"],
            "SILICA_QA_ROOT": site["qa_dir"],
            "SILICA_RUN_STATIC_DRIVERS": "false",
            "SILICA_RUN_DYNAMIC_DRIVERS": "true",
            "SILICA_DYNAMIC_DRIVER_NAMES": driver,
            "SILICA_SKIP_WATERSHED_REBUILD": "true",
            "SILICA_TARGET_YEAR_START": str(args.start_year),
            "SILICA_TARGET_YEAR_END": str(args.end_year),
            "SILICA_OUTPUT_DATE": args.output_date,
            "SILICA_RUN_LABEL": args.run_label,
            "SILICA_ALLOW_OVERWRITE": "false",
            "SILICA_RESUME_PARTIALS": "true",
            "SILICA_SNOW_KEEP_PARTIAL_2001": "false",
            "SILICA_SNOW_TERRA_FALLBACK": "true",
        }
    )
    return environment


def run_site(
    site: dict[str, str],
    ledger: TimingLedger,
    repo_root: Path,
    rscript: str,
    args: argparse.Namespace,
    run_tag: str,
) -> tuple[str, bool, str]:
    ledger.start_site(site)
    completed = 0

    for driver in DRIVERS:
        destination = output_path(site, driver, run_tag)
        log_file = Path(site["site_root"]) / "logs" / f"extract-{driver}.log"
        timing = ledger.driver_row(site["watershed_key"], driver)
        output_is_ready = destination.exists() and destination.stat().st_size > 0
        if timing and timing.get("status") == "complete" and output_is_ready:
            completed += 1
            continue

        log_file.parent.mkdir(parents=True, exist_ok=True)
        ledger.start_driver(site, driver, destination, log_file)
        command = [
            rscript,
            "03_spatial_extraction/wrappers/run-targeted-subset-workflow.R",
            "--subset",
            site["subset_file"],
            "--combine-full",
            "false",
        ]
        with log_file.open("a", encoding="utf-8") as log:
            log.write(f"\n[{iso_utc(utc_now())}] Starting {driver} extraction.\n")
            result = subprocess.run(
                command,
                cwd=repo_root,
                env=extraction_environment(site, driver, args),
                stdout=log,
                stderr=subprocess.STDOUT,
                check=False,
            )

        if result.returncode != 0 or not destination.exists() or destination.stat().st_size == 0:
            message = (
                f"The {driver} extraction stopped with exit code {result.returncode}; "
                f"see {log_file}."
            )
            ledger.finish_driver(site, driver, "failed", message)
            ledger.finish_site(site, "failed", completed, message)
            return site["watershed_key"], False, message

        completed += 1
        ledger.finish_driver(
            site,
            driver,
            "complete",
            f"The {driver} output was written and is ready for QA.",
        )

    message = "All four MODIS driver outputs are ready for watershed-level QA."
    ledger.finish_site(site, "complete", completed, message)
    return site["watershed_key"], True, message


def main() -> None:
    args = parse_args()
    repo_root = Path.cwd().resolve()
    run_root = args.run_root.resolve()
    manifest_path = run_root / "run_manifest.csv"
    if not manifest_path.exists():
        raise SystemExit(
            "The prepared run manifest is missing. Finish downloads and run "
            "tools/appeears/prepare_targeted_extraction_inputs.R first."
        )
    sites = read_csv(manifest_path)
    if not sites or len({row["watershed_key"] for row in sites}) != len(sites):
        raise SystemExit("The prepared run must contain unique watershed keys.")
    for pattern in args.exclude_task_pattern:
        matches = [row["task_name"] for row in sites if re.search(pattern, row["task_name"])]
        if matches:
            raise SystemExit(
                f"Task exclusion pattern {pattern!r} matched: {matches}"
            )
    if args.watershed_key:
        requested = set(args.watershed_key)
        sites = [row for row in sites if row["watershed_key"] in requested]
        found = {row["watershed_key"] for row in sites}
        missing = sorted(requested - found)
        if missing:
            raise SystemExit(f"Prepared watershed key(s) not found: {missing}")

    rscript = shutil.which("Rscript")
    if not rscript:
        raise SystemExit("Rscript is not available on this computer.")
    workers = max(1, min(args.workers, len(sites)))
    run_tag = f"{args.output_date}_{args.run_label}"
    ledger = TimingLedger(run_root)
    if args.reconcile_only:
        completed = reconcile_completed_timing(sites, ledger, run_tag)
        print(f"Reconciled complete extraction timing for {completed} watershed(s).")
        return
    failures: list[str] = []

    print(
        f"Running isolated extractions for {len(sites)} accepted watersheds "
        f"with {workers} workers."
    )
    with ThreadPoolExecutor(max_workers=workers) as executor:
        futures = {
            executor.submit(
                run_site,
                site,
                ledger,
                repo_root,
                rscript,
                args,
                run_tag,
            ): site
            for site in sites
        }
        for future in as_completed(futures):
            watershed_key, ok, message = future.result()
            print(f"{watershed_key}: {'complete' if ok else 'failed'}")
            if not ok:
                failures.append(f"{watershed_key}: {message}")

    if failures:
        raise SystemExit("\n".join(failures))
    print(f"All {len(sites)} accepted watershed extractions are ready for QA.")


if __name__ == "__main__":
    main()
