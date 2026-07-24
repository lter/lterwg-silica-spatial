"""Download completed AppEEARS task files into separate task folders.

Each download list belongs to one watershed. The script processes one
watershed at a time, downloads its files in parallel, and writes timing after
every task. Existing files are checked and skipped, so an interrupted run can
be resumed without downloading finished files again.
"""

from __future__ import annotations

import argparse
import csv
import json
import os
import shutil
import time
from concurrent.futures import ThreadPoolExecutor, as_completed
from datetime import datetime, timezone
from pathlib import Path
from urllib.error import HTTPError, URLError
from urllib.parse import unquote, urlsplit
from urllib.request import Request, urlopen


TIMING_FIELDS = (
    "watershed_key",
    "task_name",
    "started_at_utc",
    "completed_at_utc",
    "elapsed_hours",
    "expected_files",
    "downloaded_this_run",
    "already_present",
    "failed_files",
    "status",
)


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(
        description="Download completed AppEEARS files with retries and resume support."
    )
    parser.add_argument(
        "--run-root",
        action="append",
        type=Path,
        dest="run_roots",
        help="Run folder containing download-lists. Repeat for more than one run.",
    )
    parser.add_argument(
        "--token-file",
        type=Path,
        help="AppEEARS login JSON (or set APPEEARS_TOKEN_FILE).",
    )
    parser.add_argument(
        "--status-file",
        type=Path,
        help="Optional table used to map task names to watershed keys.",
    )
    parser.add_argument(
        "--task-name",
        action="append",
        default=[],
        help=(
            "Download only this task. Repeat for more than one task. "
            "When omitted, every list in the selected run folder is used."
        ),
    )
    parser.add_argument("--workers", type=int, default=8)
    parser.add_argument("--retries", type=int, default=6)
    args = parser.parse_args()
    if not args.run_roots:
        parser.error("Provide at least one --run-root.")
    if args.token_file is None:
        token = os.getenv("APPEEARS_TOKEN_FILE", "").strip()
        if not token:
            parser.error(
                "--token-file is required unless APPEEARS_TOKEN_FILE is set."
            )
        args.token_file = Path(token)
    return args


def utc_now() -> datetime:
    return datetime.now(timezone.utc)


def iso_utc(value: datetime) -> str:
    return value.astimezone(timezone.utc).isoformat().replace("+00:00", "Z")


def parse_time(value: str) -> datetime:
    return datetime.fromisoformat(value.replace("Z", "+00:00")).astimezone(
        timezone.utc
    )


def read_rows(path: Path) -> list[dict[str, str]]:
    if not path.exists():
        return []
    with path.open(newline="", encoding="utf-8-sig") as handle:
        return list(csv.DictReader(handle))


def write_timing(path: Path, rows: dict[str, dict[str, str]]) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    with path.open("w", newline="", encoding="utf-8") as handle:
        writer = csv.DictWriter(
            handle, fieldnames=TIMING_FIELDS, lineterminator="\n"
        )
        writer.writeheader()
        writer.writerows(sorted(rows.values(), key=lambda row: row["task_name"]))


def load_token(path: Path) -> str:
    payload = json.loads(path.read_text(encoding="utf-8"))
    token = payload.get("token")
    expiration = payload.get("expiration")
    if not token or not expiration:
        raise RuntimeError(f"The token file is incomplete: {path}")
    if parse_time(expiration) <= utc_now():
        raise RuntimeError(f"The AppEEARS token expired at {expiration}.")
    return token


def task_name_from_list(path: Path) -> str:
    suffix = "-download-list.txt"
    if not path.name.endswith(suffix):
        raise ValueError(f"Unexpected download-list name: {path.name}")
    return path.name[: -len(suffix)]


def read_download_list(path: Path) -> list[tuple[str, str]]:
    files = []
    for raw in path.read_text(encoding="utf-8").splitlines():
        url = raw.strip()
        if not url:
            continue
        parsed = urlsplit(url)
        if (
            parsed.scheme != "https"
            or parsed.netloc != "appeears.earthdatacloud.nasa.gov"
            or not parsed.path.startswith("/api/bundle/")
        ):
            raise ValueError(f"Unexpected AppEEARS download URL in {path.name}")
        name = Path(unquote(parsed.path)).name
        if not name.lower().endswith(".tif"):
            raise ValueError(f"The selected file is not a GeoTIFF: {name}")
        files.append((url, name))
    names = [name for _, name in files]
    if not files or len(names) != len(set(names)):
        raise ValueError(f"Expected unique GeoTIFF names in {path.name}")
    return files


def valid_existing_file(path: Path) -> bool:
    return path.is_file() and path.stat().st_size > 0


def download_one(
    url: str,
    destination: Path,
    token: str,
    retries: int,
) -> str:
    if valid_existing_file(destination):
        return "already_present"
    partial = destination.with_name(f"{destination.name}.part")
    for attempt in range(1, retries + 1):
        try:
            request = Request(url, headers={"Authorization": f"Bearer {token}"})
            with urlopen(request, timeout=120) as response, partial.open("wb") as handle:
                shutil.copyfileobj(response, handle)
                expected = response.headers.get("Content-Length")
            if expected is not None and partial.stat().st_size != int(expected):
                raise OSError("Downloaded size did not match Content-Length")
            if partial.stat().st_size == 0:
                raise OSError("AppEEARS returned an empty file")
            os.replace(partial, destination)
            return "downloaded"
        except (HTTPError, URLError, OSError, TimeoutError):
            if partial.exists():
                partial.unlink()
            if attempt == retries:
                return "failed"
            time.sleep(min(30, 2 ** (attempt - 1)))
    return "failed"


def process_task(
    list_path: Path,
    run_root: Path,
    token: str,
    workers: int,
    retries: int,
    watershed_by_task: dict[str, str],
    timing_rows: dict[str, dict[str, str]],
) -> None:
    task_name = task_name_from_list(list_path)
    files = read_download_list(list_path)
    destination_root = run_root / "downloads" / task_name
    destination_root.mkdir(parents=True, exist_ok=True)
    timing_path = run_root / "download_timing.csv"
    previous = timing_rows.get(task_name, {})
    started = previous.get("started_at_utc") or iso_utc(utc_now())
    timing_rows[task_name] = {
        "watershed_key": watershed_by_task.get(task_name, ""),
        "task_name": task_name,
        "started_at_utc": started,
        "completed_at_utc": "",
        "elapsed_hours": "",
        "expected_files": str(len(files)),
        "downloaded_this_run": "0",
        "already_present": "0",
        "failed_files": "",
        "status": "in_progress",
    }
    write_timing(timing_path, timing_rows)
    print(f"Starting {task_name}: {len(files)} files", flush=True)

    counts = {"downloaded": 0, "already_present": 0, "failed": 0}
    with ThreadPoolExecutor(max_workers=workers) as executor:
        futures = {
            executor.submit(
                download_one,
                url,
                destination_root / name,
                token,
                retries,
            ): name
            for url, name in files
        }
        for number, future in enumerate(as_completed(futures), start=1):
            counts[future.result()] += 1
            if number % 250 == 0 or number == len(futures):
                print(
                    f"  {number}/{len(futures)} checked; "
                    f"{counts['downloaded']} downloaded, "
                    f"{counts['already_present']} already present, "
                    f"{counts['failed']} failed",
                    flush=True,
                )

    completed = iso_utc(utc_now())
    elapsed = (parse_time(completed) - parse_time(started)).total_seconds() / 3600
    timing_rows[task_name].update(
        {
            "completed_at_utc": completed if counts["failed"] == 0 else "",
            "elapsed_hours": f"{elapsed:.3f}" if counts["failed"] == 0 else "",
            "downloaded_this_run": str(counts["downloaded"]),
            "already_present": str(counts["already_present"]),
            "failed_files": str(counts["failed"]),
            "status": "complete" if counts["failed"] == 0 else "failed",
        }
    )
    write_timing(timing_path, timing_rows)
    print(
        f"Finished {task_name}: {timing_rows[task_name]['status']}",
        flush=True,
    )


def main() -> None:
    args = parse_args()
    if args.workers < 1 or args.retries < 1:
        raise ValueError("Workers and retries must both be positive integers.")
    token = load_token(args.token_file)
    run_roots = tuple(args.run_roots)
    status_rows = read_rows(args.status_file) if args.status_file else []
    watershed_by_task = {
        row["task_name"]: row["watershed_key"]
        for row in status_rows
        if row.get("task_name")
    }
    for run_root in run_roots:
        list_dir = run_root / "download-lists"
        list_paths = sorted(list_dir.glob("*-download-list.txt"))
        if args.task_name:
            requested = set(args.task_name)
            list_paths = [
                path for path in list_paths if task_name_from_list(path) in requested
            ]
            found = {task_name_from_list(path) for path in list_paths}
            missing = sorted(requested - found)
            if missing:
                raise FileNotFoundError(
                    f"No download list found for requested task(s): {missing}"
                )
        if not list_paths:
            raise FileNotFoundError(f"No download lists found in {list_dir}")
        timing_path = run_root / "download_timing.csv"
        timing_rows = {
            row["task_name"]: row for row in read_rows(timing_path)
        }
        for list_path in list_paths:
            if timing_rows.get(task_name_from_list(list_path), {}).get("status") == "complete":
                continue
            process_task(
                list_path,
                run_root,
                token,
                args.workers,
                args.retries,
                watershed_by_task,
                timing_rows,
            )


if __name__ == "__main__":
    main()
