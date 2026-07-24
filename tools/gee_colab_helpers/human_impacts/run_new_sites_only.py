"""Run human-impact exports only for sites missing from the baseline asset.

The command starts with a dry run. It lists the new sites for review and does
not submit anything until ``--submit`` and the matching approved site list are
provided. This keeps a larger watershed asset from rerunning sites that the
earlier batch already covered.
"""

from __future__ import annotations

import argparse
import csv
import json
import os
import re
import sys
from datetime import datetime, timezone
from pathlib import Path
from typing import Iterable

try:
    import ee
except ImportError:  # Allow --help before Earth Engine is installed.
    ee = None


HELPER_ROOT = Path(__file__).resolve().parents[1]
if str(HELPER_ROOT) not in sys.path:
    sys.path.insert(0, str(HELPER_ROOT))
from gee_quota_preflight import consume_preflight_receipt  # noqa: E402


DEFAULT_PROJECT = os.getenv("SILICA_GEE_PROJECT", "silica-synthesis")
DEFAULT_BASELINE_ASSET = os.getenv("SILICA_GEE_BASELINE_ASSET", "")
DEFAULT_WORKFLOW_ROOT = os.getenv("SILICA_HUMAN_IMPACT_WORKFLOW_ROOT", "")
SITE_ID_PROPERTY = "site_id"
STATIC_DATASETS = ("dams", "fertilizer", "wastewater")
LABEL_PATTERN = re.compile(r"^[a-z0-9][a-z0-9_]*$")


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(
        description=(
            "Find sites that are not in a previously covered watershed asset. "
            "Nothing is "
            "submitted unless --submit and the matching approved site list are "
            "both provided."
        )
    )
    parser.add_argument(
        "--candidate-asset",
        required=True,
        help="Earth Engine watershed asset containing the latest accepted sites.",
    )
    parser.add_argument(
        "--expected-new-count",
        required=True,
        type=int,
        help="Number of new site IDs you expect to find.",
    )
    parser.add_argument(
        "--run-label",
        required=True,
        help="Lowercase task label, for example reviewed_new_sites.",
    )
    parser.add_argument(
        "--baseline-asset",
        default=DEFAULT_BASELINE_ASSET,
        help=(
            "Previously processed watershed asset. Alternatively set "
            "SILICA_GEE_BASELINE_ASSET."
        ),
    )
    parser.add_argument("--project", default=DEFAULT_PROJECT)
    parser.add_argument(
        "--workflow-root",
        type=Path,
        default=Path(DEFAULT_WORKFLOW_ROOT) if DEFAULT_WORKFLOW_ROOT else None,
        help=(
            "Checkout containing src/gee_spatial and the human-impact config. "
            "Alternatively set SILICA_HUMAN_IMPACT_WORKFLOW_ROOT."
        ),
    )
    parser.add_argument(
        "--output-folder",
        help=(
            "Destination Earth Engine asset folder. The default is derived "
            "from the run label."
        ),
    )
    parser.add_argument(
        "--manifest",
        type=Path,
        help="Path for the JSON review summary written before submission.",
    )
    parser.add_argument(
        "--approved-site-ids",
        type=Path,
        help=(
            "CSV with a site_id column, or a text file with one ID per line. "
            "Required with --submit."
        ),
    )
    parser.add_argument(
        "--submit",
        action="store_true",
        help="Launch the missing exports after the site list passes every check.",
    )
    parser.add_argument(
        "--max-new-tasks",
        type=int,
        default=1,
        help="Maximum new tasks to submit (default: one smoke-test task).",
    )
    parser.add_argument(
        "--preflight-receipt",
        type=Path,
        help="Fresh, approved receipt from gee_quota_preflight.py.",
    )
    args = parser.parse_args()
    if not args.baseline_asset:
        parser.error(
            "--baseline-asset is required unless SILICA_GEE_BASELINE_ASSET is set."
        )
    if args.workflow_root is None:
        parser.error(
            "--workflow-root is required unless "
            "SILICA_HUMAN_IMPACT_WORKFLOW_ROOT is set."
        )
    if args.max_new_tasks < 1:
        parser.error("--max-new-tasks must be positive.")
    return args


def read_approved_site_ids(path: Path) -> list[str]:
    """Read the human-reviewed new-site list without changing ID spelling."""
    if not path.exists():
        raise FileNotFoundError(f"Approved site-ID file does not exist: {path}")
    if path.suffix.lower() == ".csv":
        with path.open(newline="", encoding="utf-8-sig") as handle:
            reader = csv.DictReader(handle)
            if not reader.fieldnames or SITE_ID_PROPERTY not in reader.fieldnames:
                raise ValueError(
                    f"Approved CSV must contain a {SITE_ID_PROPERTY!r} column."
                )
            values = [row[SITE_ID_PROPERTY].strip() for row in reader]
    else:
        values = [
            line.strip()
            for line in path.read_text(encoding="utf-8").splitlines()
            if line.strip()
        ]
    if not values or any(not value for value in values):
        raise ValueError("Approved site-ID file is empty or contains blank IDs.")
    if len(values) != len(set(values)):
        raise ValueError("Approved site-ID file contains duplicate IDs.")
    return sorted(values)


def feature_collection_ids(
    collection: ee.FeatureCollection,
    label: str,
) -> tuple[int, list[str]]:
    """Return validated site IDs for one watershed collection."""
    row_count = int(collection.size().getInfo())
    raw_ids = collection.aggregate_array(SITE_ID_PROPERTY).getInfo()
    site_ids = [str(value).strip() if value is not None else "" for value in raw_ids]
    if len(site_ids) != row_count:
        raise RuntimeError(
            f"{label} returned {row_count} rows but {len(site_ids)} site IDs."
        )
    if any(not site_id for site_id in site_ids):
        raise RuntimeError(f"{label} contains a blank {SITE_ID_PROPERTY} value.")
    if len(site_ids) != len(set(site_ids)):
        raise RuntimeError(f"{label} contains duplicate {SITE_ID_PROPERTY} values.")
    return row_count, sorted(site_ids)


def human_run_plan(config: dict, available_dataset_years) -> list[dict]:
    """Return the 28 non-GHSL outputs used by the accepted replacement plan."""
    plan = [
        {"dataset": dataset, "year": None}
        for dataset in STATIC_DATASETS
    ]
    plan.extend(
        {"dataset": "population", "year": year}
        for year in range(2000, 2025)
    )
    if len(plan) != 28:
        raise RuntimeError(
            f"Expected 28 non-GHSL human-impact outputs; found {len(plan)}."
        )
    return plan


def asset_or_none(asset_id: str):
    try:
        return ee.data.getAsset(asset_id)
    except ee.EEException as exc:
        message = str(exc).lower()
        if "not found" in message or "does not exist" in message or "404" in message:
            return None
        raise


def ensure_folder(asset_id: str) -> None:
    if asset_or_none(asset_id) is None:
        ee.data.createAsset({"type": "FOLDER"}, asset_id)


def active_operations_by_description() -> dict[str, dict]:
    return {
        operation.get("metadata", {}).get("description", ""): operation
        for operation in ee.data.listOperations()
        if not operation.get("done")
        and operation.get("metadata", {}).get("state")
        not in {"CANCELLING", "CANCELLED"}
    }


def output_name(dataset: str, year: int | None, new_count: int, run_label: str) -> str:
    period = str(year) if year is not None else "static"
    return (
        f"human_impacts_{dataset}_{period}_{new_count}newsites_"
        f"{run_label}_watershed_extract"
    )


def write_manifest(path: Path, payload: dict) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_text(json.dumps(payload, indent=2, sort_keys=True) + "\n", encoding="utf-8")


def write_site_id_manifest(path: Path, site_ids: Iterable[str]) -> None:
    """Write the site list that a reviewer will approve before submission."""
    path.parent.mkdir(parents=True, exist_ok=True)
    with path.open("w", newline="", encoding="utf-8") as handle:
        writer = csv.writer(handle, lineterminator="\n")
        writer.writerow([SITE_ID_PROPERTY])
        writer.writerows([site_id] for site_id in site_ids)


def assert_exact_approved_delta(
    approved_path: Path | None,
    computed_new_ids: list[str],
) -> None:
    if approved_path is None:
        raise ValueError("--submit requires --approved-site-ids.")
    approved_ids = read_approved_site_ids(approved_path)
    if approved_ids != computed_new_ids:
        missing_from_approval = sorted(set(computed_new_ids) - set(approved_ids))
        unexpected_in_approval = sorted(set(approved_ids) - set(computed_new_ids))
        raise RuntimeError(
            "The approved site IDs do not match the new sites found in the asset. "
            f"Missing from approval: {missing_from_approval}; "
            f"unexpected in approval: {unexpected_in_approval}."
        )


def restore_centroid_geometry(
    collection: ee.FeatureCollection,
    watersheds: ee.FeatureCollection,
) -> ee.FeatureCollection:
    """Attach one small geometry so asset exports retain a spatial reference."""
    def restore(feature):
        watershed = ee.Feature(
            watersheds.filter(
                ee.Filter.eq(SITE_ID_PROPERTY, feature.get(SITE_ID_PROPERTY))
            ).first()
        )
        return ee.Feature(feature).setGeometry(
            watershed.geometry().centroid(maxError=100)
        )

    return collection.map(restore)


def launch(
    plan: Iterable[dict],
    watersheds: ee.FeatureCollection,
    config: dict,
    output_folder: str,
    new_count: int,
    run_label: str,
    extract_human_impact_dataset,
    human_impact_export_columns,
    allowed_descriptions: set[str],
) -> list[dict]:
    """Launch missing tasks and leave completed or active tasks alone."""
    ensure_folder(output_folder)
    active = active_operations_by_description()
    task_rows = []
    for item in plan:
        dataset = item["dataset"]
        year = item["year"]
        name = output_name(dataset, year, new_count, run_label)
        asset_id = f"{output_folder}/{name}"
        if asset_or_none(asset_id) is not None:
            state = "SKIP_COMPLETED"
            task_id = ""
        elif name in active:
            operation = active[name]
            state = operation.get("metadata", {}).get("state", "PENDING")
            task_id = operation.get("name", "").rsplit("/", 1)[-1]
        elif name not in allowed_descriptions:
            state = "DEFERRED_BY_TASK_CAP"
            task_id = ""
        else:
            extracted = extract_human_impact_dataset(
                dataset,
                config,
                watersheds,
                year=year,
                fertilizer_crops=None,
            )
            extracted = restore_centroid_geometry(extracted, watersheds)
            selectors = human_impact_export_columns(config, dataset)
            task = ee.batch.Export.table.toAsset(
                collection=extracted.select(selectors),
                description=name,
                assetId=asset_id,
            )
            task.start()
            state = task.status().get("state", "READY")
            task_id = task.id
        task_rows.append(
            {
                "dataset": dataset,
                "year": year,
                "description": name,
                "asset_id": asset_id,
                "task_id": task_id,
                "state": state,
            }
        )
        state_text = {
            "SKIP_COMPLETED": "Already complete",
            "READY": "Submitted and waiting",
            "RUNNING": "Already running",
            "PENDING": "Already queued",
        }.get(state, state.replace("_", " ").title())
        print(f"{state_text}: {name}", flush=True)
    return task_rows


def main() -> None:
    args = parse_args()
    if ee is None:
        raise SystemExit(
            "Install and authenticate the Earth Engine Python API before "
            "running exports: python3 -m pip install earthengine-api"
        )
    if args.expected_new_count <= 0:
        raise ValueError("--expected-new-count must be greater than zero.")
    if not LABEL_PATTERN.fullmatch(args.run_label):
        raise ValueError(
            "--run-label must contain only lowercase letters, numbers, and underscores."
        )

    workflow_root = args.workflow_root.resolve()
    if not (workflow_root / "src/gee_spatial/human_impacts.py").exists():
        raise FileNotFoundError(
            f"Human-impact workflow source was not found under {workflow_root}."
        )
    sys.path.insert(0, str(workflow_root))
    from src.gee_spatial.human_impacts import (  # noqa: PLC0415
        available_dataset_years,
        extract_human_impact_dataset,
        human_impact_export_columns,
        load_human_impact_config,
    )

    ee.Initialize(project=args.project)
    baseline = ee.FeatureCollection(args.baseline_asset)
    candidate = ee.FeatureCollection(args.candidate_asset)
    baseline_rows, baseline_ids = feature_collection_ids(baseline, "Baseline asset")
    candidate_rows, candidate_ids = feature_collection_ids(candidate, "Candidate asset")

    baseline_set = set(baseline_ids)
    candidate_set = set(candidate_ids)
    new_ids = sorted(candidate_set - baseline_set)
    candidate_overlap_ids = sorted(candidate_set & baseline_set)
    if len(new_ids) != args.expected_new_count:
        raise RuntimeError(
            f"Expected {args.expected_new_count} new sites but computed {len(new_ids)}. "
            "Nothing was submitted."
        )

    new_watersheds = candidate.filter(
        ee.Filter.inList(SITE_ID_PROPERTY, new_ids)
    )
    selected_rows, selected_ids = feature_collection_ids(
        new_watersheds,
        "New-site selection",
    )
    repeated_selected_ids = sorted(set(selected_ids) & baseline_set)
    if repeated_selected_ids:
        raise RuntimeError(
            "New-site selection overlaps the completed baseline: "
            f"{repeated_selected_ids}"
        )
    if selected_ids != new_ids or selected_rows != len(new_ids):
        raise RuntimeError("The selected watersheds do not match the new site list.")

    output_folder = args.output_folder or (
        f"projects/{args.project}/assets/human_impacts_new_sites_{args.run_label}"
    )
    manifest_path = args.manifest or Path(
        "generated_outputs/gee_task_timing"
    ) / f"human_impacts_new_sites_preflight_{args.run_label}.json"
    site_id_manifest_path = manifest_path.with_name(
        f"{manifest_path.stem}_site_ids.csv"
    )
    manifest = {
        "checked_at_utc": datetime.now(timezone.utc).isoformat(),
        "project": args.project,
        "baseline_asset": args.baseline_asset,
        "baseline_rows": baseline_rows,
        "candidate_asset": args.candidate_asset,
        "candidate_rows": candidate_rows,
        "candidate_ids_already_in_baseline": len(candidate_overlap_ids),
        "expected_new_count": args.expected_new_count,
        "computed_new_count": len(new_ids),
        "new_site_ids": new_ids,
        "new_site_id_manifest": str(site_id_manifest_path),
        "selected_baseline_overlap_count": len(repeated_selected_ids),
        "output_folder": output_folder,
        "run_label": args.run_label,
        "submit_requested": args.submit,
    }
    write_manifest(manifest_path, manifest)
    write_site_id_manifest(site_id_manifest_path, new_ids)
    print(json.dumps(manifest, indent=2, sort_keys=True), flush=True)
    print(f"Review summary saved to: {manifest_path}", flush=True)
    print(f"New-site list saved to: {site_id_manifest_path}", flush=True)

    if not args.submit:
        print("Dry run complete. No Earth Engine tasks were submitted.", flush=True)
        return

    assert_exact_approved_delta(args.approved_site_ids, new_ids)
    config = load_human_impact_config(
        workflow_root / "config/human-impact-products.yml"
    )
    plan = human_run_plan(config, available_dataset_years)
    active = active_operations_by_description()
    missing_plan = []
    for item in plan:
        name = output_name(
            item["dataset"],
            item["year"],
            len(new_ids),
            args.run_label,
        )
        asset_id = f"{output_folder}/{name}"
        if asset_or_none(asset_id) is None and name not in active:
            missing_plan.append({**item, "description": name})
    launch_plan = missing_plan[: args.max_new_tasks]
    max_task_area_km2 = float(
        new_watersheds.aggregate_sum("polygon_area_km2").getInfo() or 0
    )
    print(
        f"Missing non-GHSL tasks: {len(missing_plan)}; "
        f"new-task cap: {args.max_new_tasks}; "
        f"tasks eligible now: {len(launch_plan)}.",
        flush=True,
    )
    if launch_plan:
        print(
            "Required preflight:\n"
            "  python tools/gee_colab_helpers/gee_quota_preflight.py "
            "--workflow human_impacts_new_sites "
            "--description-prefix human_impacts_ "
            f"--proposed-task-count {len(launch_plan)} "
            f"--site-count {len(new_ids)} "
            f"--max-task-area-km2 {max_task_area_km2:.6f} "
            "--scale-m 100 "
            "--receipt generated_outputs/gee_preflight/"
            f"human_impacts_{args.run_label}.json",
            flush=True,
        )
    if not launch_plan:
        print("No missing tasks remain; nothing submitted.", flush=True)
        return
    if args.preflight_receipt is None:
        raise RuntimeError(
            "--submit requires --preflight-receipt. Run the printed GEE quota "
            "preflight command immediately before this launcher."
        )
    consume_preflight_receipt(
        args.preflight_receipt,
        project=args.project,
        workflow="human_impacts_new_sites",
        description_prefix="human_impacts_",
        proposed_task_count=len(launch_plan),
        site_count=len(new_ids),
        max_task_area_km2=max_task_area_km2,
        scale_m=100.0,
        time_slices_per_task=1,
    )
    allowed_descriptions = {
        str(item["description"]) for item in launch_plan
    }
    task_rows = launch(
        plan,
        new_watersheds,
        config,
        output_folder,
        len(new_ids),
        args.run_label,
        extract_human_impact_dataset,
        human_impact_export_columns,
        allowed_descriptions,
    )
    task_log = manifest_path.with_name(
        f"human_impacts_new_sites_tasks_{args.run_label}.json"
    )
    write_manifest(task_log, {"manifest": manifest, "tasks": task_rows})
    print(f"Task list saved to: {task_log}", flush=True)


if __name__ == "__main__":
    main()
