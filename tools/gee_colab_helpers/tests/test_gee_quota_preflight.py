"""Unit tests for the fail-closed Earth Engine quota gate."""

from __future__ import annotations

import json
import sys
import tempfile
import unittest
from datetime import datetime, timedelta, timezone
from pathlib import Path


HELPER_ROOT = Path(__file__).resolve().parents[1]
sys.path.insert(0, str(HELPER_ROOT))

from gee_quota_preflight import (  # noqa: E402
    build_receipt,
    consume_preflight_receipt,
    evaluate_preflight,
    validate_preflight_receipt,
)


def task_summary(
    *,
    active: int = 0,
    median: float | None = None,
    p90: float | None = None,
    maximum: float | None = None,
) -> dict:
    return {
        "active_task_count": active,
        "active_state_counts": {},
        "matching_task_count": 0,
        "matching_tasks_with_usage": 0,
        "historical_eecu_hours": {
            "median": median,
            "p90": p90,
            "max": maximum,
        },
        "highest_cost_matching_tasks": [],
    }


class DecisionTests(unittest.TestCase):
    def test_blocks_monthly_usage_at_safety_stop(self) -> None:
        result = evaluate_preflight(
            proposed_task_count=1,
            monthly_limit_hours=1_000,
            monitoring={
                "completed_eecu_hours": 701,
                "in_progress_eecu_hours": 0,
                "observed_eecu_hours": 701,
            },
            task_summary=task_summary(p90=1, maximum=1),
            max_task_area_km2=1_000,
            scale_m=1_000,
            time_slices_per_task=1,
        )
        self.assertFalse(result["approved"])
        self.assertTrue(
            any("safety stop" in value for value in result["blockers"])
        )

    def test_unknown_workflow_allows_only_one_smoke_task(self) -> None:
        result = evaluate_preflight(
            proposed_task_count=2,
            monthly_limit_hours=1_000,
            monitoring={
                "completed_eecu_hours": 0,
                "in_progress_eecu_hours": 0,
                "observed_eecu_hours": 0,
            },
            task_summary=task_summary(),
            max_task_area_km2=1_000,
            scale_m=1_000,
            time_slices_per_task=1,
        )
        self.assertFalse(result["approved"])
        self.assertTrue(
            any("exactly one" in value for value in result["blockers"])
        )

    def test_blocks_glc_sized_geometry(self) -> None:
        result = evaluate_preflight(
            proposed_task_count=1,
            monthly_limit_hours=1_000,
            monitoring={
                "completed_eecu_hours": 0,
                "in_progress_eecu_hours": 0,
                "observed_eecu_hours": 0,
            },
            task_summary=task_summary(),
            max_task_area_km2=3_813_807,
            scale_m=30,
            time_slices_per_task=1,
        )
        self.assertFalse(result["approved"])
        self.assertTrue(
            any("pixel-time" in value for value in result["blockers"])
        )

    def test_approves_small_measured_batch(self) -> None:
        result = evaluate_preflight(
            proposed_task_count=2,
            monthly_limit_hours=1_000,
            monitoring={
                "completed_eecu_hours": 100,
                "in_progress_eecu_hours": 0,
                "observed_eecu_hours": 100,
            },
            task_summary=task_summary(median=1, p90=2, maximum=3),
            max_task_area_km2=10_000,
            scale_m=1_000,
            time_slices_per_task=1,
        )
        self.assertTrue(result["approved"])


class ReceiptTests(unittest.TestCase):
    def make_receipt(self, now: datetime) -> dict:
        return build_receipt(
            project="silica-synthesis",
            workflow="unit_test",
            description_prefix="unit_",
            proposed_task_count=1,
            site_count=2,
            max_task_area_km2=100,
            scale_m=1_000,
            time_slices_per_task=1,
            monthly_limit_hours=1_000,
            monitoring={
                "completed_eecu_hours": 10,
                "in_progress_eecu_hours": 0,
                "observed_eecu_hours": 10,
            },
            task_summary=task_summary(),
            interval={
                "start_utc": "2026-07-01T07:00:00Z",
                "end_utc": now.isoformat(),
                "timezone": "America/Los_Angeles",
            },
            now=now,
        )

    def test_receipt_is_exact_and_one_use(self) -> None:
        now = datetime(2026, 7, 23, 12, tzinfo=timezone.utc)
        with tempfile.TemporaryDirectory() as directory:
            path = Path(directory) / "receipt.json"
            path.write_text(json.dumps(self.make_receipt(now)), encoding="utf-8")
            consume_preflight_receipt(
                path,
                project="silica-synthesis",
                workflow="unit_test",
                description_prefix="unit_",
                proposed_task_count=1,
                site_count=2,
                max_task_area_km2=100,
                scale_m=1_000,
                time_slices_per_task=1,
                now=now,
                start_watchdog=False,
            )
            with self.assertRaisesRegex(RuntimeError, "already consumed"):
                validate_preflight_receipt(
                    path,
                    project="silica-synthesis",
                    workflow="unit_test",
                    description_prefix="unit_",
                    proposed_task_count=1,
                )

    def test_expired_receipt_is_rejected(self) -> None:
        now = datetime(2026, 7, 23, 12, tzinfo=timezone.utc)
        with tempfile.TemporaryDirectory() as directory:
            path = Path(directory) / "receipt.json"
            path.write_text(json.dumps(self.make_receipt(now)), encoding="utf-8")
            with self.assertRaisesRegex(RuntimeError, "expired"):
                validate_preflight_receipt(
                    path,
                    project="silica-synthesis",
                    workflow="unit_test",
                    description_prefix="unit_",
                    proposed_task_count=1,
                    now=now + timedelta(hours=1),
                )

    def test_wrong_workload_dimensions_are_rejected(self) -> None:
        now = datetime(2026, 7, 23, 12, tzinfo=timezone.utc)
        with tempfile.TemporaryDirectory() as directory:
            path = Path(directory) / "receipt.json"
            path.write_text(json.dumps(self.make_receipt(now)), encoding="utf-8")
            with self.assertRaisesRegex(RuntimeError, "workload dimensions"):
                validate_preflight_receipt(
                    path,
                    project="silica-synthesis",
                    workflow="unit_test",
                    description_prefix="unit_",
                    proposed_task_count=1,
                    site_count=999,
                    now=now,
                )


if __name__ == "__main__":
    unittest.main()
