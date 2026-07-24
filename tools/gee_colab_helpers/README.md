# Google Earth Engine

These tools prepare watershed payloads, submit Earth Engine jobs, and check the
exports. Generated files belong in `generated_outputs/` or shared storage.

## Safe submission

Launchers are read-only unless `--submit` is supplied. Production submissions
also require a fresh receipt from `gee_quota_preflight.py`.

Start with one task:

```text
--submit --max-new-tasks 1 --preflight-receipt PATH
```

Check the smoke test and its EECU cost before increasing the task count. Do not
bypass the quota gate or watchdog.

## Main entry points

| Data | Entry point |
|---|---|
| Annual ERA5-Land | `full_annual_all_sites/run_all_sites_annual_era5_land_2000_2025.ipynb` |

Build payloads with `build_gee_vector_payloads.R`. Use `coarse-1km` for
kilometre-scale products and `fine-30m` for 30 m land cover. Each launcher and
consolidator provides its own command-line options.

After downloading exports, run the scripts in `post_run_qa/`.
