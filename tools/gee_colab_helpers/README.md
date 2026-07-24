# Google Earth Engine

These tools prepare watershed inputs, submit Earth Engine jobs, and check the
exports. Generated files belong in `generated_outputs/` or shared storage.

## Safe submission

The scripts show what they would do unless `--submit` is supplied. A real
submission also requires a current safety-check file from
`gee_quota_preflight.py`.

Start with one task:

```text
--submit --max-new-tasks 1 --preflight-receipt PATH
```

Check the test task and its EECU cost before increasing the task count. Do not
bypass the quota or overnight safety checks.

## Main entry points

| Data | Entry point |
|---|---|
| Annual ERA5-Land | `full_annual_all_sites/run_all_sites_annual_era5_land_2000_2025.ipynb` |

Build watershed input files with `build_gee_vector_payloads.R`. Use `coarse-1km`
for kilometre-scale products and `fine-30m` for 30 m land cover. Each script
lists its own command-line options.

After downloading exports, run the scripts in `post_run_qa/`.
