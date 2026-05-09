# Local Cleanup Plan: targeted-subset-rerun

Date: 2026-05-08
Repo: `lterwg-silica-spatial`
Branch: `targeted-subset-rerun`

## Aurora Preservation Checkpoint

Aurora preservation is complete and should stay archived, not actively edited during this local cleanup step.

- Branch: `bush/aurora-snapshot-20260508`
- Source snapshot: `aurora_source_snapshot_20260508.tar.gz`
- Minimal workflow snapshot: `aurora_minimal_workflow_files_20260508.tar.gz`

## What Belongs In This Repo

Keep and reconcile only the workflow pieces that are already part of local `lterwg-silica-spatial`:

- `site-subset-helpers.R`
- `tools/build_targeted_rerun_manifest.R`
- `tools/combine_from_site_ref_local.R`
- `tools/run_targeted_subset_workflow.R`
- existing subset-aware changes in `wrangle-watersheds.R`, `wrangle-hydrosheds.R`, and the extract scripts

Do not pull in Aurora-only helpers during this cleanup pass:

- additional APPEEARS helper scripts
- `tools/check_all_sites_extent_and_watershed.R`
- unrelated `data-workflow/workflows/spatial` archaeology

## Confirmed Findings

- The best combined 2026 file was `2026-03-23`, not `2026-04-10`.
- The `2026-04-10` combined file was mostly shell/empty.
- Canada1-6 and MD7-18 were omitted from the HydroSHEDS rerun because the manifest builder filtered out HydroSHEDS provenance rows when `Shapefile_Name` contained placeholder values.
- The local patch in [tools/build_targeted_rerun_manifest.R](/Users/sidneybush/Documents/GitHub/lterwg-silica-spatial/tools/build_targeted_rerun_manifest.R:92) fixes that by allowing rows with HydroSHEDS provenance through even when `Shapefile_Name` is populated.
- The refreshed site reference CSV exported from `Site_Reference_Table_20260228.xlsx` was needed because the live WRTDS CSV was stale and missing `MD9` and `MD16`.
- Canada and MD sites now appear in `qa/targeted_rerun_subset_20260508.csv`.
- The targeted rerun then failed later in [wrangle-hydrosheds.R](/Users/sidneybush/Documents/GitHub/lterwg-silica-spatial/wrangle-hydrosheds.R:313) to [wrangle-hydrosheds.R](/Users/sidneybush/Documents/GitHub/lterwg-silica-spatial/wrangle-hydrosheds.R:366) when cached upstream-ID CSVs and newly generated ID frames were row-bound.
- The odd evapo multi-site label was a log-formatting issue, not evidence of a bad extraction by itself.

## Local Diff Assessment

Current code changes to keep in scope:

- [tools/build_targeted_rerun_manifest.R](/Users/sidneybush/Documents/GitHub/lterwg-silica-spatial/tools/build_targeted_rerun_manifest.R:92)
- [wrangle-hydrosheds.R](/Users/sidneybush/Documents/GitHub/lterwg-silica-spatial/wrangle-hydrosheds.R:273)

Assessment:

- The manifest patch belongs in the cleaned repo.
- The HydroSHEDS patch should stay narrow and only normalize cached versus freshly generated upstream ID tables.
- These can be committed separately if we want the manifest fix isolated first.

## Unresolved Runtime Bug

The blocker sits downstream from the manifest logic.

In [wrangle-hydrosheds.R](/Users/sidneybush/Documents/GitHub/lterwg-silica-spatial/wrangle-hydrosheds.R:320), cached upstream polygon CSVs are read with `read.csv()`, while new IDs are built in memory at [wrangle-hydrosheds.R](/Users/sidneybush/Documents/GitHub/lterwg-silica-spatial/wrangle-hydrosheds.R:356). Those mixed objects are combined with `purrr::list_rbind()` at [wrangle-hydrosheds.R](/Users/sidneybush/Documents/GitHub/lterwg-silica-spatial/wrangle-hydrosheds.R:375). The fix here is to normalize both inputs to the same two required numeric columns before binding.

Aurora note:

- Keep this compatible with the active Aurora run context.
- Avoid turning this into a larger refactor or changing the wider workflow contract.

## Clean Commit Plan

1. Commit the manifest-builder fix by itself.
2. Commit the HydroSHEDS type-normalization fix in `wrangle-hydrosheds.R`.
3. Re-run the targeted subset workflow with the refreshed reference CSV and verify Canada/MD remain present through hydrosheds output assembly.
4. If the rerun succeeds, commit any minimal follow-on fixes required for subset output merging.
5. Leave Aurora-only tooling archived unless a specific file is later proven necessary in this repo.

## Immediate Next Work

- Preserve the current manifest patch in git.
- Verify the `wrangle-hydrosheds.R` normalization fix against the active Aurora run behavior.
- Re-run only the targeted subset workflow path needed to confirm the hydrosheds bind step is stable.
