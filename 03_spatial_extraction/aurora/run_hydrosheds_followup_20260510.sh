
# Starts the separate 2026-05-10 HydroSHEDS follow-up run on Aurora.
# It writes to its own output folder so it does not overwrite the main extract files.

set -euo pipefail

repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
subset_file="${1:-${SILICA_SITE_SUBSET_FILE:-/home/shares/lter-si/si-watershed-extract/review/re03_spatial_extraction/targeted_rerun_subset_hydrosheds_followup_20260510.csv}}"
log_file="${2:-$repo_root/logs/hydrosheds_followup_20260510.log}"
data_root="${SILICA_DATA_ROOT:-/home/shares/lter-si/si-watershed-extract}"
output_date="${SILICA_OUTPUT_DATE:-$(date +%Y%m%d)}"
run_label="${SILICA_RUN_LABEL:-hydrosheds-followup}"
run_label="$(printf '%s' "$run_label" | tr '[:upper:]' '[:lower:]' | sed 's/[^a-z0-9]/-/g; s/-\\{2,\\}/-/g; s/^-//; s/-$//')"
run_tag="${output_date}${run_label:+_${run_label}}"
run_root="${SILICA_RUN_ROOT:-${data_root}/run-outputs/${run_tag}}"
site_coord_dir="${SILICA_SITE_COORD_DIR:-${run_root}/site-coordinates}"
out_dir="${SILICA_EXTRACTED_DIR:-${run_root}/extracted-data}"
shared_site_coord_dir="${data_root}/site-coordinates"

if [[ ! -f "$subset_file" ]]; then
  printf 'ERROR: Missing subset file: %s\n' "$subset_file" >&2
  exit 1
fi

if [[ ! -f "$shared_site_coord_dir/silica-coords_RAW.xlsx" ]]; then
  printf 'ERROR: Missing shared workbook: %s\n' "$shared_site_coord_dir/silica-coords_RAW.xlsx" >&2
  exit 1
fi

mkdir -p "$(dirname "$log_file")"
mkdir -p "$site_coord_dir" "$out_dir"
cp "$shared_site_coord_dir/silica-coords_RAW.xlsx" "$site_coord_dir/silica-coords_RAW.xlsx"

# Seed the run-local site-coordinates folder with the shared artisanal watershed
# shapefile when we are not rebuilding artisanal polygons in this run. Several
# downstream scripts expect an artisanal layer to exist alongside the run-local
# HydroSHEDS outputs.
if [[ "${SILICA_REBUILD_ARTISANAL:-FALSE}" != "TRUE" ]]; then
  for ext in shp shx dbf prj cpg; do
    src="$shared_site_coord_dir/silica-watersheds_artisanal.$ext"
    if [[ -f "$src" ]]; then
      cp "$src" "$site_coord_dir/"
    fi
  done
fi

cd "$repo_root"

export SILICA_DATA_ROOT="$data_root"
export SILICA_SITE_COORD_DIR="$site_coord_dir"
export SILICA_SITE_SUBSET_FILE="$subset_file"
export SILICA_EXTRACTED_DIR="$out_dir"
export SILICA_SKIP_DRIVE_AUTH=TRUE
export SILICA_SKIP_DRIVE_UPLOAD=TRUE
export SILICA_MERGE_SUBSET_OUTPUTS=TRUE
export SILICA_REBUILD_ARTISANAL=FALSE
export SILICA_REBUILD_HYDROSHEDS=TRUE
export SILICA_COMBINE_FULL=FALSE
export SILICA_OUTPUT_DATE="$output_date"
export SILICA_RUN_LABEL="$run_label"
export SILICA_ALLOW_OVERWRITE=FALSE

nohup Rscript tools/run_targeted_subset_workflow.R \
  --subset "$subset_file" \
  --combine-full false \
  > "$log_file" 2>&1 &

echo "PID=$!"
echo "LOG=$log_file"
echo "SITE_COORD_DIR=$site_coord_dir"
echo "OUT_DIR=$out_dir"
