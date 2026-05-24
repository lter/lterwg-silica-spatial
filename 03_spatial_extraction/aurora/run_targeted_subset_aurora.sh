
# Starts the normal targeted subset workflow on Aurora.
# Use this when you want one direct run and do not need `sbatch`.

set -euo pipefail

repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"

data_root="${SILICA_DATA_ROOT:-/home/shares/lter-si/si-watershed-extract}"
hydrosheds_raw_dir="${SILICA_HYDROSHEDS_RAW_DIR:-${data_root}/hydrosheds-raw}"
subset_file="${1:-${SILICA_SITE_SUBSET_FILE:-/home/shares/lter-si/si-watershed-extract/review/re03_spatial_extraction/targeted_rerun_subset_hydrosheds_fallback_20260510.csv}}"
output_date="${SILICA_OUTPUT_DATE:-$(date +%Y%m%d)}"
run_label="${SILICA_RUN_LABEL:-$(basename "${subset_file%.*}")}"
run_label="$(printf '%s' "$run_label" | tr '[:upper:]' '[:lower:]' | sed 's/[^a-z0-9]/-/g; s/-\\{2,\\}/-/g; s/^-//; s/-$//')"
run_tag="${output_date}${run_label:+_${run_label}}"
run_root="${SILICA_RUN_ROOT:-${data_root}/run-outputs/${run_tag}}"
site_coord_dir="${SILICA_SITE_COORD_DIR:-${run_root}/site-coordinates}"
extracted_dir="${SILICA_EXTRACTED_DIR:-${run_root}/extracted-data}"
shared_site_coord_dir="${data_root}/site-coordinates"
hydro_ids_dir="${data_root}/hydrosheds-basin-ids"
required_hydrosheds=(
  "hybas_af_lev00_v1c.shp"
  "hybas_ar_lev00_v1c.shp"
  "hybas_as_lev00_v1c.shp"
  "hybas_au_lev00_v1c.shp"
  "hybas_eu_lev00_v1c.shp"
  "hybas_gr_lev00_v1c.shp"
  "hybas_na_lev00_v1c.shp"
  "hybas_sa_lev00_v1c.shp"
  "hybas_si_lev00_v1c.shp"
)

die() {
  printf 'ERROR: %s\n' "$*" >&2
  exit 1
}

require_file() {
  local path="$1"
  [[ -f "$path" ]] || die "Missing required file: $path"
}

require_dir() {
  local path="$1"
  [[ -d "$path" ]] || die "Missing required directory: $path"
}

require_dir "$data_root"
require_dir "$shared_site_coord_dir"
require_dir "$hydrosheds_raw_dir"
require_dir "$hydro_ids_dir"
require_file "$subset_file"
require_file "$shared_site_coord_dir/silica-coords_RAW.xlsx"

for shp in "${required_hydrosheds[@]}"; do
  require_file "$hydrosheds_raw_dir/$shp"
done

mkdir -p "$site_coord_dir" "$extracted_dir"
cp "$shared_site_coord_dir/silica-coords_RAW.xlsx" "$site_coord_dir/silica-coords_RAW.xlsx"

export SILICA_DATA_ROOT="$data_root"
export SILICA_SITE_COORD_DIR="$site_coord_dir"
export SILICA_EXTRACTED_DIR="$extracted_dir"
export SILICA_HYDROSHEDS_RAW_DIR="$hydrosheds_raw_dir"
export SILICA_SITE_SUBSET_FILE="$subset_file"
export SILICA_SKIP_DRIVE_AUTH=TRUE
export SILICA_SKIP_DRIVE_UPLOAD=TRUE
export SILICA_COMBINE_LOCAL_ONLY=TRUE
export SILICA_CANONICALIZE_OBIDOS=TRUE
export SILICA_MERGE_SUBSET_OUTPUTS=TRUE
export SILICA_REBUILD_ARTISANAL=TRUE
export SILICA_REBUILD_HYDROSHEDS=TRUE
export SILICA_COMBINE_FULL=FALSE
export SILICA_OUTPUT_DATE="$output_date"
export SILICA_RUN_LABEL="$run_label"
export SILICA_ALLOW_OVERWRITE=FALSE
unset SILICA_TARGET_YEARS
unset SILICA_TARGET_YEAR_START
unset SILICA_TARGET_YEAR_END

printf 'repo_root=%s\n' "$repo_root"
printf 'subset_file=%s\n' "$subset_file"
printf 'data_root=%s\n' "$data_root"
printf 'run_root=%s\n' "$run_root"
printf 'run_label=%s\n' "$run_label"
printf 'site_coord_dir=%s\n' "$site_coord_dir"
printf 'extracted_dir=%s\n' "$extracted_dir"
printf 'hydrosheds_raw_dir=%s\n' "$hydrosheds_raw_dir"
printf 'output_date=%s\n' "$output_date"

cd "$repo_root"
exec Rscript tools/run_targeted_subset_workflow.R \
  --subset "$subset_file" \
  --combine-full false
