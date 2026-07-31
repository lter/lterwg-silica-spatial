# Run the targeted non-MODIS extraction on Aurora.

set -euo pipefail

repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
output_date="${SILICA_OUTPUT_DATE:-$(date +%Y%m%d)}"
data_root="${SILICA_DATA_ROOT:-/home/shares/lter-si/si-watershed-extract}"
input_dir="${SILICA_AURORA_INPUT_DIR:-${data_root}/run-inputs}"
shape_root="${SILICA_SHAPE_LIBRARY_ROOT:-${data_root}/silica-shapefiles}"
reference_file="${SILICA_BASE_FILE:-${input_dir}/AppEEARS_Site_Reference_Table.xlsx}"
default_subset="${input_dir}/non_modis_sites.csv"
subset_file="${1:-${SILICA_SITE_SUBSET_FILE:-${default_subset}}}"

run_label="${SILICA_RUN_LABEL:-$(basename "${subset_file%.*}")}"
run_label="$(printf '%s' "$run_label" | tr '[:upper:]' '[:lower:]' | sed 's/[^a-z0-9]/-/g; s/-\{2,\}/-/g; s/^-//; s/-$//')"
run_tag="${output_date}${run_label:+_${run_label}}"
run_root="${SILICA_RUN_ROOT:-${data_root}/run-outputs/${run_tag}}"
site_coord_dir="${SILICA_SITE_COORD_DIR:-${run_root}/site-coordinates}"
extracted_dir="${SILICA_EXTRACTED_DIR:-${run_root}/extracted-data}"

die() {
  printf 'ERROR: %s\n' "$*" >&2
  exit 1
}

require_file() {
  [[ -f "$1" ]] || die "Missing required file: $1"
}

require_dir() {
  [[ -d "$1" ]] || die "Missing required directory: $1"
}

require_dir "$data_root"
require_dir "$shape_root"
for release in 1 2 3; do
  require_dir "${shape_root}/data_release_${release}"
done
require_file "$reference_file"
require_file "$subset_file"

mkdir -p "$site_coord_dir" "$extracted_dir"

export SILICA_DATA_ROOT="$data_root"
export SILICA_SHAPE_LIBRARY_ROOT="$shape_root"
export SILICA_SITE_COORD_DIR="$site_coord_dir"
export SILICA_EXTRACTED_DIR="$extracted_dir"
export SILICA_BASE_FILE="$reference_file"
export SILICA_SITE_SUBSET_FILE="$subset_file"
export SILICA_USE_CANONICAL_RELEASE_LIBRARY=TRUE
export SILICA_REFERENCE_RELEASE=3
export SILICA_SKIP_DRIVE_AUTH=TRUE
export SILICA_SKIP_DRIVE_UPLOAD=TRUE
export SILICA_COMBINE_LOCAL_ONLY=TRUE
export SILICA_MERGE_SUBSET_OUTPUTS=TRUE
export SILICA_REBUILD_ARTISANAL=TRUE
export SILICA_REBUILD_HYDROSHEDS=FALSE
export SILICA_COMBINE_FULL=FALSE
export SILICA_RUN_STATIC_DRIVERS="${SILICA_RUN_STATIC_DRIVERS:-TRUE}"
export SILICA_RUN_DYNAMIC_DRIVERS="${SILICA_RUN_DYNAMIC_DRIVERS:-TRUE}"
export SILICA_DYNAMIC_DRIVER_NAMES="${SILICA_DYNAMIC_DRIVER_NAMES:-precip,airtemp}"
export SILICA_OUTPUT_DATE="$output_date"
export SILICA_RUN_LABEL="$run_label"
export SILICA_ALLOW_OVERWRITE=FALSE

drivers=()
case "$SILICA_RUN_STATIC_DRIVERS" in
  TRUE|true|True) drivers+=(soil lithology elevation permafrost) ;;
esac
case "$SILICA_RUN_DYNAMIC_DRIVERS" in
  TRUE|true|True)
    IFS=',' read -r -a dynamic_drivers <<< "$SILICA_DYNAMIC_DRIVER_NAMES"
    drivers+=("${dynamic_drivers[@]}")
    ;;
esac

printf 'subset_file=%s\n' "$subset_file"
printf 'reference_file=%s\n' "$reference_file"
printf 'shape_root=%s\n' "$shape_root"
printf 'run_root=%s\n' "$run_root"
printf 'drivers=%s\n' "$(IFS=','; printf '%s' "${drivers[*]}")"
if [[ -n "${SILICA_TARGET_YEARS:-}" ]]; then
  printf 'target_years=%s\n' "$SILICA_TARGET_YEARS"
fi

cd "$repo_root"
exec Rscript 03_spatial_extraction/wrappers/run-targeted-subset-workflow.R \
  --subset "$subset_file" \
  --combine-full false
