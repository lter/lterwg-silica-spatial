
# Optional `sbatch` wrapper for `03_spatial_extraction/aurora/run_targeted_subset_aurora.sh`.

#SBATCH --job-name=silica-targeted
#SBATCH --output=logs/silica-targeted-%j.out
#SBATCH --error=logs/silica-targeted-%j.err
#SBATCH --time=24:00:00
#SBATCH --cpus-per-task=4
#SBATCH --mem=32G

set -euo pipefail

repo_root="${SLURM_SUBMIT_DIR:-$(pwd)}"
cd "$repo_root"
mkdir -p logs

if command -v module >/dev/null 2>&1; then
  module purge || true
fi

if [[ -n "${AURORA_R_MODULE:-}" ]] && command -v module >/dev/null 2>&1; then
  module load "$AURORA_R_MODULE"
fi

bash 03_spatial_extraction/aurora/run_targeted_subset_aurora.sh "${SILICA_SITE_SUBSET_FILE:-}"
