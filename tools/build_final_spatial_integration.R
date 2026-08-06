# Build the provenance-locked final spatial table

source(file.path("tools", "workflow_paths.R"))
source(file.path("tools", "final_spatial_integration_functions.R"))

args <- commandArgs(trailingOnly = TRUE)
manifest_path <- cli_value(
  args,
  "--manifest",
  file.path(
    "04_combine_qaqc", "config", "final_spatial_sources_20260805.tsv"
  )
)
output_root <- cli_value(
  args,
  "--outdir",
  file.path("generated_outputs", "final-integration", "final-spatial-20260805")
)

build_final_spatial_integration(
  manifest_path = manifest_path,
  output_root = output_root,
  allow_incomplete = cli_boolean(args, "--allow-incomplete", FALSE)
)
