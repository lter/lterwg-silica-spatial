librarian::shelf(dplyr)

args <- commandArgs(trailingOnly = TRUE)
source(file.path("01_pre_aurora_run", "00_pre_aurora_paths.R"))

get_arg <- function(flag, default = NULL) {
  hit <- which(args == flag)
  if (!length(hit)) {
    return(default)
  }
  if (hit[1] == length(args)) {
    stop("Missing value for ", flag, call. = FALSE)
  }
  args[hit[1] + 1]
}

today_tag <- format(Sys.Date(), "%Y%m%d")
review_root <- get_arg("--outdir", silica_review_root(resolve_silica_data_root()))
ensure_pre_aurora_dirs(review_root)

approval_path <- get_arg(
  "--approval",
  latest_pre_aurora_file(
    pre_aurora_approvals_dir(review_root),
    "pre_aurora_approval_mixed_*.csv"
  )
)

if (is.na(approval_path) || !file.exists(approval_path)) {
  stop("Could not find a mixed approval CSV to split. Run 01_pre_aurora_run/01_build_run_candidates.R first.", call. = FALSE)
}

approval <- read.csv(approval_path, check.names = FALSE, stringsAsFactors = FALSE)
if (!"run_type" %in% names(approval)) {
  stop("Approval file must include a run_type column.", call. = FALSE)
}

full_subset <- approval %>%
  filter(run_type == "full_record") %>%
  select(
    LTER, Stream_Name, Discharge_File_Name, Shapefile_Name,
    Watershed_Source, Force_HydroSHEDS
  ) %>%
  distinct()

update_subset <- approval %>%
  filter(run_type == "update_years") %>%
  select(
    LTER, Stream_Name, Discharge_File_Name, Shapefile_Name,
    Watershed_Source, Force_HydroSHEDS
  ) %>%
  distinct()

full_out_path <- file.path(
  pre_aurora_aurora_subsets_dir(review_root),
  paste0("aurora_handoff_full_record_subset_", today_tag, ".csv")
)
update_out_path <- file.path(
  pre_aurora_aurora_subsets_dir(review_root),
  paste0("aurora_handoff_update_years_subset_", today_tag, ".csv")
)
summary_path <- file.path(
  pre_aurora_reports_dir(review_root),
  paste0("aurora_handoff_split_summary_", today_tag, ".csv")
)

write.csv(full_subset, full_out_path, row.names = FALSE, na = "")
write.csv(update_subset, update_out_path, row.names = FALSE, na = "")
write.csv(
  bind_rows(
    count(full_subset, LTER, Watershed_Source, sort = TRUE, name = "n_sites") %>% mutate(run_type = "full_record"),
    count(update_subset, LTER, Watershed_Source, sort = TRUE, name = "n_sites") %>% mutate(run_type = "update_years")
  ) %>%
    select(run_type, Watershed_Source, LTER, n_sites),
  summary_path,
  row.names = FALSE
)

message("Wrote full-record Aurora subset: ", full_out_path)
message("Wrote update-years Aurora subset: ", update_out_path)
message("Wrote split summary: ", summary_path)
message("Suggested next step:")
message("  Rscript 01_pre_aurora_run/03_print_aurora_handoff.R")
