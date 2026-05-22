args <- commandArgs(trailingOnly = TRUE)
source(file.path("01_pre_aurora_run", "00_pre_aurora_paths.R"))
source(file.path("tools", "subset_and_output_helpers.R"))

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
target_years <- silica_target_years()

full_subset <- get_arg(
  "--full-subset",
  latest_pre_aurora_file(
    pre_aurora_aurora_subsets_dir(review_root),
    "aurora_handoff_full_record_subset_*.csv"
  )
)
update_subset <- get_arg(
  "--update-subset",
  latest_pre_aurora_file(
    pre_aurora_aurora_subsets_dir(review_root),
    "aurora_handoff_update_years_subset_*.csv"
  )
)

cat("Aurora handoff\n")
cat("==============\n\n")
cat("Run order:\n")
cat("1. Review the mixed approval file.\n")
cat("2. Confirm which rows require HydroSHEDS and which already have usable shapefiles.\n")
cat("3. Split it into full_record and update_years subsets.\n")
cat("4. Run the full_record subset on Aurora.\n")
cat("5. Run the update_years subset on Aurora with target years.\n\n")

if (!is.na(full_subset) && file.exists(full_subset)) {
  cat("Aurora command for the full-record pass:\n")
  cat(
    "scp ", full_subset, " bush@aurora.nceas.ucsb.edu:/home/shares/lter-si/si-watershed-extract/review/01_pre_aurora_run/\n",
    sep = ""
  )
  cat(
    "ssh bush@aurora.nceas.ucsb.edu 'cd /home/shares/lter-si/si-watershed-extract && ",
    "SILICA_RUN_LABEL=aurora-full-record-",
    today_tag,
    " bash 03_spatial_extraction/aurora/run_targeted_subset_aurora.sh /home/shares/lter-si/si-watershed-extract/review/01_pre_aurora_run/",
    basename(full_subset),
    "'\n\n",
    sep = ""
  )
}

if (!is.na(update_subset) && file.exists(update_subset)) {
  year_text <- if (length(target_years)) paste(target_years, collapse = ",") else "2024"
  cat("Aurora command for the update-years pass:\n")
  cat(
    "scp ", update_subset, " bush@aurora.nceas.ucsb.edu:/home/shares/lter-si/si-watershed-extract/review/01_pre_aurora_run/\n",
    sep = ""
  )
  cat(
    "ssh bush@aurora.nceas.ucsb.edu 'cd /home/shares/lter-si/si-watershed-extract && ",
    "SILICA_TARGET_YEARS=",
    year_text,
    " SILICA_RUN_LABEL=aurora-update-years-",
    today_tag,
    " SILICA_SITE_SUBSET_FILE=/home/shares/lter-si/si-watershed-extract/review/01_pre_aurora_run/",
    basename(update_subset),
    " Rscript 02_run-workflow.R'\n",
    sep = ""
  )
}
