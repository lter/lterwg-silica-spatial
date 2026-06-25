# Optional wrapper config for the numbered workflow
# You can either run the numbered stage scripts directly, or edit this file
# and run `Rscript 02_run-workflow.R`

run_settings <- list(
  # Choose one:
  # "full"         = all sites, all drivers
  # "subset"       = run only the rows in subset_file
  #                  this can mix artisanal and HydroSHEDS rows
  #                  rows in the subset file will go to HydroSHEDS if:
  #                  - you explicitly flag them, or
  #                  - Shapefile_Name is missing and drainage area is greater
  #                    than hydrosheds_area_threshold_km2
  # "update_years" = add only new years for existing sites
  #                  this mode treats sites in two groups:
  #                  - sites we already have: skip static drivers and only run
  #                    the years you request below
  #                  - truly new sites: run the full available record
  # "hydrosheds"   = build a new HydroSHEDS candidate list from the current
  #                  files and STOP for review
  #                  use this when you want the workflow to decide which sites
  #                  still need HydroSHEDS work before running Aurora
  #                  script order after running this mode:
  #                  1. review/run-lists/02_approvals/run_list_approval_mixed_<date>.csv
  #                  2. Rscript 03_spatial_extraction/run-lists/02_split_approved_runs.R
  #                  3. Rscript 03_spatial_extraction/run-lists/03_print_aurora_handoff.R
  #                  the approval output can contain both:
  #                  - run_type = "full_record" for new / never-successful
  #                    sites that need the whole record
  #                  - run_type = "update_years" for previously successful
  #                    HydroSHEDS sites that only need newer target years
  #                  the candidate rule is:
  #                  - drainage area > hydrosheds_area_threshold_km2
  #                  - and either Shapefile_Name is missing
  #                  - or the previous run had no dynamic spatial data
  #                  - or, for HydroSHEDS-derived sites, one or more
  #                    target_years are still missing in the current combined
  #                    table
  #                  set target_years below when you want this year-completeness
  #                  rule to be part of the approval list
  # "combine_review" = combine old + new outputs and write review files
  #                    also called: "combine_qaqc"
  mode = "combine_review",

  # Optional paths / labels
  data_root = "",
  site_followup_file = "",
  # subset_file should point to a CSV file, not a list typed into this script.
  #
  # How this is used:
  # - in "subset" mode:
  #   the workflow runs exactly the rows in this file
  # - in "hydrosheds" mode:
  #   leave this blank if you want the workflow to decide which sites still
  #   need HydroSHEDS
  #   fill this in if you already know which HydroSHEDS sites you want and do
  #   not want the workflow to build the list for you again
  #
  # A mixed subset file is fine. Some rows can use artisanal watersheds and
  # some can use HydroSHEDS in the same run.
  #
  # You can still force HydroSHEDS for a row with:
  # - Watershed_Source = "hydrosheds"
  # - Force_HydroSHEDS = TRUE
  #
  # But you do not have to set those columns just because a row is missing
  # Shapefile_Name. In subset mode, the workflow will automatically send that
  # row to HydroSHEDS if its drainage area is above the threshold below.
  subset_file = "",
  # Leave blank to use <data_root>/review by default.
  qa_root = "",
  output_date = format(Sys.Date(), "%Y%m%d"),
  run_label = "",
  write_detailed_review = FALSE,
  dry_run = FALSE,

  # In subset mode, rows with no Shapefile_Name and drainage area above this
  # threshold are sent to HydroSHEDS automatically.
  hydrosheds_area_threshold_km2 = 100,
  # In update_years mode, or in hydrosheds mode when you want to flag
  # HydroSHEDS-derived sites that are missing newer years, use one of these:
  # - start_year + end_year
  # - target_years = c(2024, 2025)
  # Leave them blank for the other run modes.
  start_year = NA,
  end_year = NA,
  target_years = integer(0),

  # After extraction, should this run also build the combined local output?
  # Leave this FALSE for most runs. The cleaner pattern is:
  # 1. run extraction
  # 2. run combine / QAQC separately in 04_combine_qaqc
  combine_after_extract = FALSE,
  skip_drive_auth = TRUE,
  skip_drive_upload = TRUE,

  # Watershed rebuild options
  rebuild_artisanal = TRUE,
  rebuild_hydrosheds = TRUE
)
