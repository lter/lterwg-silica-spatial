#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(jsonlite)
})

args <- commandArgs(trailingOnly = TRUE)
run_date <- if (length(args) >= 1 && nzchar(args[[1]])) args[[1]] else format(Sys.Date(), "%Y%m%d")

base_dir <- file.path(
  "/home/shares/lter-si/si-watershed-extract/review/01_pre_aurora_run/05_appeears_backfill",
  paste0(run_date, "_cameroon_evapo_v061")
)

if (!dir.exists(base_dir)) {
  stop("Missing AppEEARS backfill directory: ", base_dir, call. = FALSE)
}

template_path <- file.path(
  base_dir,
  paste0("appeears_task_cameroon-evapo-v061-2000_2012-ET_500m-", run_date, ".json")
)

if (!file.exists(template_path)) {
  stop("Missing template JSON: ", template_path, call. = FALSE)
}

template <- jsonlite::read_json(template_path, simplifyVector = FALSE)

windows <- list(
  list(tag = "2000_2004", start = "01-01-2000", end = "12-31-2004"),
  list(tag = "2005_2008", start = "01-01-2005", end = "12-31-2008"),
  list(tag = "2009_2012", start = "01-01-2009", end = "12-31-2012"),
  list(tag = "2023_only_retry2", start = "01-01-2023", end = "12-31-2023")
)

submit_lines <- c(
  "#!/usr/bin/env bash",
  "set -euo pipefail",
  "",
  "if [[ -z \"${APPEEARS_TOKEN:-}\" ]]; then",
  "  echo \"APPEEARS_TOKEN is not set in the shell.\" >&2",
  "  exit 1",
  "fi",
  ""
)

for (w in windows) {
  payload <- template
  payload$task_name <- paste0("cameroon-evapo-v061-", w$tag, "-ET_500m-", run_date)
  payload$params$dates[[1]]$startDate <- w$start
  payload$params$dates[[1]]$endDate <- w$end

  out_json <- file.path(
    base_dir,
    paste0("appeears_task_cameroon-evapo-v061-", w$tag, "-ET_500m-", run_date, ".json")
  )

  jsonlite::write_json(payload, out_json, auto_unbox = TRUE, pretty = TRUE)

  submit_lines <- c(
    submit_lines,
    paste0("echo 'Submitting ", payload$task_name, "'"),
    paste0(
      "curl -sS -X POST \"https://appeears.earthdatacloud.nasa.gov/api/task\" ",
      "-H \"Authorization: Bearer $APPEEARS_TOKEN\" ",
      "-H \"Content-Type: application/json\" ",
      "--data @\"",
      out_json,
      "\""
    ),
    ""
  )
}

submit_path <- file.path(base_dir, paste0("submit_chunked_retries_", run_date, ".sh"))
writeLines(submit_lines, submit_path)

message("Wrote chunked retry payloads under: ", base_dir)
message("Submit script: ", submit_path)
