# Small, dependency-free helpers for publication-facing R command-line tools.
#
# Conventions
# -----------
# - Required inputs are passed as `--flag /path/to/file`.
# - Environment variables may provide defaults for shared infrastructure.
# - Personal absolute paths are never embedded in scripts.
# - A missing value produces one short, actionable error.
# - Boolean values accept true/false, yes/no, 1/0, and y/n.

cli_has_flag <- function(args, flag) {
  flag %in% args
}

cli_value <- function(
  args,
  flag,
  default = NULL,
  required = FALSE,
  allow_empty = FALSE
) {
  positions <- which(args == flag)
  if (!length(positions)) {
    if (required) {
      stop("Missing required option ", flag, ".", call. = FALSE)
    }
    return(default)
  }

  position <- positions[[1]]
  if (position == length(args) || startsWith(args[[position + 1L]], "--")) {
    stop("Option ", flag, " requires a value.", call. = FALSE)
  }

  value <- args[[position + 1L]]
  if (!allow_empty && !nzchar(trimws(value))) {
    stop("Option ", flag, " cannot be empty.", call. = FALSE)
  }
  value
}

cli_values <- function(args, flag) {
  positions <- which(args == flag)
  if (!length(positions)) {
    return(character())
  }
  values <- vapply(
    positions,
    function(position) {
      if (position == length(args) ||
        startsWith(args[[position + 1L]], "--")) {
        stop("Option ", flag, " requires a value.", call. = FALSE)
      }
      args[[position + 1L]]
    },
    character(1)
  )
  values[nzchar(trimws(values))]
}

cli_without_option <- function(args, flag) {
  positions <- which(args == flag)
  if (!length(positions)) {
    return(args)
  }
  remove <- unique(c(positions, positions + 1L))
  args[!seq_along(args) %in% remove]
}

parse_boolean <- function(value, label = "value") {
  normalized <- tolower(trimws(as.character(value)))
  if (normalized %in% c("true", "t", "yes", "y", "1")) {
    return(TRUE)
  }
  if (normalized %in% c("false", "f", "no", "n", "0")) {
    return(FALSE)
  }
  stop(
    label,
    " must be true/false, yes/no, y/n, or 1/0.",
    call. = FALSE
  )
}

cli_boolean <- function(args, flag, default = FALSE) {
  value <- cli_value(
    args,
    flag,
    default = if (isTRUE(default)) "true" else "false"
  )
  parse_boolean(value, flag)
}

cli_integer <- function(
  args,
  flag,
  default = NULL,
  required = FALSE,
  minimum = NULL,
  maximum = NULL
) {
  value <- cli_value(
    args,
    flag,
    default = default,
    required = required
  )
  if (is.null(value)) {
    return(NULL)
  }

  parsed <- suppressWarnings(as.integer(value))
  if (is.na(parsed)) {
    stop(flag, " must be an integer.", call. = FALSE)
  }
  if (!is.null(minimum) && parsed < minimum) {
    stop(flag, " must be at least ", minimum, ".", call. = FALSE)
  }
  if (!is.null(maximum) && parsed > maximum) {
    stop(flag, " must be no greater than ", maximum, ".", call. = FALSE)
  }
  parsed
}

env_value <- function(name, default = NULL, required = FALSE) {
  value <- trimws(Sys.getenv(name, unset = ""))
  if (!nzchar(value)) {
    if (required) {
      stop("Set ", name, " or pass the corresponding CLI option.", call. = FALSE)
    }
    return(default)
  }
  value
}

env_boolean <- function(name, default = FALSE) {
  value <- env_value(
    name,
    default = if (isTRUE(default)) "true" else "false"
  )
  parse_boolean(value, name)
}

require_input_file <- function(path, label = "input file") {
  if (is.null(path) || !nzchar(trimws(path))) {
    stop("Provide ", label, ".", call. = FALSE)
  }
  if (!file.exists(path)) {
    stop("Missing ", label, ": ", path, call. = FALSE)
  }
  normalizePath(path, mustWork = TRUE)
}

require_input_dir <- function(path, label = "input directory") {
  if (is.null(path) || !nzchar(trimws(path))) {
    stop("Provide ", label, ".", call. = FALSE)
  }
  if (!dir.exists(path)) {
    stop("Missing ", label, ": ", path, call. = FALSE)
  }
  normalizePath(path, mustWork = TRUE)
}

prepare_output_dir <- function(path, is_file = FALSE) {
  if (is.null(path) || !nzchar(trimws(path))) {
    stop("Provide an output path.", call. = FALSE)
  }
  directory <- if (isTRUE(is_file)) dirname(path) else path
  dir.create(directory, recursive = TRUE, showWarnings = FALSE)
  invisible(normalizePath(directory, mustWork = TRUE))
}

assert_required_columns <- function(data, columns, label = "table") {
  missing <- setdiff(columns, names(data))
  if (length(missing)) {
    stop(
      label,
      " is missing required columns: ",
      paste(missing, collapse = ", "),
      call. = FALSE
    )
  }
  invisible(data)
}
