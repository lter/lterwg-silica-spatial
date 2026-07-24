#!/usr/bin/env Rscript

# Check every publication-facing script for portability and basic validity.
#
# Usage:
#   Rscript tools/audit_publication_scripts.R
#   Rscript tools/audit_publication_scripts.R --root path/to/repository
#   Rscript tools/audit_publication_scripts.R --skip-syntax

args <- commandArgs(trailingOnly = TRUE)

argument_value <- function(arguments, name, default = NULL) {
  exact_index <- which(arguments == name)
  if (length(exact_index) > 0L) {
    value_index <- exact_index[[1]] + 1L
    if (value_index > length(arguments)) {
      stop("Missing value after ", name, ".", call. = FALSE)
    }
    return(arguments[[value_index]])
  }

  prefix <- paste0(name, "=")
  inline_index <- grep(paste0("^", name, "="), arguments)
  if (length(inline_index) > 0L) {
    return(sub(paste0("^", prefix), "", arguments[[inline_index[[1]]]]))
  }
  default
}

script_argument <- grep("^--file=", commandArgs(), value = TRUE)
script_path <- if (length(script_argument) > 0L) {
  sub("^--file=", "", script_argument[[1]])
} else {
  file.path(getwd(), "tools", "audit_publication_scripts.R")
}
script_path <- normalizePath(script_path, mustWork = FALSE)
default_root <- dirname(dirname(script_path))

root <- normalizePath(
  argument_value(args, "--root", default_root),
  mustWork = TRUE
)
skip_syntax <- "--skip-syntax" %in% args

script_extensions <- c("R", "py", "sh", "mjs")
excluded_directories <- c(
  ".git",
  ".Rproj.user",
  "__pycache__",
  "generated_outputs",
  "outputs"
)
forbidden_text <- c(
  "personal macOS path" = paste0("/", "Users/"),
  "temporary Codex attachment" = paste0(".", "codex/attachments"),
  "temporary macOS path" = paste0("/private/", "tmp/")
)

relative_path <- function(path) {
  root_prefix <- paste0(root, .Platform$file.sep)
  if (startsWith(path, root_prefix)) {
    substring(path, nchar(root_prefix) + 1L)
  } else {
    path
  }
}

has_excluded_directory <- function(path) {
  components <- strsplit(
    relative_path(path),
    split = .Platform$file.sep,
    fixed = TRUE
  )[[1]]
  any(components %in% excluded_directories)
}

all_files <- list.files(
  root,
  recursive = TRUE,
  full.names = TRUE,
  all.files = TRUE,
  include.dirs = FALSE,
  no.. = TRUE
)
extensions <- tools::file_ext(all_files)
scripts <- all_files[
  extensions %in% script_extensions &
    !vapply(all_files, has_excluded_directory, logical(1))
]
scripts <- sort(
  normalizePath(scripts, mustWork = TRUE)
)

if (length(scripts) == 0L) {
  stop("No scripts found under ", root, ".", call. = FALSE)
}

errors <- character()
warnings <- character()

for (path in scripts) {
  relative <- relative_path(path)
  lines <- readLines(path, warn = FALSE, encoding = "UTF-8")
  text <- paste(lines, collapse = "\n")

  for (description in names(forbidden_text)) {
    forbidden <- forbidden_text[[description]]
    if (grepl(forbidden, text, fixed = TRUE)) {
      errors <- c(
        errors,
        paste0(relative, ": contains a ", description)
      )
    }
  }

  stable_name <- tools::file_path_sans_ext(basename(path))
  if (grepl("20[0-9]{6}", stable_name)) {
    errors <- c(
      errors,
      paste0(
        relative,
        ": date-stamped filename; use a stable workflow name"
      )
    )
  }

  line_count <- length(lines)
  if (line_count > 1200L) {
    warnings <- c(
      warnings,
      paste0(
        relative,
        ": ",
        line_count,
        " lines; consider another module boundary"
      )
    )
  }
}

command_result <- function(command, command_args) {
  output <- suppressWarnings(
    system2(
      command,
      args = command_args,
      stdout = TRUE,
      stderr = TRUE
    )
  )
  status <- attr(output, "status")
  if (is.null(status)) status <- 0L
  list(status = status, output = output)
}

syntax_failure_message <- function(relative, result) {
  detail <- trimws(result$output)
  detail <- detail[nzchar(detail)]
  suffix <- if (length(detail) > 0L) {
    paste0(": ", tail(detail, 1L))
  } else {
    ""
  }
  paste0(relative, ": syntax check failed", suffix)
}

if (!skip_syntax) {
  python <- Sys.which("python3")
  bash <- Sys.which("bash")
  node <- Sys.which("node")
  python_check <- paste(
    "import ast,sys;",
    "source=open(sys.argv[1], encoding='utf-8').read();",
    "ast.parse(source, filename=sys.argv[1])"
  )

  for (path in scripts) {
    relative <- relative_path(path)
    extension <- tools::file_ext(path)

    if (extension == "R") {
      parse_error <- tryCatch(
        {
          parse(file = path)
          NULL
        },
        error = function(error) conditionMessage(error)
      )
      if (!is.null(parse_error)) {
        errors <- c(
          errors,
          paste0(relative, ": syntax check failed: ", parse_error)
        )
      }
    } else if (extension == "py") {
      if (!nzchar(python)) {
        warnings <- c(
          warnings,
          paste0(
            relative,
            ": no local Python interpreter available for syntax check"
          )
        )
      } else {
        result <- command_result(
          python,
          c("-c", shQuote(python_check), shQuote(path))
        )
        if (result$status != 0L) {
          errors <- c(errors, syntax_failure_message(relative, result))
        }
      }
    } else if (extension == "sh") {
      if (!nzchar(bash)) {
        warnings <- c(
          warnings,
          paste0(
            relative,
            ": no local Bash interpreter available for syntax check"
          )
        )
      } else {
        result <- command_result(bash, c("-n", shQuote(path)))
        if (result$status != 0L) {
          errors <- c(errors, syntax_failure_message(relative, result))
        }
      }
    } else if (extension == "mjs") {
      if (!nzchar(node)) {
        warnings <- c(
          warnings,
          paste0(
            relative,
            ": no local Node.js interpreter available for syntax check"
          )
        )
      } else {
        result <- command_result(node, c("--check", shQuote(path)))
        if (result$status != 0L) {
          errors <- c(errors, syntax_failure_message(relative, result))
        }
      }
    }
  }
}

extension_counts <- table(tools::file_ext(scripts))
summary <- paste(
  paste0(names(extension_counts), "=", as.integer(extension_counts)),
  collapse = ", "
)
cat(
  "Audited ",
  length(scripts),
  " scripts (",
  summary,
  ").\n",
  sep = ""
)

if (length(warnings) > 0L) {
  cat(paste0("WARNING: ", warnings, "\n"), sep = "")
}
if (length(errors) > 0L) {
  cat(paste0("ERROR: ", errors, "\n"), sep = "")
  cat(
    "Publication script audit failed with ",
    length(errors),
    " error(s).\n",
    sep = ""
  )
  quit(status = 1L, save = "no")
}

cat("Publication script audit passed.\n")
