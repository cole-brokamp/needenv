#' Validate required environment variables
#'
#' `needenv()` checks that a set of environment variables is available and
#' non-empty. Variables may be supplied with defaults. All unresolved
#' variables are reported together, and all variables that use defaults are
#' reported in one warning.
#'
#' @param ... Bare or quoted environment-variable names. Unnamed arguments are
#' required. Named arguments associate the argument name with a default value.
#' Bare names are preferred. Do not supply `...` together with `.vars`.
#' @param .vars `NULL`, or a character vector describing variables
#' programmatically. Unnamed elements contain required variable names; named
#' elements associate their names with defaults. Do not supply `.vars`
#' together with `...`.
#'
#' @return Invisibly, a `needenv_config` object: a named list of scalar
#' character values whose print method redacts all values. If any variables use
#' defaults, a warning of class `needenv_default` is signaled first. If any
#' variables are unresolved, an error of class `needenv_missing` is signaled
#' and no value is returned.
#'
#' @details
#' Each variable is resolved independently. A set, non-empty process
#' environment value is used first. Otherwise, a supplied default is used.
#' A variable without either value is missing. Defaults are returned to the
#' caller but are never written to the process environment. Named default
#' expressions in `...` are evaluated only when their environment value is
#' unavailable.
#'
#' `needenv()` only inspects the current process environment. It does not read
#' environment files and does not call [Sys.setenv()]. Package authors should
#' call it at the point where configuration is needed rather than while their
#' package is loading.
#'
#' @section R startup environment files:
#' R reads site and user environment files before evaluating startup profiles.
#' A project or user `.Renviron` file may therefore prepare variables before
#' `needenv()` runs. `R_ENVIRON` can be set before R starts to select a site
#' environment file, and `R_ENVIRON_USER` can select a user environment file.
#' For example, a shell can launch a script with
#' `R_ENVIRON=.env Rscript analysis.R`. These files must use R's Renviron
#' syntax, and setting the selector inside an already-running R process is too
#' late for startup processing. See [Startup].
#'
#' @examples
#' variable_names <- c("NEEDENV_EXAMPLE_TOKEN", "NEEDENV_EXAMPLE_URL")
#' old_values <- Sys.getenv(variable_names, unset = NA_character_, names = TRUE)
#'
#' Sys.setenv(
#'   NEEDENV_EXAMPLE_TOKEN = "example-token",
#'   NEEDENV_EXAMPLE_URL = "https://configured.example.com"
#' )
#'
#' config <- needenv(
#'   NEEDENV_EXAMPLE_TOKEN,
#'   NEEDENV_EXAMPLE_URL = "https://default.example.com"
#' )
#'
#' # printing redacts values
#' config
#'
#' # but values are still accessible
#' config$NEEDENV_EXAMPLE_TOKEN
#' config$NEEDENV_EXAMPLE_URL
#'
#' spec <- c(
#'   "NEEDENV_EXAMPLE_TOKEN",
#'   NEEDENV_EXAMPLE_URL = "https://default.example.com"
#' )
#' needenv(.vars = spec)
#'
#' Sys.unsetenv(variable_names)
#' restore <- !is.na(old_values)
#' if (any(restore)) {
#'   do.call(Sys.setenv, as.list(old_values[restore]))
#' }
#'
#' @export
needenv <- function(..., .vars = NULL) {
  dots <- as.list(substitute(list(...)))[-1L]
  has_dots <- length(dots) > 0L
  has_vars <- !is.null(.vars)

  if (has_dots && has_vars) {
    .needenv_abort_input(
      "Supply environment variables with either `...` or `.vars`, not both."
    )
  }

  if (!has_dots && !has_vars) {
    .needenv_abort_input(
      "Supply at least one environment variable with `...` or `.vars`."
    )
  }

  spec <- if (has_dots) {
    .needenv_parse_dots(dots, parent.frame())
  } else {
    .needenv_parse_vars(.vars)
  }

  .needenv_reject_duplicates(spec$variables)

  values <- Sys.getenv(
    spec$variables,
    unset = NA_character_,
    names = FALSE
  )
  available <- !is.na(values) & values != ""
  defaulted <- !available & spec$has_default
  missing <- !available & !spec$has_default

  if (any(missing)) {
    .needenv_abort_missing(spec$variables[missing])
  }

  for (i in which(defaulted)) {
    default <- if (spec$evaluate_defaults) {
      eval(spec$defaults[[i]], envir = spec$default_envir)
    } else {
      spec$defaults[[i]]
    }
    values[[i]] <- .needenv_validate_default(default, spec$variables[[i]])
  }
  result <- as.list(values)
  names(result) <- spec$variables
  class(result) <- c("needenv_config", "list")

  if (any(defaulted)) {
    .needenv_warn_default(spec$variables[defaulted])
  }

  invisible(result)
}

.needenv_parse_dots <- function(dots, envir) {
  tags <- names(dots)
  if (is.null(tags)) {
    tags <- rep.int("", length(dots))
  }

  if (anyNA(tags)) {
    .needenv_abort_input("Argument names in `...` cannot be `NA`.")
  }

  variables <- character(length(dots))
  defaults <- vector("list", length(dots))
  has_default <- nzchar(tags)

  for (i in seq_along(dots)) {
    expression <- dots[[i]]

    if (has_default[[i]]) {
      variables[[i]] <- .needenv_validate_name(tags[[i]])
      defaults[[i]] <- expression
    } else if (is.symbol(expression)) {
      variables[[i]] <- .needenv_validate_name(as.character(expression))
    } else if (
      is.character(expression) &&
        length(expression) == 1L &&
        !is.na(expression)
    ) {
      variables[[i]] <- .needenv_validate_name(expression)
    } else {
      .needenv_abort_input(
        paste0(
          "Unnamed arguments in `...` must be bare or quoted environment-variable names. ",
          "Use `.vars` for computed names."
        )
      )
    }
  }

  list(
    variables = variables,
    defaults = defaults,
    has_default = has_default,
    evaluate_defaults = TRUE,
    default_envir = envir
  )
}

.needenv_parse_vars <- function(vars) {
  if (!is.character(vars) || length(vars) == 0L) {
    .needenv_abort_input("`.vars` must be a non-empty character vector.")
  }

  tags <- names(vars)
  if (is.null(tags)) {
    tags <- rep.int("", length(vars))
  }

  if (anyNA(tags)) {
    .needenv_abort_input("Names in `.vars` cannot be `NA`.")
  }

  has_default <- nzchar(tags)
  variables <- character(length(vars))
  defaults <- rep.int(NA_character_, length(vars))

  for (i in seq_along(vars)) {
    if (has_default[[i]]) {
      variables[[i]] <- .needenv_validate_name(tags[[i]])
      defaults[[i]] <- .needenv_validate_default(vars[[i]], variables[[i]])
    } else {
      variables[[i]] <- .needenv_validate_name(vars[[i]])
    }
  }

  list(
    variables = variables,
    defaults = defaults,
    has_default = has_default,
    evaluate_defaults = FALSE,
    default_envir = NULL
  )
}

.needenv_validate_name <- function(name) {
  if (
    !is.character(name) ||
      length(name) != 1L ||
      is.na(name) ||
      !nzchar(name)
  ) {
    .needenv_abort_input(
      "Environment-variable names must be non-empty, non-`NA` strings."
    )
  }

  name
}

.needenv_validate_default <- function(value, variable) {
  if (
    !is.character(value) ||
      length(value) != 1L ||
      is.na(value) ||
      !nzchar(value)
  ) {
    .needenv_abort_input(
      paste0(
        "The default for environment variable `",
        variable,
        "` must be a non-empty, non-`NA` string."
      )
    )
  }

  value
}

.needenv_reject_duplicates <- function(variables) {
  duplicated_variables <- unique(variables[duplicated(variables)])

  if (length(duplicated_variables) > 0L) {
    .needenv_abort_input(
      paste0(
        "Environment variables must be specified only once:\n",
        .needenv_bullets(duplicated_variables)
      )
    )
  }
}

.needenv_bullets <- function(variables) {
  paste0("- ", variables, collapse = "\n")
}

.needenv_abort_input <- function(message) {
  condition <- structure(
    list(message = message, call = NULL),
    class = c("needenv_input", "error", "condition")
  )
  stop(condition)
}

.needenv_abort_missing <- function(missing) {
  condition <- structure(
    list(
      message = paste0(
        "Required environment variables are missing or empty:\n",
        .needenv_bullets(missing)
      ),
      call = NULL,
      missing = missing
    ),
    class = c("needenv_missing", "error", "condition")
  )
  stop(condition)
}

.needenv_warn_default <- function(defaulted) {
  condition <- structure(
    list(
      message = paste0(
        "Using default values for environment variables:\n",
        .needenv_bullets(defaulted)
      ),
      call = NULL,
      defaulted = defaulted
    ),
    class = c("needenv_default", "warning", "condition")
  )
  warning(condition)
}
