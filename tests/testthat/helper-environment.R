with_envvars <- function(set = character(), unset = character(), code) {
  if (length(set) > 0L && (is.null(names(set)) || any(!nzchar(names(set))))) {
    stop("`set` must be a named character vector.", call. = FALSE)
  }

  variables <- unique(c(names(set), unset))
  old <- Sys.getenv(variables, unset = NA_character_, names = TRUE)

  on.exit({
    Sys.unsetenv(variables)
    restore <- !is.na(old)
    if (any(restore)) {
      do.call(Sys.setenv, as.list(old[restore]))
    }
  }, add = TRUE)

  Sys.unsetenv(variables)
  if (length(set) > 0L) {
    do.call(Sys.setenv, as.list(set))
  }

  force(code)
}

capture_default_warning <- function(code) {
  captured <- NULL
  value <- withCallingHandlers(
    force(code),
    needenv_default = function(condition) {
      captured <<- condition
      invokeRestart("muffleWarning")
    }
  )

  list(value = value, warning = captured)
}

