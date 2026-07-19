test_that("bare names resolve to an invisible named list", {
  with_envvars(
    set = c(
      NEEDENV_TEST_FIRST = "first-value",
      NEEDENV_TEST_SECOND = "second-value"
    ),
    code = {
      result <- withVisible(needenv(NEEDENV_TEST_FIRST, NEEDENV_TEST_SECOND))

      expect_false(result$visible)
      expect_type(result$value, "list")
      expect_s3_class(result$value, "needenv_config")
      expect_named(result$value, c("NEEDENV_TEST_FIRST", "NEEDENV_TEST_SECOND"))
      expect_identical(result$value$NEEDENV_TEST_FIRST, "first-value")
      expect_identical(result$value[["NEEDENV_TEST_SECOND"]], "second-value")
    }
  )
})

test_that("printing configurations fully redacts values", {
  with_envvars(
    set = c(NEEDENV_TEST_PRINT_TOKEN = "super-secret-token"),
    unset = "NEEDENV_TEST_PRINT_DEFAULT",
    code = {
      captured <- capture_default_warning(
        needenv(
          NEEDENV_TEST_PRINT_TOKEN,
          NEEDENV_TEST_PRINT_DEFAULT = "secret-default"
        )
      )
      visibility <- NULL
      output <- capture.output(
        visibility <- withVisible(print(captured$value))
      )

      expect_match(output[[1L]], "<needenv configuration>", fixed = TRUE)
      expect_true(any(grepl("NEEDENV_TEST_PRINT_TOKEN: <set>", output, fixed = TRUE)))
      expect_true(any(grepl("NEEDENV_TEST_PRINT_DEFAULT: <set>", output, fixed = TRUE)))
      expect_false(any(grepl("super-secret-token", output, fixed = TRUE)))
      expect_false(any(grepl("secret-default", output, fixed = TRUE)))
      expect_false(visibility$visible)
      expect_identical(visibility$value, captured$value)
    }
  )
})

test_that("bare names are captured without evaluating R objects", {
  NEEDENV_TEST_CAPTURE <- "an R object, not the environment value"

  with_envvars(
    set = c(NEEDENV_TEST_CAPTURE = "environment-value"),
    code = {
      result <- needenv(NEEDENV_TEST_CAPTURE)
      expect_identical(result$NEEDENV_TEST_CAPTURE, "environment-value")
    }
  )
})

test_that("quoted names remain supported", {
  with_envvars(
    set = c(NEEDENV_TEST_QUOTED = "quoted-value"),
    code = {
      result <- needenv("NEEDENV_TEST_QUOTED")
      expect_identical(result$NEEDENV_TEST_QUOTED, "quoted-value")
    }
  )
})

test_that("quoted names can associate defaults", {
  with_envvars(
    unset = "NEEDENV_TEST_QUOTED_DEFAULT",
    code = {
      captured <- capture_default_warning(
        needenv("NEEDENV_TEST_QUOTED_DEFAULT" = "quoted-default")
      )
      expect_identical(
        captured$value$NEEDENV_TEST_QUOTED_DEFAULT,
        "quoted-default"
      )
    }
  )
})

test_that("environment values take precedence over defaults", {
  with_envvars(
    set = c(NEEDENV_TEST_PRECEDENCE = "environment-value"),
    code = {
      expect_no_warning(
        result <- needenv(NEEDENV_TEST_PRECEDENCE = "default-value")
      )
      expect_identical(result$NEEDENV_TEST_PRECEDENCE, "environment-value")
    }
  )
})

test_that("unused default expressions are not evaluated", {
  with_envvars(
    set = c(NEEDENV_TEST_LAZY_DEFAULT = "environment-value"),
    code = {
      result <- needenv(
        NEEDENV_TEST_LAZY_DEFAULT = stop("unused default was evaluated")
      )
      expect_identical(result$NEEDENV_TEST_LAZY_DEFAULT, "environment-value")
    }
  )
})

test_that("named defaults are evaluated in the caller", {
  fallback <- "computed-default"

  with_envvars(
    unset = "NEEDENV_TEST_COMPUTED_DEFAULT",
    code = {
      captured <- capture_default_warning(
        needenv(NEEDENV_TEST_COMPUTED_DEFAULT = fallback)
      )
      expect_identical(
        captured$value$NEEDENV_TEST_COMPUTED_DEFAULT,
        "computed-default"
      )
    }
  )
})

test_that("defaults produce one structured warning in input order", {
  with_envvars(
    unset = c("NEEDENV_TEST_DEFAULT_B", "NEEDENV_TEST_DEFAULT_A"),
    code = {
      captured <- capture_default_warning(
        needenv(
          NEEDENV_TEST_DEFAULT_B = "b-default",
          NEEDENV_TEST_DEFAULT_A = "a-default"
        )
      )

      expect_s3_class(captured$warning, "needenv_default")
      expect_identical(
        captured$warning$defaulted,
        c("NEEDENV_TEST_DEFAULT_B", "NEEDENV_TEST_DEFAULT_A")
      )
      expect_identical(
        names(captured$value),
        c("NEEDENV_TEST_DEFAULT_B", "NEEDENV_TEST_DEFAULT_A")
      )
      expect_match(
        conditionMessage(captured$warning),
        "NEEDENV_TEST_DEFAULT_B.*NEEDENV_TEST_DEFAULT_A"
      )
      expect_false(grepl("b-default", conditionMessage(captured$warning), fixed = TRUE))
      expect_false(grepl("a-default", conditionMessage(captured$warning), fixed = TRUE))
    }
  )
})

test_that("missing variables produce one structured error in input order", {
  with_envvars(
    unset = c("NEEDENV_TEST_MISSING_B", "NEEDENV_TEST_MISSING_A"),
    code = {
      condition <- tryCatch(
        needenv(NEEDENV_TEST_MISSING_B, NEEDENV_TEST_MISSING_A),
        needenv_missing = identity
      )

      expect_s3_class(condition, "needenv_missing")
      expect_identical(
        condition$missing,
        c("NEEDENV_TEST_MISSING_B", "NEEDENV_TEST_MISSING_A")
      )
      expect_match(
        conditionMessage(condition),
        "NEEDENV_TEST_MISSING_B.*NEEDENV_TEST_MISSING_A"
      )
    }
  )
})

test_that("missing variables suppress the defaults warning", {
  with_envvars(
    unset = c("NEEDENV_TEST_MISSING", "NEEDENV_TEST_WOULD_DEFAULT"),
    code = {
      warned <- FALSE
      condition <- tryCatch(
        withCallingHandlers(
          needenv(
            NEEDENV_TEST_MISSING,
            NEEDENV_TEST_WOULD_DEFAULT = "fallback"
          ),
          needenv_default = function(condition) {
            warned <<- TRUE
          }
        ),
        needenv_missing = identity
      )

      expect_s3_class(condition, "needenv_missing")
      expect_false(warned)
      expect_identical(condition$missing, "NEEDENV_TEST_MISSING")
    }
  )
})

test_that("empty environment values are unavailable", {
  with_envvars(
    set = c(
      NEEDENV_TEST_EMPTY_DEFAULT = "",
      NEEDENV_TEST_EMPTY_MISSING = ""
    ),
    code = {
      captured <- capture_default_warning(
        needenv(NEEDENV_TEST_EMPTY_DEFAULT = "fallback")
      )
      expect_identical(captured$value$NEEDENV_TEST_EMPTY_DEFAULT, "fallback")

      condition <- tryCatch(
        needenv(NEEDENV_TEST_EMPTY_MISSING),
        needenv_missing = identity
      )
      expect_identical(condition$missing, "NEEDENV_TEST_EMPTY_MISSING")
    }
  )
})

test_that("programmatic specifications support requirements and defaults", {
  with_envvars(
    set = c(NEEDENV_TEST_VECTOR_REQUIRED = "required-value"),
    unset = "NEEDENV_TEST_VECTOR_DEFAULT",
    code = {
      spec <- c(
        "NEEDENV_TEST_VECTOR_REQUIRED",
        NEEDENV_TEST_VECTOR_DEFAULT = "vector-default"
      )
      captured <- capture_default_warning(needenv(.vars = spec))

      expect_identical(
        names(captured$value),
        c("NEEDENV_TEST_VECTOR_REQUIRED", "NEEDENV_TEST_VECTOR_DEFAULT")
      )
      expect_identical(
        captured$value$NEEDENV_TEST_VECTOR_REQUIRED,
        "required-value"
      )
      expect_identical(
        captured$value$NEEDENV_TEST_VECTOR_DEFAULT,
        "vector-default"
      )
      expect_identical(
        captured$warning$defaulted,
        "NEEDENV_TEST_VECTOR_DEFAULT"
      )
    }
  )
})

test_that("defaults never modify the process environment", {
  with_envvars(
    unset = "NEEDENV_TEST_NO_MUTATION",
    code = {
      captured <- capture_default_warning(
        needenv(NEEDENV_TEST_NO_MUTATION = "fallback")
      )

      expect_identical(captured$value$NEEDENV_TEST_NO_MUTATION, "fallback")
      expect_identical(
        Sys.getenv("NEEDENV_TEST_NO_MUTATION", unset = NA_character_),
        NA_character_
      )
    }
  )
})

test_that("needenv does not alter existing process values", {
  with_envvars(
    set = c(NEEDENV_TEST_EXISTING = "original"),
    code = {
      needenv(NEEDENV_TEST_EXISTING)
      expect_identical(Sys.getenv("NEEDENV_TEST_EXISTING"), "original")
    }
  )
})

test_that("input modes are mutually exclusive", {
  expect_error(
    needenv(NEEDENV_TEST_MIXED, .vars = "NEEDENV_TEST_VECTOR"),
    class = "needenv_input"
  )
})

test_that("duplicates are rejected within either input mode", {
  expect_error(
    needenv(NEEDENV_TEST_DUPLICATE, NEEDENV_TEST_DUPLICATE),
    class = "needenv_input"
  )
  expect_error(
    needenv(NEEDENV_TEST_DUPLICATE, NEEDENV_TEST_DUPLICATE = "fallback"),
    class = "needenv_input"
  )
  expect_error(
    needenv(.vars = c("NEEDENV_TEST_DUPLICATE", "NEEDENV_TEST_DUPLICATE")),
    class = "needenv_input"
  )
  expect_error(
    needenv(
      .vars = c(
        "NEEDENV_TEST_DUPLICATE",
        NEEDENV_TEST_DUPLICATE = "fallback"
      )
    ),
    class = "needenv_input"
  )
})

test_that("unsupported expressions point callers to .vars", {
  expect_error(
    needenv(paste0("NEEDENV", "_TEST_COMPUTED")),
    "Use `.vars` for computed names",
    fixed = TRUE,
    class = "needenv_input"
  )
})

test_that("NA argument names in dots are rejected", {
  dots <- list(quote(NEEDENV_TEST_NA_TAG))
  names(dots) <- NA_character_

  expect_error(
    .needenv_parse_dots(dots, environment()),
    "Argument names in `...` cannot be `NA`.",
    fixed = TRUE,
    class = "needenv_input"
  )
})

test_that("invalid specifications fail with input errors", {
  expect_error(needenv(), class = "needenv_input")
  expect_error(needenv(.vars = character()), class = "needenv_input")
  expect_error(needenv(.vars = list("NEEDENV_TEST")), class = "needenv_input")
  expect_error(needenv(.vars = NA_character_), class = "needenv_input")
  expect_error(needenv(.vars = ""), class = "needenv_input")
  expect_error(needenv(NEEDENV_TEST_BAD_DEFAULT = ""), class = "needenv_input")
  expect_error(needenv(NEEDENV_TEST_BAD_DEFAULT = NA_character_), class = "needenv_input")
  expect_error(needenv(NEEDENV_TEST_BAD_DEFAULT = 1), class = "needenv_input")
  expect_error(needenv(NEEDENV_TEST_BAD_DEFAULT = c("a", "b")), class = "needenv_input")

  bad_names <- "value"
  names(bad_names) <- NA_character_
  expect_error(needenv(.vars = bad_names), class = "needenv_input")
})

test_that("condition messages never reveal environment values or defaults", {
  with_envvars(
    set = c(NEEDENV_TEST_SECRET = "super-secret-value"),
    unset = c("NEEDENV_TEST_MISSING_SECRET", "NEEDENV_TEST_DEFAULT_SECRET"),
    code = {
      missing <- tryCatch(
        needenv(NEEDENV_TEST_SECRET, NEEDENV_TEST_MISSING_SECRET),
        needenv_missing = identity
      )
      expect_false(grepl("super-secret-value", conditionMessage(missing), fixed = TRUE))

      defaulted <- capture_default_warning(
        needenv(NEEDENV_TEST_DEFAULT_SECRET = "secret-default")
      )
      expect_false(
        grepl("secret-default", conditionMessage(defaulted$warning), fixed = TRUE)
      )
    }
  )
})
