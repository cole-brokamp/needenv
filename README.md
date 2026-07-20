# needenv

[![CRAN status](https://www.r-pkg.org/badges/version/needenv)](https://CRAN.R-project.org/package=needenv)
[![R-CMD-check](https://github.com/cole-brokamp/needenv/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/cole-brokamp/needenv/actions/workflows/R-CMD-check.yaml)

`needenv` is a small, dependency-free validation tool for R code that relies on
[environment
variables](https://stat.ethz.ch/R-manual/R-devel/library/base/html/EnvVar.html).
It checks every requested variable at once, reports all missing variables
together, and makes defaults visible through one aggregated warning.

The package does not [read environment
files](https://stat.ethz.ch/R-manual/R-devel/library/base/html/readRenviron.html)
and never [changes the process
environment](https://stat.ethz.ch/R-manual/R-devel/library/base/html/Sys.getenv.html).

## Installation

Until `needenv` is available on CRAN, install it from GitHub with
[`pak`](https://pak.r-lib.org/):

```r
install.packages("pak")
pak::pkg_install("cole-brokamp/needenv")
```

## Usage

Bare names are the preferred interface:

```r
config <- needenv::needenv(
  API_TOKEN,
  API_URL = "https://example.com"
)
```

Printing the configuration shows which variables were resolved without
revealing their values:

```r
config
#> <needenv configuration>
#> API_TOKEN: <set>
#> API_URL: <set>
```

The returned object is still a named list, so `$` and `[[` retrieve its actual
values. Redacted printing protects against accidental console output; it is not
a security boundary because explicitly accessing, unclassing, or inspecting
the object can still reveal its values.

In this example, `API_TOKEN` is required. `API_URL` uses its process environment value when that
value is set and non-empty; otherwise it uses the supplied default. Defaults are
evaluated only when needed, returned in `config`, and never written to the
process environment.

## Resolved configuration versus the process environment

`needenv()` treats the process environment as input and returns a resolved
configuration for R code to use. A default is an ordinary value in that
returned list; it does not become an environment variable and is not visible to
other code that calls `Sys.getenv()`.

This keeps validation and default selection together at the boundary of a
script or function instead of scattering `Sys.getenv()` calls and checks
throughout its implementation:

```r
run_analysis <- function() {
  config <- needenv::needenv(
    API_TOKEN,
    API_URL = "https://example.com"
  )

  analyze_with_api(
    token = config$API_TOKEN,
    url = config$API_URL
  )
}
```

Users remain responsible for managing their own process environment. If the
variables must actually exist there (for example, because downstream code reads
them independently with `Sys.getenv()`) do not supply defaults:

```r
config <- needenv::needenv(API_TOKEN, API_URL)
```

That call succeeds only when both variables are set and non-empty in the
current process environment. Alternatively, arrange for them to be set before
the check runs, using an [R startup environment
file](https://stat.ethz.ch/R-manual/R-devel/library/base/html/Startup.html) or an
[explicit upstream setup
step](https://stat.ethz.ch/R-manual/R-devel/library/base/html/Sys.getenv.html).

If a default is used, `needenv()` emits one warning containing only the affected
variable names:

```text
Warning: Using default values for environment variables:
- API_URL
```

If required variables are unavailable, one error reports all of them:

```text
Error: Required environment variables are missing or empty:
- API_TOKEN
- DATABASE_URL
```

Values and defaults are never included in these messages.

## Programmatic specifications

Use `.vars` when the specification is stored in a character vector:

```r
spec <- c(
  "API_TOKEN",
  API_URL = "https://example.com"
)

config <- needenv::needenv(.vars = spec)
```

Unnamed elements are required variable names. Named elements use their names as
variable names and their values as defaults. Use either `...` or `.vars` in a
call, not both. A variable may appear only once.

## Preparing the environment before R starts

R handles environment files before `needenv()` runs. A project or user
`.Renviron` file can provide values automatically. To select a particular site
environment file (including one named `.env`) set `R_ENVIRON` before starting R:

```sh
R_ENVIRON=.env Rscript analysis.R
```

`R_ENVIRON_USER` similarly selects a user environment file. The selected file
must use R's Renviron syntax, and setting these selectors inside an already
running R process does not trigger startup processing. See R's
[`Startup`](https://stat.ethz.ch/R-manual/R-devel/library/base/html/Startup.html)
documentation for details.

For an explicit, interactive workflow, use base R to load the file before
calling `needenv()`:

```r
if (file.exists(".env")) {
  readRenviron(".env")
}

config <- needenv::needenv(
  API_TOKEN,
  API_URL = "https://example.com"
)
```

This is an opt-in mutation of the current process environment by
`readRenviron()`, not an action performed by `needenv()`. For scripts, prefer
selecting the file before R starts with `R_ENVIRON` as shown above.

Package authors should call `needenv()` where configuration is actually needed,
never from `.onLoad()`.
