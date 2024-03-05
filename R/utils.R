`%||%` <- function(x, y) {
    if (is.null(x)) y else x
}

is_scalar <- function(x) {
    length(x) == 1L
}

is_scalar_numeric <- function(x) {
    is_scalar(x) && is.numeric(x)
}

dir_create <- function(dir, ...) {
    if (!dir.exists(dir)) {
        if (!dir.create(path = dir, showWarnings = FALSE, ...)) {
            cli::cli_abort("Cannot create directory {.path {dir}}")
        }
    }
}

internal_file <- function(...) {
    system.file(..., package = pkg_nm(), mustWork = TRUE)
}

pkg_nm <- function() {
    utils::packageName(topenv(environment()))
}

build_opath <- function(odir, ofile, call = rlang::caller_env()) {
    assert_string(odir, empty_ok = FALSE, call = call)
    if (!missing(ofile)) assert_string(ofile, empty_ok = FALSE)
    dir_create(odir)
    file.path(odir, ofile)
}
