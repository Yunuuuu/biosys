`%||%` <- function(x, y) {
    if (is.null(x)) y else x
}

is_scalar <- function(x) {
    length(x) == 1L
}

is_scalar_numeric <- function(x) {
    is_scalar(x) && is.numeric(x)
}

is_number <- function(x) is_scalar_numeric(x) && !is.na(x)

dir_create <- function(dir, ...) {
    if (!dir.exists(dir)) {
        if (!dir.create(path = dir, showWarnings = FALSE, ...)) {
            cli::cli_abort("Cannot create directory {.path {dir}}")
        }
    }
}

path_ext_remove <- function(x) {
    sub("([^.]+)\\.[[:alnum:]]+$", "\\1", x, perl = TRUE)
}

path_ext <- function(x) {
    pos <- regexpr("\\.([[:alnum:]]+)$", x, perl = TRUE)
    ifelse(pos > -1L, substring(x, pos + 1L), "")
}

internal_file <- function(...) {
    system.file(..., package = pkg_nm(), mustWork = TRUE)
}

pkg_nm <- function() {
    utils::packageName(topenv(environment()))
}

build_opath <- function(odir, ofile, call = rlang::caller_env()) {
    assert_string(odir, empty_ok = FALSE, call = call)
    if (!missing(ofile)) assert_string(ofile, empty_ok = FALSE, call = call)
    dir_create(odir)
    file.path(odir, ofile)
}
