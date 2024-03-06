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

pkg_nm <- function() {
    utils::packageName(topenv(environment()))
}

dir_create <- function(dir, ...) {
    if (!dir.exists(dir)) {
        if (!dir.create(path = dir, showWarnings = FALSE, ...)) {
            cli::cli_abort("Cannot create directory {.path {dir}}")
        }
    }
}

build_opath <- function(odir, ofile = NULL, abs = FALSE, call = rlang::caller_env()) {
    assert_string(odir, empty_ok = FALSE, arg = substitute(odir), call = call)
    assert_string(ofile,
        empty_ok = FALSE, null_ok = TRUE,
        arg = substitute(ofile), call = call
    )
    dir_create(odir)
    # whether to use absolute path
    if (abs) odir <- normalizePath(odir, "/", mustWork = TRUE)
    if (!is.null(ofile)) path(odir, ofile) else odir
}

path <- function(..., ext = NULL) {
    paths <- file.path(..., fsep = "/")
    if (!is.null(ext)) paths <- paste(paths, ext, sep = ".")
    paths
}

path_ext_remove <- function(path, compression = FALSE) {
    if (compression) {
        path <- sub(
            "\\.(br|bz2|gz|lz|lz4|lzma|lzo|rz|xz|7z|zip)$",
            "", path,
            perl = TRUE
        )
    }
    sub("\\.[[:alnum:]]+$", "", path, perl = TRUE)
}

path_ext <- function(path) {
    pos <- regexpr("\\.([[:alnum:]]+)$", path, perl = TRUE)
    ifelse(pos > -1L, substring(path, pos + 1L), "")
}

internal_file <- function(...) {
    system.file(..., package = pkg_nm(), mustWork = TRUE)
}
