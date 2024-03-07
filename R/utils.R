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

dir_create <- function(path, ...) {
    if (!dir.exists(path) &&
        !dir.create(path = path, showWarnings = FALSE, ...)) {
        cli::cli_abort("Cannot create directory {.path {path}}")
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
    if (!is.null(ofile)) file_path(odir, ofile) else odir
}

file_path <- function(..., ext = NULL) {
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

#' Will always add the basename of file into the exdir
#' @noRd
unzip2 <- function(file, exdir, overwrite = TRUE) {
    dir_create(exdir)
    exdir <- file.path(exdir, path_ext_remove(file))
    dir_create(exdir)
    if (is.null(utils::unzip(file, exdir = exdir, overwrite = overwrite))) {
        cli::cli_abort("Cannot unzip {.path {file}}")
    }
    exdir
}

download_file <- function(url, path, ...) {
    cli::cli_inform("Downloading from {.path {url}}")
    if (utils::download.file(url = url, destfile = path, ...) > 0L) {
        cli::cli_abort("Cannot download {.path {url}}")
    }
}

# file.symlink

internal_file <- function(..., dir = "extdata") {
    system.file(dir, ..., package = pkg_nm(), mustWork = TRUE)
}

read_lines <- function(file) {
    data.table::fread(
        file = file, sep = "", header = FALSE,
        colClasses = "character",
        showProgress = FALSE
    )[[1L]]
}

# To write a file with windows line endings use write_lines(eol = "\r\n")
write_lines <- function(text, path, eol = "\n", compress = "auto") {
    data.table::fwrite(
        x = list(text),
        file = path,
        quote = FALSE,
        na = "NA",
        col.names = FALSE,
        logical01 = FALSE,
        showProgress = FALSE,
        verbose = FALSE,
        compress = compress
    )
    invisible(text)
}
