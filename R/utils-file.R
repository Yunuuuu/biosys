dir_create <- function(path, ...) {
    if (!dir.exists(path) &&
        !dir.create(path = path, showWarnings = FALSE, ...)) {
        cli::cli_abort("Cannot create directory {.path {path}}")
    }
}

file_path <- function(..., ext = NULL) {
    path <- file.path(..., fsep = "/")
    if (!is.null(ext)) path <- paste(path, ext, sep = ".")
    path
}

path_ext_remove <- function(path) {
    sub("\\.[[:alnum:]]*$", "", path, perl = TRUE)
}

path_ext_set <- function(path, ext) {
    if (!is.null(ext) && nzchar(ext)) {
        path <- paste(path_ext_remove(path), ext, sep = ".")
    }
    path
}

path_ext <- function(path) {
    matches <- regexpr("\\.([[:alnum:]]+)$", path, perl = TRUE)
    start <- as.vector(matches)
    end <- start + attr(matches, "match.length") - 1L
    ifelse(start == -1L, "", substr(path, start + 1L, end))
}

path_trim <- function(path) {
    # remove trailing backslash or slash
    sub("(\\\\+|/+)$", "", path, perl = TRUE)
}

unlink2 <- function(path, ...) {
    if (file.exists(path) && unlink(x = path, ...)) {
        cli::cli_abort("Canno remove {.path {path}}")
    }
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

internal_file <- function(..., dir = "extdata") {
    system.file(dir, ..., package = pkg_nm(), mustWork = TRUE)
}

read_lines <- function(file, n = -1L, connection = TRUE) {
    # data.table don't support xz compression
    if (connection) {
        if (is_gzip_suffix(file) || is_gzip_signature(file)) {
            file <- gzfile(file, open = "r")
        } else if (is_bz2_suffix(file) || is_bz2_signature(file)) {
            file <- bzfile(file, open = "r")
        } else if (is_xz_suffix(file)) {
            file <- xzfile(file, open = "r")
        }
    }
    if (inherits(file, "connection")) {
        on.exit(close(file))
        readLines(file, n = n)
    } else {
        if (n < 0L) n <- Inf
        data.table::fread(
            file = file, sep = "", header = FALSE,
            colClasses = "character",
            showProgress = FALSE,
            nrows = n
        )[[1L]]
    }
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
