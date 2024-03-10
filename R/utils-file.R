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

read_lines <- function(file, n = -1L) {
    if (is_gzip_suffix(file) || is_gzip_signature(file)) {
        con <- gzfile(file, open = "r")
    } else if (is_bz2_suffix(file) || is_bz2_signature(file)) {
        con <- bzfile(file, open = "r")
    } else if (is_xz_suffix(file)) {
        con <- xzfile(file, open = "r")
    } else {
        if (n < 0L) n <- Inf
        lines <- data.table::fread(
            file = file, sep = "", header = FALSE,
            colClasses = "character",
            showProgress = FALSE,
            nrows = n
        )[[1L]]
        return(lines)
    }
    on.exit(close(con))
    readLines(con, n = n)
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

uncompress_file <- function(file, exdir = tempdir()) {
    file_info <- file.info(file)
    if (is.na(file_info$size)) {
        cli::cli_abort(sprintf(
            "File %s does not exist or is non-readable.",
            style_file(file)
        ))
    }
    if (isTRUE(file_info$isdir)) {
        cli::cli_abort(sprintf(
            "File %s is a directory. Not yet implemented.",
            style_file(file)
        ))
    }
    # if (!file_info$size) {
    #     cli::cli_warn(sprintf("File %s has size 0", style_file(file)))
    #     return(file)
    # }
    file_signature <- readBin(file, raw(), 8L)

    # support gzip and bz2 files
    if (is_gzip_suffix(file, tar = TRUE)) {
        file <- gunzip(file, odir = exdir, verbose = FALSE)
    } else if (is_gzip_signature(file, file_signature)) {
        # gunzip can only work for specific file extension
        cli::cli_abort(
            "gzip only support 'z', 'gz', 'tgz', and 'taz' extension"
        )
    } else if (is_bz2_suffix(file, tar = TRUE) ||
        is_bz2_signature(file, file_signature)) {
        file <- bunzip2(file, odir = exdir, verbose = FALSE)
    }

    # support zip, and tar files
    if (is_tar(file)) {
        FUN <- utils::untar
    } else if (is_zip(file, file_signature)) {
        FUN <- utils::unzip
    } else {
        return(file)
    }
    fnames <- FUN(file, list = TRUE)
    if (inherits(fnames, "data.frame")) fnames <- fnames[[1L]]
    if (length(fnames) > 1L) {
        cli::cli_abort(c_msg(
            "Compressed files containing more than 1 file",
            "are currently not supported."
        ))
    }
    FUN(file, exdir = exdir)
    file.path(exdir, fnames)
}

# https://github.com/Rdatatable/data.table/blob/15c127e99f8d6aab599c590d4aec346a850f1334/R/fread.R#L90
is_tar <- function(file) endsWith(file, ".tar")

is_gzip_suffix <- function(file, tar = FALSE) {
    if (endsWith(file, ".z") || endsWith(file, ".gz")) {
        return(TRUE)
    } else if (tar && (endsWith(file, ".tgz") || endsWith(file, ".taz"))) {
        return(TRUE)
    }
    FALSE
}

is_gzip_signature <- function(file, file_signature = NULL) {
    match_file_signature(file, file_signature, as.raw(c(0x1F, 0x8B)))
}

is_bz2_suffix <- function(file, tar = FALSE) {
    if (endsWith(file, ".bz2") || endsWith(file, ".bz")) {
        return(TRUE)
    } else if (tar && (endsWith(file, ".tbz2") || endsWith(file, ".tbz"))) {
        return(TRUE)
    }
    FALSE
}

is_bz2_signature <- function(file, file_signature = NULL) {
    match_file_signature(file, file_signature, as.raw(c(0x42, 0x5A, 0x68)))
}

is_xz_suffix <- function(file, tar = FALSE) {
    if (endsWith(file, ".xz") || endsWith(file, ".lzma")) {
        return(TRUE)
    } else if (tar && (endsWith(file, ".txz") || endsWith(file, ".tlz"))) {
        return(TRUE)
    }
    FALSE
}

is_zip <- function(file, file_signature = NULL) {
    endsWith(file, ".zip") ||
        match_file_signature(file, file_signature, charToRaw("PK\x03\x04"))
}

match_file_signature <- function(file, file_signature, match) {
    n <- length(match)
    file_signature <- file_signature %||% readBin(file, raw(), n)
    identical(file_signature[seq_len(n)], match)
}

gunzip <- function(file, ..., ofile = NULL, odir = getwd(), keep = TRUE) {
    if (is.null(ofile)) {
        ofile <- basename(file)
        ofile <- switch(tolower(path_ext(ofile)),
            z = ,
            gz = path_ext_remove(ofile),
            tgz = ,
            taz = path_ext_set(ofile, ext = "tar"),
            paste(ofile, "out", sep = ".")
        )
    }
    gzip(file = file, "-d", ..., ofile = ofile, odir = odir, keep = keep)
}

gzip <- function(file, ..., ofile = NULL, odir = getwd(), keep = TRUE) {
    ofile <- ofile %||% paste0(basename(file), ".gz")
    opath <- build_opath(odir, ofile)
    status <- exec(
        "gzip", "-c",
        ..., file, ">", opath,
        opath = opath
    )
    if (status != 0L) return(status) # styler: off
    if (!keep) file.remove(file)
    opath
}

bunzip2 <- function(file, ..., ofile = NULL, odir = getwd(), keep = TRUE) {
    if (is.null(ofile)) {
        ofile <- basename(file)
        ofile <- switch(tolower(path_ext(ofile)),
            bz2 = ,
            bz = path_ext_remove(ofile),
            tbz2 = ,
            tbz = path_ext_set(ofile, ext = "tar"),
            paste(ofile, "out", sep = ".")
        )
    }
    bzip2(file = file, "-d", ..., ofile = ofile, odir = odir, keep = keep)
}

bzip2 <- function(file, ..., ofile = NULL, odir = getwd(), keep = TRUE) {
    ofile <- ofile %||% paste0(basename(file), ".bz2")
    opath <- build_opath(odir, ofile)
    status <- exec(
        "bzip2", "-c",
        ..., file, ">", opath,
        opath = opath
    )
    if (status != 0L) return(status) # styler: off
    if (!keep) file.remove(file)
    opath
}
