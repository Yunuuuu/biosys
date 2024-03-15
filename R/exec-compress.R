#' Python is a programming language that lets you work quickly and integrate
#' systems more effectively.
#'
#' @param cmd One of `gz`, `gzip`, `pigz`, `bzip2`, or `xz`.
#' @param file A string of input file path.
#' @param ... `r rd_dots("compress")`. Details see: `compress(cmd, help =
#' TRUE)`.
#' @param keep A bool, indicates whether keep original file.
#' @param override A bool, indicates whether to override ouput file if it
#' exists.
#' @inheritParams allele_counter
#' @seealso
#' - <https://www.gnu.org/software/gzip/>
#' - <https://zlib.net/pigz/>
#' - <https://sourceware.org/bzip2/>
#' - <https://tukaani.org/xz/>
#' @export
compress <- function(cmd, file, ...,
                     ofile = NULL, odir = getwd(), keep = TRUE,
                     override = FALSE,
                     envpath = NULL, envvar = NULL, help = FALSE,
                     stdout = TRUE, stderr = TRUE, stdin = "",
                     wait = TRUE, timeout = 0L, abort = TRUE,
                     verbose = TRUE) {
    cmd <- match.arg(cmd, c("gz", "pigz", "gzip", "bzip2", "xz"))
    if (cmd == "gz") {
        Sys <- SysGz$new()
    } else {
        Sys <- SysCompress$new(name = cmd)
    }
    Sys$exec(
        cmd = NULL, ..., file = file,
        ofile = ofile, odir = odir, keep = keep, override = override,
        envpath = envpath, envvar = envvar,
        help = help, stdout = stdout, stderr = stderr, stdin = stdin,
        wait = wait, timeout = timeout, abort = abort, verbose = verbose
    )
}

SysCompress <- R6::R6Class(
    "SysCompress",
    inherit = SysName,
    private = list(
        setup_command_params = function(file, ofile, odir, keep, override) {
            assert_string(file)
            assert_bool(keep)
            assert_bool(override)
            if (is.null(ofile)) {
                ext <- switch(private$name,
                    gz = ,
                    pigz = ,
                    gzip = "gz",
                    bzip2 = "bz2",
                    xz = "xz"
                )
                ofile <- paste(basename(file), ext, sep = ".")
            }
            opath <- build_opath(odir, ofile)
            if (!override && file.exists(opath)) {
                cli::cli_abort(c(
                    "Cannot (un)compress {.path {file}} into {.path {opath}}",
                    i = "{.path {opath}} exists"
                ))
            }
            private$insert_param("opath", opath)
            c("-c", file, ">", opath)
        },
        setup_help_params = function() "--help",
        success = function(status, command, verbose) {
            if (!private$get_param("keep")) {
                file.remove(private$get_param("file"))
            }
        },
        return = function(status, command, verbose) {
            private$get_param("opath")
        }
    )
)

SysGz <- R6::R6Class(
    "SysGz",
    inherit = SysCompress,
    private = list(
        name = "gz",
        command_locate_by_name = function() {
            gzip <- Sys.which("gzip")
            pigz <- Sys.which("pigz")
            if (nzchar(pigz)) pigz else gzip
        }
    )
)

#########################################################
#' @export
#' @rdname compress
decompress <- function(cmd, file, ..., ofile = NULL) {
    if (is.null(ofile)) {
        ofile <- basename(file)
        ext <- tolower(path_ext(ofile))
        if (cmd == "bzip2") {
            ofile <- switch(ext,
                bz2 = ,
                bz = path_ext_remove(ofile),
                tbz2 = ,
                tbz = path_ext_set(ofile, ext = "tar"),
                paste(ofile, "out", sep = ".")
            )
        } else if (cmd == "gzip" || cmd == "gz" || cmd == "pigz") {
            ofile <- switch(ext,
                z = ,
                gz = path_ext_remove(ofile),
                tgz = ,
                taz = path_ext_set(ofile, ext = "tar"),
                paste(ofile, "out", sep = ".")
            )
        } else {
            ofile <- switch(ext,
                xz = ,
                lzma = path_ext_remove(ofile),
                txz = ,
                tlz = path_ext_set(ofile, ext = "tar"),
                paste(ofile, "out", sep = ".")
            )
        }
    }
    compress(cmd = cmd, file = file, "-d", ..., ofile = ofile)
}

decompress_file <- function(file, exdir = tempdir()) {
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

    file_signature <- readBin(file, raw(), 8L)
    # support gzip and bz2 files
    if (is_gzip_suffix(file, tar = TRUE) ||
        is_gzip_signature(file, file_signature)) {
        file <- decompress("gz", file, odir = exdir, verbose = FALSE)
    } else if (is_bz2_suffix(file, tar = TRUE) ||
        is_bz2_signature(file, file_signature)) {
        file <- decompress("bzip2", file, odir = exdir, verbose = FALSE)
    } else if (is_xz_suffix(file, tar = TRUE)) {
        file <- decompress("xz", file, odir = exdir, verbose = FALSE)
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
