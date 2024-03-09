exec_return <- function(status, id, opath, abort, warn) {
    if (is.null(id)) {
        msg <- "command"
    } else {
        msg <- "command {.field {id}}"
    }
    if (status == 0L) {
        msg <- sprintf("Running %s successfully", msg)
        cli::cli_inform(msg)
    } else {
        # if command run failed, we remove the output
        if (!is.null(opath)) remove_opath(opath)
        msg <- c(
            sprintf("something wrong when running %s", msg),
            i = "error code: {.val {status}}"
        )
        if (abort) {
            cli::cli_abort(msg)
        } else if (warn) {
            cli::cli_warn(msg)
        }
    }
    invisible(status)
}

remove_opath <- function(opath) {
    # remove trailing backslash or slash
    opath <- sub("(\\\\+|/+)$", "", opath, perl = TRUE)
    opath <- opath[file.exists(opath)] # can also check directory
    if (length(opath) == 0L) return(NULL) # styler: off
    failed <- vapply(opath, unlink, integer(1L),
        recursive = TRUE, USE.NAMES = FALSE
    ) != 0L
    if (any(failed)) {
        cli::cli_warn("Cannot remove {.path {opath[failed]}}")
    }
}

build_opath <- function(odir, ofile = NULL, abs = FALSE, call = rlang::caller_env()) {
    assert_string(odir, empty_ok = FALSE, arg = substitute(odir), call = call)
    assert_string(ofile,
        empty_ok = FALSE, null_ok = TRUE,
        arg = substitute(ofile), call = call
    )
    odir <- path_trim(odir)
    dir_create(odir)
    # whether to use absolute path
    if (abs) odir <- normalizePath(odir, winslash = "/", mustWork = TRUE)
    if (!is.null(ofile)) file_path(odir, ofile) else odir
}
