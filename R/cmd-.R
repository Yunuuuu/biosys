#' Function factory to create function to run system command
#'
#' For each algorithm, we'll only build command arguments for required
#' parameters, for optional arguments, we'll provide a `...` argument to pass
#' these parameters.
#'
#' @param ... Required arguments to run command
#' @param cmd The argument name used to specify command path.
#' @importFrom rlang :=
#' @keywords internal
#' @noRd
exec_fn <- function(name, ..., cmd = name, oopath = NULL, before = NULL, after = NULL) {
    argv <- list(
        envpath = NULL, envvar = NULL, abort = TRUE,
        stdout = TRUE, stderr = TRUE, stdin = "", wait = TRUE, timeout = 0L,
        verbose = TRUE
    )
    # prepare arguments for function ------------------------------
    # insert `cmd` argument in suitable position
    if (cmd_null_ok <- !is.null(name)) {
        argv <- rlang::exprs(..., !!!argv, !!cmd := NULL) # nolint
    } else {
        argv <- rlang::exprs(cmd = , ..., !!!argv)
        cmd <- "cmd"
    }
    # prepare body for function ---------------------------
    ## prepare `cmd` argument ------------------------------
    cmd_symbol <- rlang::sym(cmd)
    cmd_assert <- rlang::exprs(
        assert_string(!!cmd_symbol, empty_ok = FALSE, null_ok = !!cmd_null_ok)
    )
    ## prepare required arguments --------------------------
    any_required_args <- FALSE
    if (length(setdiff(...names(), c("...", "opath", "ofile", "odir")))) {
        # if there are some required arguments passed into `cmd`, we should use
        # `before` argument to pre-process required argument
        any_required_args <- TRUE
    }

    ## prepare optional argument ---------------------------
    any_optional_args <- FALSE
    if (any(...names() == "...")) {
        optional_args <- expression(
            # `dots` is the optional arguments passed into function
            dots <- rlang::list2(...),
            if (length(dots) > 0L && any(lengths(dots)) > 1L) {
                cli::cli_abort(
                    "all arguments passed into {.arg ...} must be of length 1"
                )
            },
            dots <- unlist(dots, recursive = FALSE, use.names = FALSE)
        )
        any_optional_args <- TRUE
    } else {
        optional_args <- NULL
    }

    ## construct `arg` expression ---------------------------------
    if (any_required_args && any_optional_args) {
        args <- quote(c(required_args, dots))
    } else if (any_required_args) {
        args <- quote(required_args)
    } else if (any_optional_args) {
        args <- quote(dots)
    } else {
        args <- quote(character())
    }
    ## construct function body ----------------------------------
    body <- substitute(
        exec_internal(
            name = name, cmd = cmd_symbol, args = args,
            # nolint start
            opath = oopath, envpath = envpath, envvar = envvar, abort = abort,
            stdout = stdout, stderr = stderr, stdin = stdin, wait = wait,
            timeout = timeout, verbose = verbose
            # nolint end
        ),
        list(name = name, cmd_symbol = cmd_symbol, args = args, oopath = oopath)
    )
    body <- c(optional_args, list(body))
    if (!is.null(before)) body <- c(before, body)
    if (!is.null(after)) body <- c(body, after)
    # construct function ----------------------------------
    body <- as.call(c(as.name("{"), c(cmd_assert, body)))
    rlang::new_function(argv, body = body)
}

#' Invoke a System Command
#'
#' @param cmd Command to be invoked, as a character string.
#' @param ... Arguments passed to `cmd`.
#' @param envpath A character define the environment variables `PATH` to be
#'  added before running command.
#' @param envvar A named atomic vector define running environment variables of
#' the command.
#' @param opath Specifying the output file or directory to be removed if
#'  command running error. Note: all files in the directory will be removed, you
#'  must use this argument carefully.
#' @param abort A bool indicates whether to report error if command return
#'  non-zero status.
#' @param stdout,stderr Where output to ‘stdout’ or ‘stderr’ should be sent.
#' Possible values are:
#'
#'  - `TRUE`: print child output in R console
#'  - `FALSE`: suppress output stream
#'  - **string**: name or path of file to redirect output
#'
#' @inheritParams base::system2
#' @param wait A bool (not NA) indicating whether the R interpreter should wait
#' for the command to finish, or run it asynchronously. This will be ignored
#' (and the interpreter will always wait) if `abort = TRUE`.
#' @param verbose A logical value indicates whether to print running command
#'  message.
#' @return
#'  - if `abort=TRUE`, zero if command success, otherwise, abort error.
#'  - if `abort=FALSE` and `wait=FALSE`, always return `0`.
#'  - if `abort=FALSE` and `wait=TRUE`, exit status returned by the command.
#' @export
exec <- exec_fn(NULL, cmd = "cmd", ... = , opath = NULL)

#' Don't provide default value for name, in this way, we must provide name
#' manually for every internal function.
#' @keywords internal
#' @noRd
exec_internal <- function(
    name, cmd = NULL, args = character(),
    opath = NULL, envpath = NULL, envvar = NULL, abort = TRUE,
    stdout = TRUE, stderr = TRUE, stdin = "", wait = TRUE, timeout = 0L,
    verbose = TRUE) {
    assert_(
        envvar, function(x) is.atomic(x) && rlang::is_named2(x),
        "named {.cls atomic}",
        null_ok = TRUE
    )
    assert_bool(abort)
    stdout <- build_io_arg(stdout)
    stderr <- build_io_arg(stderr)
    assert_bool(wait)
    if (abort && !wait) {
        cli::cli_warn("{.arg wait} must be `TRUE` if `abort = TRUE`")
        wait <- TRUE
    }
    assert_bool(verbose)
    arg_internals <- list( # nolint
        stdout = stdout,
        stderr = stderr,
        stdin = stdin,
        wait = wait,
        timeout = timeout
    )
    run <- quote(cmd_run(
        cmd = cmd, name = name, args = args,
        arg_internals = arg_internals, verbose = verbose
    ))
    envvar <- cmd_envvar(envpath, envvar)
    if (!is.null(envvar)) {
        if (verbose) cli::cli_inform("Setting environment variables")
        status <- with_envvar(envvar = envvar, eval(run), action = "replace")
    } else {
        status <- eval(run)
    }
    cmd_return(status, id = cmd %||% name, opath = opath, abort = abort)
}

build_io_arg <- function(x, ..., arg = rlang::caller_arg(x), call = rlang::caller_env()) {
    if (rlang::is_string(x)) {
        if (x == "") cli::cli_abort("{.arg {arg}} cannot be a empty string")
        return(x)
    } else if (rlang::is_bool(x)) {
        if (x) return("") else return(FALSE) # styler: off
    } else {
        cli::cli_abort("{.arg {arg}} must be a bool or a string of file path")
    }
}

cmd_envvar <- function(envpath, envvar) {
    if (!is.null(envvar) && any(names(envvar) == "PATH")) {
        cli::cli_warn(c(
            "!" = "{.field PATH} in {.arg env} won't work",
            "i" = "Please use {.arg envpath} argument directly."
        ))
        envvar <- envvar[names(envvar) != "PATH"]
    }
    if (!is.null(envpath)) envvar <- c(envvar, PATH = envpath_add(envpath))
    envvar
}

#' @noRd
cmd_run <- function(cmd = NULL, name, args = character(), arg_internals = list(), verbose = TRUE) {
    assert_string(cmd, null_ok = !is.null(name))
    command <- cmd_locate(cmd = cmd, name = name)
    if (verbose) {
        cli_args <- cli::cli_vec(args, # nolint styler: off
            list("vec-sep" = " ", "vec-last" = " ")
        )
        cli::cli_inform("Running command {.field {command} {cli_args}}")
    }
    arg_internals <- c(list(command = command, args = as.character(args)), arg_internals)
    do.call(system2, arg_internals)
}

cmd_locate <- function(cmd = NULL, name) {
    if (!is.null(cmd)) {
        if (!file.exists(cmd)) {
            command <- Sys.which(cmd)
            if (!nzchar(command)) {
                cli::cli_abort("Cannot locate {.field {cmd}} command")
            }
        } else {
            command <- path.expand(cmd)
        }
    } else if (!is.null(name)) {
        command <- Sys.which(name)
        if (!nzchar(command)) {
            cli::cli_abort("Cannot locate {.field {name}} command")
        }
    } else {
        cli::cli_abort("{.arg cmd} or {.arg name} must be provided")
    }
    command
}

cmd_return <- function(status, id = NULL, opath = NULL, abort = FALSE) {
    if (is.null(id)) {
        msg <- "command"
    } else {
        msg <- "command {.filed {id}}"
    }
    if (status == 0L) {
        msg <- sprintf("Running %s successfully", msg)
        cli::cli_inform(msg)
    } else {
        # if command run failed, we remove the output
        msg <- sprintf("Something wrong when running %s", msg)
        remove_opath(opath)
        if (abort) cli::cli_abort(msg) else cli::cli_warn(msg)
    }
    invisible(status)
}

remove_opath <- function(opath) {
    if (!is.null(opath)) {
        # remove trailing backslash or slash
        opath <- sub("(\\\\+|/+)$", "", opath, perl = TRUE)
        opath <- opath[file.exists(opath)] # can also check directory
        if (length(opath)) {
            failed <- vapply(opath, unlink, integer(1L),
                recursive = TRUE, USE.NAMES = FALSE
            ) != 0L
            if (any(failed)) {
                cli::cli_warn("Cannot remove {.path {opath[failed]}}")
            }
        }
    }
}
