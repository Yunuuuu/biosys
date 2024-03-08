#' Function to create a function to run system command
#'
#' For each algorithm, we'll only build command arguments for required
#' parameters, for optional arguments, we'll provide a `...` argument to pass
#' these parameters.
#'
#' we always use `ofile` and `odir` to specify output file or directory.
#' `opath` is special for `exec_internal` argument, we ofthen build `opath` with
#' `ofile` and `odir`.
#'
#' @param ... Arguments to run command, you can pass `... =` to add dots (which
#' should be optional arguments).
#' @param cmd The argument name used to specify command path.
#' @param opath_internal NULL or a symbol specify the variable passed into
#'  `opath` of `exec_internal`.
#' @param help If not `FALSE`, will create a function with an argument `help`.
#' in which case the help string will be passed into argument to print help
#' document. (Often "--help" or NULL).
#' @param prepare Expression list used to create `required_args`.
#' @importFrom rlang :=
#' @keywords internal
#' @noRd
# running order:
# 1. help (hijack, can skip 2nd and 3rd steps)
# 2. prepare (required_args)
# 3. optional_args
# 4. `exec_internal`
exec_build <- function(name, ..., cmd = name, opath_internal = NULL, help = FALSE, prepare = NULL) {
    # prepare arguments for function ------------------------------
    argv <- list(
        envpath = NULL, envvar = NULL, abort = TRUE,
        stdout = TRUE, stderr = TRUE, stdin = "", wait = TRUE, timeout = 0L,
        verbose = TRUE
    )
    if (!isFALSE(help)) argv <- c(argv, list(help = FALSE))
    # insert `cmd` argument in suitable position
    if (cmd_null_ok <- !is.null(name)) {
        argv <- rlang::exprs(..., !!!argv, !!cmd := NULL) # nolint
    } else {
        argv <- rlang::exprs(cmd = , ..., !!!argv)
        cmd <- "cmd"
    }

    # prepare body for function ----------------------------
    ## prepare `cmd` argument ------------------------------
    # this will insert into `exec_internal` call expression
    cmd_symbol <- rlang::sym(cmd)
    # this should be in the top of function body
    cmd_assert <- rlang::exprs(
        assert_string(!!cmd_symbol, empty_ok = FALSE, null_ok = !!cmd_null_ok)
    )
    ## prepare required arguments --------------------------
    any_required_args <- FALSE
    # `opath` is an argument for `exec_internal`
    if (length(setdiff(...names(), c("...", "opath")))) {
        # if there are some required arguments passed into `cmd`, we should use
        # `prepare` argument to pre-process required argument
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

    ## combining `optional_args` and `required_args` expression ------
    if (any_required_args && any_optional_args) {
        combining_args <- expression(args <- c(required_args, dots))
    } else if (any_required_args) {
        combining_args <- expression(args <- required_args)
    } else if (any_optional_args) {
        combining_args <- expression(args <- dots)
    } else {
        combining_args <- expression(args <- character())
    }

    ## construct `exec_internal` expression to run command -----------
    exec_call <- substitute(
        exec_internal(
            name = name, cmd = cmd, args = args,
            # nolint start
            opath = opath, envpath = envpath, envvar = envvar,
            abort = abort, stdout = stdout, stderr = stderr, stdin = stdin,
            wait = wait, timeout = timeout, verbose = verbose
            # nolint end
        ),
        list(name = name, cmd = cmd_symbol, opath = opath_internal)
    )

    ## prepare `help` expression ---------------------------
    if (!isFALSE(help)) {
        # for some commands, the help document can return an error code
        # so we always use `abort=FALSE` and `warn=FALSE`
        help_exec_call <- rlang::call_modify(exec_call,
            args = help, abort = FALSE, warn = FALSE
        )
        help <- rlang::exprs(assert_bool(help), # styler: off
            if (help) return(!!help_exec_call) # styler: off
        )
    } else {
        help <- NULL
    }

    ## construct function body ----------------------------------
    # running order:
    # 1. cmd_assert
    # 2. help (hijack, can skip 3rd and 4th steps)
    # 3. prepare (required_args)
    # 4. optional_args
    # 5. exec_call
    body <- as.call(c(as.name("{"), c(
        cmd_assert, help,
        prepare, optional_args, combining_args,
        list(exec_call)
    )))

    # construct function ----------------------------------
    rlang::new_function(argv, body = body)
}

#' Invoke a System Command
#'
#' @param cmd Command to be invoked, as a character string.
#' @param ... <[dynamic dots][rlang::dyn-dots]> Arguments passed to `cmd`.
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
exec <- exec_build(NULL, cmd = "cmd", ... = , opath = NULL, opath_internal = quote(opath))

#' Don't provide default value for name, in this way, we must provide name
#' manually for every internal function.
#' @keywords internal
#' @noRd
exec_internal <- function(
    name, cmd = NULL, args = character(),
    opath = NULL, envpath = NULL, envvar = NULL, abort = TRUE, warn = TRUE,
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
    cmd_return(status,
        id = cmd %||% name, opath = opath,
        abort = abort, warn = warn
    )
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
        msg <- "Running command {.field {command}"
        if (length(args)) {
            cli_args <- cli::cli_vec(args, # nolint styler: off
                list("vec-sep" = " ", "vec-last" = " ")
            )
            msg <- paste(msg, "{cli_args}}", sep = " ")
        } else {
            msg <- paste0(msg, "}")
        }
        cli::cli_inform(msg)
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

cmd_return <- function(status, id, opath, abort, warn) {
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
    if (length(opath)) {
        failed <- vapply(opath, unlink, integer(1L),
            recursive = TRUE, USE.NAMES = FALSE
        ) != 0L
        if (any(failed)) {
            cli::cli_warn("Cannot remove {.path {opath[failed]}}")
        }
    }
}
