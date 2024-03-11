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
#' @param opath_symbol NULL or a symbol specify the variable passed into
#'  `opath` of `exec_internal`.
#' @param help If not `FALSE`, will create a function with an argument `help`.
#' in which case the help string will be passed into argument to print help
#' document. (Often "--help" or NULL).
#' @param setup_params A list of expression used to create `params`.
#' @importFrom rlang :=
#' @include utils.R
#' @include helper-exec-command.R
#' @include import-standalone-assert.R
#' @include import-standalone-cli.R
#' @include import-standalone-obj-type.R
#' @keywords internal
#' @noRd
exec_build <- function(
    name, ..., cmd = name, opath_symbol = NULL,
    setup_envvar = NULL, help = FALSE, setup_params = NULL, final = NULL) {
    # running order:
    # 1. setup_envvar
    # 2. help (hijack, can skip 3rd and 4th steps)
    # 3. setup_params (params):
    #    usually the required arguments, such as the input files.
    # 4. if ... is an argument (... = ),
    #    usually the optional arguments
    # 5. `exec_internal`
    # 6. final: what to do after run command
    # use `command_new_name()` to pass `name` argument
    assert_s3_class(name, "command_name", null_ok = TRUE)
    # prepare function arguments pairlist --------------------
    argv <- list(
        envpath = NULL, envvar = NULL, abort = TRUE,
        stdout = TRUE, stderr = TRUE, stdin = "", wait = TRUE, timeout = 0L,
        verbose = TRUE
    )
    if (!isFALSE(help)) argv <- c(argv, list(help = FALSE))
    # insert `cmd` argument in suitable position
    if (cmd_null_ok <- !is.null(name)) {
        argv <- exprs(..., !!!argv, !!cmd := NULL) # nolint
    } else {
        argv <- exprs(cmd = , ..., !!!argv)
        cmd <- "cmd"
    }

    # prepare function body --------------------------------
    ## assert arguments before run command -----------------
    # this will be insert into
    # 1. `assert_arguments`
    # 2. `exec_internal(cmd = cmd_symbol)`
    cmd_symbol <- as.symbol(cmd)
    # this should be in the top of function body
    assert_arguments <- exprs({
        rlang::check_dots_unnamed()
        assert_string(!!cmd_symbol, empty_ok = FALSE, null_ok = !!cmd_null_ok)
    })

    ## prepare optional argument (...) ---------------------
    any_optional_args <- FALSE
    if (any(...names() == "...")) {
        optional_args <- exprs({
            # `dots` is the optional arguments passed into function
            dots <- rlang::list2(...)
            if (length(dots) > 0L && any(lengths(dots)) > 1L) {
                cli::cli_abort(
                    "all arguments passed into {.arg ...} must be of length 1"
                )
            }
            dots <- unlist(dots, recursive = FALSE, use.names = FALSE)
        })
        any_optional_args <- TRUE
    } else {
        optional_args <- NULL
    }

    ## combining `dots` and `params` expression ------
    ## params should be in the end
    ## since some commands use the tail argument (un-tagged) as the input files
    if (!is.null(setup_params) && any_optional_args) {
        args <- quote(c(dots, params))
    } else if (!is.null(setup_params)) {
        args <- quote(params)
    } else if (any_optional_args) {
        args <- quote(dots)
    } else {
        args <- quote(character())
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
        list(name = name, cmd = cmd_symbol, args = args, opath = opath_symbol)
    )

    ## prepare `help` expression ---------------------------
    if (!isFALSE(help)) {
        # for some commands, the help document can return an error code
        # so we always use `abort=FALSE` and `warn=FALSE`
        help_exec_call <- rlang::call_modify(exec_call,
            args = help, opath = NULL, abort = FALSE, warn = FALSE, wait = TRUE
        )
        help_exprs <- exprs({
            assert_bool(help) # styler: off
            if (help) return(!!help_exec_call) # styler: off
        })
    } else {
        help_exprs <- NULL
    }

    ## integrate `final` expression ---------------------------
    if (!is.null(final)) {
        exec_exprs <- c(
            list(substitute(status <- call, list(call = exec_call))), # nolint
            final,
            list(quote(return(status)))
        )
    } else {
        exec_exprs <- list(exec_call)
    }
    ## construct function body ----------------------------------
    # running order:
    # 1. assert_arguments
    # 2. help (hijack, can skip 3rd and 4th steps)
    # 3. setup_params (params): usually the input files
    # 4. optional_args
    # 5. exec_call
    body <- as.call(c(as.name("{"), c(
        assert_arguments,
        setup_envvar, help_exprs,
        setup_params, optional_args,
        exec_exprs
    )))

    # construct function ----------------------------------
    rlang::new_function(argv, body = body)
}

#' Don't provide default value for name, in this way, we must provide name
#' manually for every internal function.
#' @keywords internal
#' @noRd
exec_internal <- function(
    name, cmd = NULL, args = character(),
    opath = NULL, envpath = NULL, envvar = NULL, abort = TRUE, warn = TRUE,
    stdout = TRUE, stderr = TRUE, stdin = "", wait = TRUE, timeout = 0L,
    verbose = TRUE) {
    assert_envvar(envvar, null_ok = TRUE)
    assert_bool(abort)
    stdout <- build_io_arg(stdout)
    stderr <- build_io_arg(stderr)
    assert_bool(wait)
    if (abort && !wait) {
        cli::cli_warn("{.arg wait} must be `TRUE` if `abort = TRUE`")
        wait <- TRUE
    }
    assert_bool(verbose)
    if ((!is.null(envvar) || !is.null(envpath)) && verbose) {
        cli::cli_inform("Setting environment variables")
    }
    envvar <- envvar_parse_path(envvar, "PATH", envpath)
    status <- with_envvar(
        envvar = envvar,
        exec_run(
            cmd = cmd, name = name, args = args,
            stdout = stdout, stderr = stderr,
            stdin = stdin, wait = wait,
            timeout = timeout, verbose = verbose
        ),
        action = "replace"
    )
    exec_return(status,
        id = cmd %||% name, opath = opath,
        abort = abort, warn = warn, verbose = verbose
    )
}

# For `stdout` and `stderr`
build_io_arg <- function(x, ..., arg = rlang::caller_arg(x), call = rlang::caller_env()) {
    if (rlang::is_string(x)) {
        if (x == "") {
            cli::cli_abort(
                "{.arg {arg}} cannot be a empty string",
                call = call
            )
        }
        return(x)
    } else if (rlang::is_bool(x)) {
        if (x) return("") else return(FALSE) # styler: off
    } else {
        cli::cli_abort(
            "{.arg {arg}} must be a bool or a string of file path",
            call = call
        )
    }
}

#' @noRd
exec_run <- function(cmd = NULL, name, args = character(), ..., verbose = TRUE) {
    command <- exec_locate_command(cmd = cmd, name = name)
    if (verbose) {
        msg <- c_msg("Running command", style_field(command))
        if (length(args)) {
            msg <- c_msg(msg, style_field(paste(args, collapse = " ")))
        }
        cli::cli_inform(msg)
    }
    sys_args <- c(list(command = command, args = as.character(args)), ...)
    do.call(system2, sys_args)
}
