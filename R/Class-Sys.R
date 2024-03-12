Sys <- R6::R6Class("Sys",
    public = list(
        initialize = function(...) {
            private$validate()
            params <- rlang::list2(...)
            verbose <- extra_params$verbose
            abort <- extra_params$abort
            help <- extra_params$help
            action <- extra_params$action
            params <- params[
                !rlang::names2(params) %in%
                    c("verbose", "abort", "help", "action")
            ]
            # Split up params between
            # `envvar`: params used to define running environment variables
            # `system2`: params used by base::system2 function
            # `cmd`: A string to define the command path
            # `command`: params used by command
            #  1. parameters in R arguments (setup_command_params())
            #  2. passed by `...`
            envvar_params <- params[
                intersect(names(params), formalArgs(self$setup_envvar))
            ]
            system2_params <- params[
                intersect(names(params), setdiff(
                    formalArgs(system3),
                    c("command", "params")
                ))
            ]
            if (!is.null(system2_params$stdout)) check_io(system2_params$stdout)
            if (!is.null(system2_params$stderr)) check_io(system2_params$stderr)
            # there should be always an argument define the command path
            # which has the same name of `self$command`
            cmd <- params[[self$command]]
            # this will be passed into `$setup_params`,
            # and will be used by all `Sys` object internal function
            sys_params <- params[
                intersect(names(params), formalArgs(self$setup_params))
            ]
            # `dots` is usually used to pass the additional arguments for the
            # command, so it shouldn't be named.
            dots <- params[
                setdiff(names(params), c(
                    names(envvar_params),
                    names(system2_params),
                    self$command,
                    names(sys_params)
                ))
            ]
            named <- dots[rlang::have_name(dots)]
            if (any(named)) {
                cli::cli_abort(c(
                    "Arguments in `...` must be passed by position, not name.",
                    `!` = "{paste(names(named), named, sep = '=')}"
                ))
            }
            # setting environment variables ------------
            envvar <- rlang::inject(self$setup_envvar(!!!envvar_params))
            if (length(envvar) > 0L) {
                if (verbose) cli::cli_inform("Setting environment variables")
                old <- set_envvar(as_envvars(envvar), action = action)
                on.exit(set_envvar(old))
            }
            # run command ------------------------------
            private$exec(
                cmd, sys_params, dots, system2_params,
                help = help, abort = abort, verbose = verbose
            )
        },
        # setup_envvar: parameters used to create environment variables
        setup_envvar = function(envpath, envvar) {
            envvar_parse_path(envvar = envvar, "PATH", envpath)
        },
        # The command name: must be a non-empty string
        # should also be the argument name for the associated function
        command = NULL,
        command_locate_from_user = function(cmd) {
            if (file.exists(cmd)) {
                command <- path.expand(cmd)
            } else {
                command <- Sys.which(cmd)
                if (!nzchar(command)) {
                    cli::cli_abort("Cannot locate {.field {cmd}} command")
                }
            }
            command
        },
        command_locate_from_class = function() Sys.which(self$command),
        setup_params = function(...) list(...),
        help_params = function(params) list(),
        command_params = function(params) list(),
        success = function(status, command, params, verbose) NULL,
        error = function(status, command, params, verbose) NULL,
        final = function(status, command, params, verbose) NULL,
    ),
    private = list(
        exec = function(cmd, params, dots, system2_params,
                        help, abort, verbose) {
            if (!is.null(cmd)) {
                command <- self$command_locate_from_user(cmd)
            } else {
                command <- self$command_locate_from_class()
            }
            params <- rlang::inject(self$setup_params(!!!params))
            if (help) {
                system2_params$wait <- TRUE
                status <- rlang::inject(system3(
                    command = command,
                    params = self$help_params(params),
                    !!!system2_params,
                    verbose = verbose
                ))
            } else {
                status <- rlang::inject(system3(
                    command = command,
                    params = c(self$command_params(params), dots),
                    !!!system2_params,
                    verbose = verbose
                ))
                self$final(status, command, params, verbose)
                if (status == 0L) {
                    self$success(status,
                        command = command,
                        params = params, verbose = verbose
                    )
                    if (verbose) {
                        cli::cli_inform(sprintf(
                            "Running command {.field %s} success", command
                        ))
                    }
                } else {
                    self$error(status,
                        command = command,
                        params = params, verbose = verbose
                    )
                    # if command run failed, we remove the output
                    if (!is.null(params$opath)) remove_opath(params$opath)
                    msg <- c(
                        sprintf(
                            "something wrong when running command {.field %s}", command
                        ),
                        i = "error code: {.val {status}}"
                    )
                    if (abort) cli::cli_abort(msg) else cli::cli_warn(msg)
                }
            }
            status
        },
        validate = function() {
            if (is.null(self$command)) {
                if (fclass(self) == "Sys") {
                    cli::cli_abort("You cannot use {.cls Sys} object directly")
                } else {
                    cli::cli_abort(
                        "You should define the {.field self$command} field"
                    )
                }
            } else if (!rlang::is_string(self$command)) {
                cli::cli_abort("{.field self$command} must be a string")
            } else if (self$command == "") {
                cli::cli_abort("{.field self$command} cannot be a empty string")
            }
        }
    )
)

system3 <- function(command, params,
                    stdout = TRUE, stderr = TRUE, stdin = "",
                    wait = TRUE, timeout = 0L,
                    verbose) {
    command_params <- unlist(params, FALSE, FALSE)
    if (verbose) {
        msg <- c_msg("Running command", style_field(command))
        if (length(command_params)) {
            msg <- c_msg(
                msg,
                style_field(paste(command_params, collapse = " "))
            )
        }
        cli::cli_inform(msg)
    }
    stdout <- standardize_io(stdout)
    stderr <- standardize_io(stderr)
    system2(
        command = command, args = command_params,
        stdout = stdout, stderr = stderr,
        stdin = stdin, wait = wait,
        timeout = timeout
    )
}

standardize_io <- function(io) {
    if (rlang::is_string(io)) return(io) # styler: off
    if (io) return("") else return(FALSE) # styler: off
}

# For `stdout` and `stderr`
check_io <- function(x, arg = rlang::caller_arg(x), call = rlang::caller_env()) {
    if (rlang::is_string(x)) {
        if (x == "") {
            cli::cli_abort(
                "{.arg {arg}} cannot be an empty string",
                call = call
            )
        }
    } else if (!rlang::is_bool(x)) {
        cli::cli_abort(
            "{.arg {arg}} must be a bool or a string of file path",
            call = call
        )
    }
}

remove_opath <- function(opath) {
    # remove trailing backslash or slash
    opath <- sub("(\\\\+|/+)$", "", opath, perl = TRUE)
    opath <- opath[file.exists(opath)] # can also check directory
    if (length(opath) == 0L) return(NULL) # styler: off
    failed <- vapply(opath, unlink, integer(1L),
        recursive = TRUE, USE.NAMES = FALSE
    ) != 0L
    if (any(failed)) cli::cli_warn("Cannot remove {.path {opath[failed]}}")
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
