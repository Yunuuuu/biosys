Sys <- R6::R6Class("Sys",
    public = list(
        initialize = function(...) {
            on.exit(rm(self))
            private$validate()
            params <- rlang::list2(...)
            verbose <- params$verbose
            abort <- params$abort
            help <- params$help
            params <- params[!names(params) %in% c("verbose", "abort", "help")]
            # Split up params between
            # `envvar`: params used to define running environment variables
            # `system2`: params used by base::system2 function
            # `cmd`: A string to define the command path
            # `command` include:
            #  1. parameters in R arguments (setup_params())
            #  2. passed by `...`
            envvar_params <- params[
                intersect(names(params), formalArgs(self$setup_envvar))
            ]
            system2_params <- params[
                intersect(
                    names(params),
                    setdiff(
                        formalArgs(self$run_command),
                        c("command", "params")
                    )
                )
            ]
            cmd <- params[[self$command]]
            command_params_in_r <- params[
                intersect(names(params), formalArgs(self$setup_command_params))
            ]
            command_params_in_dots <- params[
                setdiff(names(params), c(
                    names(envvar_params),
                    names(system2_params),
                    self$command,
                    names(command_params_in_r)
                ))
            ]
            envvar <- rlang::inject(self$setup_envvar(!!!envvar_params))
            with_envvar(envvar, self$exec(
                cmd,
                command_params_in_r,
                command_params_in_dots,
                system2_params,
                help = help,
                abort = abort,
                verbose = verbose
            ))
        },
        # setup_envvar: parameters used to create environment variables
        setup_envvar = function(envvar, envpath, verbose = TRUE) {
            if (verbose && (!is.null(envvar) || !is.null(envpath))) {
                cli::cli_inform("Setting environment variables")
            }
            envvar_parse_path(envvar = envvar, "PATH", envpath)
        },
        # The command name: must be a non-empty string
        # should also be the argument name for the associated function
        command = NULL, help = NULL,
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
        # `dots` is usually used to pass the optional arguments for the
        # command, must always return a list.
        setup_command_params = function(...) list(...),
        setup_help_params = function() list(self$help),
        success = function(status, command, params, verbose) {
            msg <- sprintf("Running command {.field %s} success", command)
            cli::cli_inform(msg)
        },
        setup_opath = function(params) NULL,
        error = function(status, command, params, abort, verbose) {
            opath <- self$setup_opath(params)
            # if command run failed, we remove the output
            if (!is.null(opath)) {
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
            msg <- c(
                sprintf("something wrong when running %s", msg),
                i = "error code: {.val {status}}"
            )
            if (abort) cli::cli_abort(msg) else cli::cli_warn(msg)
        },
        final = function(status, opath, params, verbose) NULL
    ),
    private = list(
        # sub-class shouldn't touch the private fields
        # instead, we should always override public fields
        validate = function() {
            if (is.null(self$command)) {
                cli::cli_abort(c_msg(
                    "You cannot use {.cls Sys} object directly",
                    "or you should define the {.field self$command} field"
                ))
            } else if (!rlang::is_string(self$command)) {
                cli::cli_abort("{.field self$command} must be a string")
            } else if (self$command == "") {
                cli::cli_abort("{.field self$command} cannot be a empty string")
            }
        },
        exec = function(cmd,
                        command_params_in_r,
                        command_params_in_dots,
                        system2_params,
                        help, abort, verbose) {
            command <- private$command_locate(cmd)
            if (help) {
                system2_params$wait <- TRUE
                status <- rlang::inject(private$run_command(
                    command = command,
                    params = self$setup_help_params(),
                    !!!system2_params,
                    verbose = verbose
                ))
            } else {
                params <- rlang::inject(
                    self$setup_command_params(!!!command_params_in_r)
                )
                params <- c(params, command_params_in_dots)
                status <- rlang::inject(private$run_command(
                    command = command,
                    params = params,
                    !!!system2_params,
                    verbose = verbose
                ))
                if (status == 0L) {
                    self$success(
                        status,
                        command = command,
                        params = command_params_in_r,
                        verbose = verbose
                    )
                } else {
                    self$error(status,
                        command = command,
                        params = command_params_in_r,
                        verbose = verbose,
                        abort = abort
                    )
                }
                self$final(status, command, command_params_in_r, verbose)
            }
            status
        },
        command_locate = function(cmd = NULL) {
            if (!is.null(cmd)) {
                self$command_locate_from_user(cmd)
            } else {
                self$command_locate_from_class()
            }
        },
        run_command = function(command, params,
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
    )
)

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
