#' Invoke a System Command
#'
#' @param cmd Command to be invoked, as a character string.
#' @param ... `r rd_dots("cmd")`.
#' @return A [Execute] object.
#' @export
exec <- function(cmd, ...) {
    assert_string(cmd, empty_ok = FALSE)
    Execute$new(Command$new(cmd = cmd, ...))
}

#' R6 Class to prepare command parameters
#'
#' @export
Command <- R6::R6Class("Command",
    public = list(

        #' @description
        #' Create a new `Command` object. User shouln't call this function
        #' directly.
        #' @param ... Additional argument passed into command.
        initialize = function(...) {
            # collect all parameters, we cannot evaluate it since if we want to
            # print help document, it's much possible there were some missing
            # argument we only evaluate necessary parameters
            params <- rlang::enquos(..., .ignore_empty = "all")

            # Extract params used by `Command` object internal
            # `command_locate`: params used to locate command path.
            # `setup_command_params`: params used by regular command
            # `combine_params`: combine combine dots and params or other global
            # params (both help and regular params)
            param_names <- private$parameters()

            # there were usually additional arguments passed into command by
            # `...`, they must be un-named
            dots <- params[!rlang::names2(params) %in% param_names]
            if (.subset2(private, "collect_dots")) {
                # we collect and check dots
                named <- dots[rlang::have_name(dots)]
                if (length(named)) {
                    cli::cli_abort(
                        "Unknown parameter{?s}: {.arg {names(named)}}"
                    )
                }
                private$dots <- dots
            } else if (length(dots)) {
                if (rlang::is_named(dots)) {
                    note <- paste(
                        "Did you misname argument{?s}",
                        "({.arg {names(dots)}})?"
                    )
                } else {
                    note <- "Did you forget to name an argument?"
                }
                cli::cli_abort(c("`...` must be empty", i = note))
            }

            # here: we check if all necessary parameters have been provided by
            #       external function. (in case from myself missing provide the
            #       parameters in external function)
            missing <- setdiff(param_names, names(params))
            if (length(missing)) {
                cli::cli_abort("Missing parameters: {.arg {missing}}")
            }
            private$params <- params[intersect(names(params), param_names)]
        },

        #' @description Build parameters to run command.
        #' @param help A bool, indicates whether to build parameters for help
        #' document a not.
        #' @param envir An environment used to Execute command.
        build = function(help = FALSE, envir = NULL) {
            private$environment <- envir
            necessary_params <- .subset(
                .subset2(private, "params"),
                private$parameters(help = help)
            )

            # weevaluate params since all params have been defused
            necessary_params <- lapply(necessary_params, rlang::eval_tidy)

            # locate command path ------------------------------
            command <- do_call(
                .subset2(private, "command_locate"),
                necessary_params, "command_locate"
            )
            if (is.null(command) || !nzchar(command)) {
                cli::cli_abort("cannot locate command {.field {private$name}}")
            }

            # run command ------------------------------
            if (help) {
                private$.params <- build_command_params(private$help)
            } else {
                # compute command params
                private$.params <- build_command_params(do_call(
                    .subset2(private, "setup_command_params"),
                    necessary_params,
                    "setup_command_params"
                ))

                private$.dots <- build_command_params(
                    lapply(.subset2(private, "dots"), rlang::eval_tidy)
                )
            }
            command_params <- do_call(
                .subset2(private, "combine_params"),
                necessary_params,
                "combine_params"
            )
            c(command, command_params)
        }
    ),
    private = list(
        name = NULL,
        alias = NULL,
        help = NULL,

        # @field environment An environment used to Execute command which
        # should be the function environment of `private$exec_command2`.
        environment = NULL,

        # @field params A list of parameters used by command.
        dots = list(),

        # @field params A list of parameters used by `Command` object methods.
        params = list(),

        # These two fields carry state throughout rendering but will always be
        # calculated before use
        .dots = character(), .params = character(),

        # @description Used to attach an expression to be evaluated when
        # exiting `private$exec_command2`.
        setup_exit = function(expr, after = TRUE, add = TRUE) {
            on_exit(!!rlang::enquo(expr),
                add = add, after = after,
                envir = private$environment
            )
        },

        # @description Extract parameters used by this object.
        # @return A character of the used parameter of this object.
        parameters = function(help = FALSE) {
            argv <- c(
                rlang::fn_fmls_names(.subset2(private, "command_locate")),
                rlang::fn_fmls_names(.subset2(private, "combine_params"))
            )
            if (!help) {
                argv <- c(argv, rlang::fn_fmls_names(.subset2(
                    private, "setup_command_params"
                )))
            }
            setdiff(argv, c(.subset2(private, "internal_params")))
        },

        # methods to insert into or get parameters from `private$params`
        insert_param = function(name, value) {
            private$params[[name]] <- value
            invisible(self)
        },
        get_param = function(name) {
            .subset2(.subset2(private, "params"), name)
        },

        ##############################################################
        # Following fields or methods should be overrided by sub-class.
        # @field collect_dots A bool indicates whether `...` should be
        # collected passed into command
        collect_dots = TRUE,

        # @field internal_params Additional parameters used by `Command` object
        # but
        #' shouldn't collected from user input.
        internal_params = NULL,

        # @description Method used to locate command
        #
        # @return An string of command path
        command_locate = function(cmd) {
            if (is.null(cmd)) {
                commands <- c(
                    .subset2(private, "name"),
                    .subset2(private, "alias")
                )
                for (cmd in commands) {
                    if (nzchar(command <- Sys.which(cmd))) {
                        break
                    }
                }
                if (!nzchar(command)) {
                    cli::cli_abort(sprintf(
                        "Cannot locate %s command",
                        oxford_comma(style_field(commands), final = "or")
                    ))
                }
            } else {
                command <- Sys.which(cmd)
            }
            command
        },

        # Method used to prepare parameters to run regular command
        #
        # @return An atomic character, or `NULL`.
        setup_command_params = function() NULL,

        # Method used to combine `dots` and `params`
        #
        # @return An atomic character, or `NULL`.
        combine_params = function() {
            c(.subset2(private, ".dots"), .subset2(private, ".params"))
        }
    )
)

#' R6 Class to invoke a series of system command
#'
#' @export
Execute <- R6::R6Class(
    "Execute",
    public = list(

        #' @description
        #' Create a new `Execute` object. User shouln't call this function
        #' directly, just call other command function which should return a
        #' `Execute` object.
        #' @param command A [Command] object.
        #' @return A new `Execute` object.
        initialize = function(command) private$.commands <- list(command),

        #' @description Use `|` to connect two command
        #' @param command A [Command] or `Execute` object to stream `stdout`
        #' into. Note: If `command` is a `Execute` object, the associated
        #' environment variables and working directory will be loss.
        #' @return The `Execute` object self, with command list updated.
        pipe = function(command) {
            msg <- paste(
                "{.arg command} must be a {.cls Command}",
                "or {.cls Execute} object"
            )
            if (R6::is.R6(command)) {
                if (inherits(command, "Command")) {
                    private$.commands <- c(
                        .subset2(private, ".commands"),
                        list(command)
                    )
                } else if (inherits(command, "Execute")) {
                    private$.commands <- c(
                        .subset2(private, ".commands"),
                        command$commands
                    )
                } else {
                    cli::cli_abort(msg)
                }
            } else {
                cli::cli_abort(msg)
            }
            invisible(self)
        },

        #' @description Print the help document for this command.
        #' @param verbose `r rd_verbose()`.
        #' @return Exit status.
        help = function(verbose = TRUE) {
            assert_bool(verbose)
            if (length(.subset2(self, "commands")) > 1L) {
                cli::cli_abort("Cannot get help document for multiple commands")
            }
            status <- private$exec_command(
                help = TRUE,
                stdout = TRUE, stderr = TRUE, stdin = "",
                timeout = 0L, verbose = verbose
            )
            # always return status for help = TRUE
            invisible(status)
        },

        #' @description Execute command
        #' @param stdout,stderr Where output to ‘stdout’ or ‘stderr’ should be
        #' sent. Possible values are:
        #'
        #'  - `TRUE`: print child output in R console
        #'  - `FALSE`: suppress output stream
        #'  - **string**: name or path of file to redirect output
        #'
        #' @param stdin should input be diverted? `""` means the default,
        #' alternatively a character string naming a file. Ignored if input is
        #' supplied.
        #' @param timeout Timeout in seconds, ignored if 0. This is a limit for
        #' the elapsed time running command in a separate process. Fractions of
        #' seconds are ignored.
        #' @param verbose `r rd_verbose()`.
        #' @return Exit status.
        run = function(stdout = TRUE, stderr = TRUE, stdin = "",
                       timeout = 0L, verbose = TRUE) {
            check_io(stdout)
            check_io(stderr)
            assert_bool(verbose)
            status <- private$exec_command(
                help = FALSE,
                stdout = stdout, stderr = stderr,
                stdin = stdin, timeout = timeout,
                verbose = verbose
            )
            # let last `Command` object to define the output
            invisible(status)
        },

        #' @description Method used to change working directory
        #' @param wd A string or NULL define the working directory of the
        #' command.
        #' @return The `Execute` object self, with working directory updated.
        setup_wd = function(wd) {
            assert_string(wd, empty_ok = FALSE, null_ok = TRUE)
            private$wd <- wd
            invisible(self)
        },

        #' @description Method used to create environment variables
        #' @param ... Named character define the environment variables.
        #' @param action Should new values "replace", "prefix" or "suffix"
        #' existing variables with the same name?
        #' @param sep A string to separate new and old value.
        #' @return The `Execute` object self, with running environment variable
        #' updated.
        setup_envvar = function(..., action = "replace", sep = " ") {
            dots <- rlang::dots_list(..., .ignore_empty = "all")
            if (!rlang::is_named2(dots)) {
                cli::cli_abort("All elements in {.arg ...} must be named")
            }
            dots[vapply(dots, is.null, logical(1L))] <- NA_character_
            if (any(lengths(dots) != 1L)) {
                cli::cli_abort(paste(
                    "all value in {.arg ...} must be of length 1",
                    "or {.val NULL}"
                ))
            }
            action <- match.arg(action, c("replace", "prefix", "suffix"))
            for (nm in names(dots)) {
                private$envvar[[nm]] <- parse_envvar(
                    name = nm, new = dots[[nm]],
                    old = private$envvar[[nm]],
                    action = action,
                    sep = sep
                )
            }
            invisible(self)
        },

        #' @description Method used to create PATH environment character
        #' @param ... Unnamed character to define the `PATH-like` environment
        #' variables `name`.
        #' @param action Should new values "replace", "prefix" or "suffix"
        #' existing variables with the same name?
        #' @param name A string define the PATH environment variable name. YOu
        #' can use this to define other `PATH-like` environment variable such as
        #' `PYTHONPATH`.
        #' @return The `Execute` object self, with running environment variable
        #' `name` updated.
        setup_envpath = function(..., action = "prefix", name = "PATH") {
            rlang::check_dots_unnamed()
            assert_string(name, empty_ok = FALSE)
            envpath <- rlang::dots_list(..., .ignore_empty = "all")
            envpath <- unlist(envpath, use.names = FALSE)
            if (anyNA(envpath)) {
                if (length(envpath) > 1L) {
                    cli::cli_warn(paste(
                        "Found {.val NA}",
                        "other {.field envpath} will be ignored",
                        sep = ", "
                    ))
                }
                envpath <- NA_character_
            } else {
                envpath <- as.character(envpath)
                envpath <- normalizePath(envpath, "/", mustWork = FALSE)
                envpath <- paste0(envpath, collapse = .Platform$path.sep)
            }
            self$setup_envvar(
                !!name := envpath,
                action = action,
                sep = .Platform$path.sep
            )
        }
    ),
    private = list(
        envvar = list(),

        # @field A string of the working directory.
        wd = NULL,

        # @field commands A list of [Command] object.
        .commands = list(),

        # @description Used to prepare command environment including
        # working directory, and environment variables, then this method call
        # `private$exec_command2` to invoke a system command.
        exec_command = function(help, stdout, stderr, stdin, timeout, verbose) {
            # setting environment variables -------------
            if (length(.subset2(private, "envvar")) > 0L) {
                if (verbose) {
                    cli::cli_inform(
                        "Setting environment variables: {names(private$envvar)}"
                    )
                }
                old <- as.list(Sys.getenv(names(.subset2(private, "envvar")),
                    names = TRUE, unset = NA_character_
                ))
                on.exit(set_envvar(old), add = TRUE)
                set_envvar(.subset2(private, "envvar"))
            }
            private$exec_command2(
                help = help, stdout = stdout, stderr = stderr, stdin = stdin,
                timeout = timeout, verbose = verbose
            )
        },

        # @description The exact method to execute a system command.
        exec_command2 = function(help, ...) {
            # save current environment -------------------------
            # `setup_exit` in Command object will push expression into this
            # environment
            envir <- environment()

            # use Command object to prepare commnad parameters -----
            commands <- .subset2(self, "commands")
            params <- lapply(commands, function(command) {
                command$build(help = help, envir = envir)
            })

            # combine command parameters -----------------------
            params <- Reduce(function(x, y) c(x, "|", y), params)

            # run command ---------------------------------------
            system3(
                command = params[1L], command_params = params[-1L],
                wd = .subset2(private, "wd"), ...
            )
        }
    ),
    active = list(

        #' @field commands A list of [Command] object.
        commands = function(value) {
            if (missing(value)) {
                .subset2(private, ".commands")
            } else {
                cli::cli_abort("Cannot modify {.field .commands}")
            }
        }
    )
)

set_envvar <- function(envs) {
    unset <- vapply(envs, is.na, logical(1L))
    if (any(!unset)) {
        do.call("Sys.setenv", envs[!unset])
    }
    if (any(unset)) {
        Sys.unsetenv(names(envs)[unset])
    }
}

parse_envvar <- function(name, new, old, action, sep) {
    if (is.null(old)) {
        old <- Sys.getenv(name, unset = NA_character_, names = FALSE)
    }
    if (!is.na(new) && !is.na(old)) {
        if (action == "prefix") {
            new <- paste(new, old, sep = sep)
        } else if (action == "suffix") {
            new <- paste(old, new, sep = sep)
        }
    }
    new
}

do_call <- function(fn, params, name) {
    args <- rlang::fn_fmls_names(fn)
    params <- params[intersect(args, names(params))]
    # we rebuild function name,
    # in this way, error message can indicates the call
    assign(name, value = fn)
    do.call(name, params)
}

system3 <- function(command, command_params, wd = NULL,
                    stdout = TRUE, stderr = TRUE, stdin = "",
                    wait = TRUE, timeout = 0L,
                    verbose) {
    # set working directory ---------------------
    if (!is.null(wd)) {
        if (!dir.exists(wd) &&
            !dir.create(wd, showWarnings = FALSE)) {
            cli::cli_abort(
                "Cannot create working directory {.path {wd}"
            )
        }
        if (verbose) {
            cli::cli_inform("Setting working directory: {wd}")
        }
        old_wd <- getwd()
        setwd(wd)
        on.exit(setwd(old_wd), add = TRUE)
    }
    if (verbose) {
        msg <- paste("Running command", style_field(command))
        if (length(command_params)) {
            msg <- paste(
                msg, style_field(paste(command_params, collapse = " "))
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
check_io <- function(x, arg = substitute(x), call = parent.frame()) {
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

#' @return Always return a character.
#' @noRd
build_command_params <- function(params) {
    if (is.character(params)) return(params) # styler: off
    if (is.null(params)) return(character()) # styler: off
    if (is.list(params)) {
        return(as.character(unlist(params, use.names = FALSE)))
    }
    cli::cli_abort("Unsupported command parameters")
}

remove_opath <- function(opath) {
    # remove trailing backslash or slash
    opath <- path_trim(opath)
    opath <- opath[file.exists(opath)] # can also check directory
    if (length(opath) == 0L) return(NULL) # styler: off
    failed <- vapply(opath, unlink, integer(1L),
        recursive = TRUE, USE.NAMES = FALSE
    ) != 0L
    if (any(failed)) cli::cli_warn("Cannot remove {.path {opath[failed]}}")
}

build_opath <- function(odir, ofile = NULL, abs = FALSE,
                        call = parent.frame()) {
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
