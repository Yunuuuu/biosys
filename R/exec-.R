#' R6 Class to invoke a system command
Sys <- R6::R6Class("Sys",
    public = list(
        #' Invoke a the System Command
        #' @inheritParams exec
        #' @return
        #'  - if `wait=FALSE`, the process ID.
        #'  - if `abort=TRUE` and `wait=TRUE`, zero if command success,
        #'    otherwise, abort error.
        #'  - if `abort=FALSE` and `wait=TRUE`, exit status returned by the
        #'    command.
        #' @noRd
        exec = function(..., help = FALSE,
                        stdout = TRUE, stderr = TRUE, stdin = "",
                        wait = TRUE, timeout = 0L, abort = TRUE,
                        verbose = TRUE) {
            # check arguments
            assert_bool(help)
            check_io(stdout)
            check_io(stderr)
            check_wait(wait)
            assert_bool(abort)
            assert_bool(verbose)

            # collect all parameters, we cannot evaluate it since if help =
            # TRUE, it's much possible there were some missing argument
            # we only evaluate necessary parameters
            params <- rlang::enquos(...)

            # Extract params used by `Sys` object internal
            # `envpath`: params used to define running PATH environment
            # `envvar`: params used to define running environment variables
            # `command_locate`: params used to locate command path.
            # `command_params`: params used by regular command:
            #                   setup_command_params(), 
            #                   setup_temporary(), setup_opath(),
            #                   and extra_params.
            # `help_params`: params used by command to print help document
            #                (setup_help_params()).
            param_names <- private$parameters(help = help)
            # here: we check if all necessary parameters have been provided by
            #       external function. (in case from myself missing provide the
            #       parameters in external function)
            missing <- setdiff(param_names, names(params))
            if (length(missing)) {
                cli::cli_abort("Missing parameters: {.arg {missing}}")
            }
            sys_params <- params[intersect(names(params), param_names)]

            # we evaluate params since all params have been defused
            sys_params <- lapply(sys_params, rlang::eval_tidy)

            if (!help) {
                # for regular command (not help document), there were usually
                # additional arguments passed into command by `...`, they must
                # be un-named and each element must be of length one
                dots <- params[
                    !rlang::names2(params) %in% private$parameters(all = TRUE)
                ]
                dots <- lapply(dots, rlang::eval_tidy)
                # remove empty items
                dots <- dots[lengths(dots) > 0L]
                if (isTRUE(.subset2(private, "add_dots"))) {
                    named <- dots[rlang::have_name(dots)]
                    if (any(named)) {
                        cli::cli_abort(
                            "Unknown parameter{?s}: {.arg {names(named)}}"
                        )
                    }
                    if (any(lengths(dots) > 1L)) {
                        cli::cli_abort(c_msg(
                            "Additional parameters passed to command by",
                            style_arg("..."), "must be each of length one"
                        ))
                    }
                    dots <- unlist(dots, recursive = FALSE, use.names = FALSE)
                } else if (length(dots)) {
                    if (rlang::is_named(dots)) {
                        note <- c(i = c_msg(
                            "Did you misname argument{?s}",
                            "({.arg {names(dots)}})?"
                        ))
                    } else {
                        note <- NULL
                    }
                    cli::cli_abort(c(
                        "`...` must be empty for {.cls {Class}} object", note
                    ))
                }
            } else {
                dots <- NULL
            }

            # for help `TRUE`, always use current session
            if (help || isTRUE(wait)) {
                o <- private$run_command(
                    params = sys_params,
                    dots = dots,
                    help = help,
                    stdout = stdout,
                    stderr = stderr,
                    stdin = stdin,
                    timeout = timeout,
                    abort = abort,
                    verbose = verbose
                )
            } else {
                # to implement wait function, we starts a parallel R
                # process to conduct Asynchronous operations
                job <- parallel::mcparallel(
                    private$run_command(
                        params = sys_params,
                        dots = dots,
                        help = help,
                        stdout = stdout,
                        stderr = stderr,
                        stdin = stdin,
                        timeout = timeout,
                        abort = abort,
                        verbose = FALSE
                    )
                )
                o <- job$pid
                if (isFALSE(wait)) name <- NULL else name <- wait
                process_add(o, name)
            }
            invisible(o)
        }
    ),
    private = list(
        run_command = function(params, dots = NULL, help = FALSE,
                               stdout = TRUE, stderr = TRUE, stdin = "",
                               timeout = 0L, abort = TRUE, verbose = TRUE) {
            # Used for message
            Class <- fclass(self) # nolint

            # save current environment
            # some function will pull expression in this environment
            # usually the `on.exit` expresssion.
            private$environment <- environment()

            # prepare common parameters for usage -------
            # usually insert the script file:
            # see `SysKraken2Mpa` and `SysTrust4ImgtAnnot`
            private$params <- private$setup_params(params = params)
            if (!is.list(.subset2(private, "params"))) {
                cli::cli_abort(c_msg(
                    style_fn("{Class}$setup_params"), "must be a {.cls list}"
                ))
            }

            # set working directory -------------
            # wd <- inject2(private$setup_wd, .subset2(private, "params"))
            # if (rlang::is_string(wd)) {
            #     if (wd == "") {
            #         cli::cli_abort(
            #             "{.fn {Class}$setup_wd} return an empty string"
            #         )
            #     }
            #     if (!dir.exists(wd)) {
            #         cli::cli_abort("No directory {.path {wd}}")
            #     }
            #     old_wd <- getwd()
            #     setwd(wd)
            #     on.exit(setwd(old_wd), add = TRUE)
            # } else if (!is.null(wd)) {
            #     cli::cli_abort(
            #         "{.fn {Class}$setup_wd} must return a string of path"
            #     )
            # }

            # setting environment variables -------------
            envvar <- inject2(
                .subset2(private, "setup_envvar"),
                .subset2(private, "params")
            )
            check_envvar(envvar, "{Class}$setup_envvar")
            if (length(envvar) > 0L) {
                if (verbose) {
                    cli::cli_inform(
                        "Setting environment variables: {names(envvar)}"
                    )
                }
                old_envvar <- set_envvar(as_envvars(envvar), action = "replace")
                on.exit(set_envvar(old_envvar), add = TRUE)
            }

            # setting PATH environment variables -------
            envpath <- inject2(
                .subset2(private, "setup_envpath"),
                .subset2(private, "params")
            )
            check_envpath(envpath, "{Class}$setup_envpath")
            if (length(envpath) > 0L) {
                if (verbose) {
                    cli::cli_inform("Setting {.field PATH} environment")
                }
                envpath <- c(PATH = parse_envpath(envpath))
                old_envpath <- set_envvar(envpath, action = "prefix")
                # since `PATH` may exist in `envvar`,
                # so we restore `old_envpath` before `envvar`
                on.exit(set_envvar(old_envpath), add = TRUE, after = FALSE)
            }

            # locate command path ------------------------------
            # command should be located in current working directory.
            # we should run `command_locate` before `setup_temporary`
            command <- inject2(
                .subset2(private, "command_locate"),
                .subset2(private, "params")
            )
            check_command(command, "{Class}$command_locate")

            # run command ------------------------------
            if (help) {
                help_params <- inject2(
                    .subset2(private, "setup_help_params"),
                    .subset2(private, "params")
                )
                help_params <- check_command_params(
                    help_params, "{Class}$setup_help_params"
                )
                # always return status for help = TRUE
                o <- system3(
                    command = command,
                    command_params = help_params,
                    stdout = stdout, stderr = stderr, stdin = stdin,
                    wait = TRUE, timeout = timeout,
                    verbose = verbose
                )
            } else {
                # compute command params
                command_params <- inject2(
                    .subset2(private, "setup_command_params"),
                    .subset2(private, "params")
                )
                command_params <- check_command_params(
                    command_params, "{Class}$setup_command_params"
                )

                command_params <- c(dots, command_params)

                # set temporaty working directory -------------
                temporary <- inject2(
                    .subset2(private, "setup_temporary"),
                    .subset2(private, "params")
                )
                if (rlang::is_string(temporary)) {
                    if (!dir.exists(temporary)) {
                        cli::cli_abort("No directory {.path {temporary}}")
                    }
                    old_wd2 <- getwd()
                    setwd(temporary)
                }

                # run command --------------------------------
                status <- system3(
                    command = command,
                    command_params = command_params,
                    stdout = stdout, stderr = stderr, stdin = stdin,
                    wait = TRUE, timeout = timeout,
                    verbose = verbose
                )

                # restore original working directory --------
                if (rlang::is_string(temporary)) setwd(old_wd2)

                ############################################
                private$final(status = status, verbose = verbose)
                if (status == 0L) {
                    # if command run success, we run function `$success`
                    o <- private$success(status = status, verbose = verbose)
                    if (verbose) {
                        cli::cli_inform(c(
                            v = sprintf(
                                "Running command %s success",
                                style_field(command)
                            )
                        ))
                    }
                } else {
                    # if command run failed, we remove the output
                    opath <- inject2(
                        .subset2(private, "setup_opath"),
                        .subset2(private, "params")
                    )
                    if (!is.null(opath)) remove_opath(as.character(opath))

                    # then we run function `$error`
                    o <- private$error(status = status, verbose = verbose)
                    msg <- c(
                        c_msg(
                            "something wrong when running command",
                            style_field(command)
                        ),
                        `x` = "error code: {.val {status}}"
                    )
                    if (abort) cli::cli_abort(msg) else cli::cli_warn(msg)
                }
            }
            o
        },

        #' @field environment A environment used to Execution command which
        #' should be the function environment of `private$run_command`.
        environment = NULL,

        #' @description Used to attach an expression to be evaluated when
        #' exiting `private$run_command`.
        setup_exit = function(expr, after = TRUE, add = TRUE) {
            on_exit({{ expr }}, add = add, after = after, # styler: off
                envir = private$environment
            )
        },

        #' @field params A list of parameters used in the internal of object.
        params = list(),

        #' @description Extract parameters used by this object.
        #' @return A character of the used parameter of this object.
        parameters = function(help, all = FALSE) {
            argv <- c(
                # rlang::fn_fmls_names(.subset2(private, "setup_wd")),
                rlang::fn_fmls_names(.subset2(private, "setup_envpath")),
                rlang::fn_fmls_names(.subset2(private, "setup_envvar")),
                rlang::fn_fmls_names(.subset2(private, "command_locate"))
            )
            command_params <- c(
                rlang::fn_fmls_names(.subset2(
                    private, "setup_command_params"
                )),
                rlang::fn_fmls_names(.subset2(private, "setup_temporary")),
                rlang::fn_fmls_names(.subset2(private, "setup_opath")),
                .subset2(private, "extra_params")
            )
            help_params <- rlang::fn_fmls_names(.subset2(
                private, "setup_help_params"
            ))
            if (all) {
                argv <- c(argv, command_params, help_params)
            } else if (help) {
                argv <- c(argv, help_params)
            } else {
                argv <- c(argv, command_params)
            }
            setdiff(argv, .subset2(private, "internal_params"))
        },

        # methods to insert into or get parameters from `private$params`
        insert_param = function(name, value) {
            private$params[[name]] <- value
            invisible(self)
        },
        get_param = function(name) {
            .subset2(.subset2(private, "params"), name)
        },

        # Following fields or methods should be overrided by sub-class.
        #' @field add_dots A bool indicates whether `...` should be collected
        #' passed into command
        add_dots = TRUE,

        #' @field extra_params Additional parameters to be collected into
        #' `private$params`. Usually used by `setup_params`, `final`, `success`
        #' and `error` function.
        extra_params = NULL,

        #' @field internal_params Additional parameters used by `Sys` object but
        #' shouldn't collected from user input.
        internal_params = NULL,

        #' @description Method used to prepare parameters, ususally the common
        #' preparations for both `help_params` and `command_params`.
        #'
        #' * Parameters in `private$internal_params` should be added in this
        #'   function.
        #'
        #' @return An named list, the output list will be saved in
        #' `private$params`
        setup_params = function(params) params,

        #' Not implemented currently.
        #' @description Method used to change working directory
        #'
        #' @return A string of path or `NULL`.
        # setup_wd = function() NULL,

        #' @description Method used to create environment variables
        #'
        #' @return An named character
        setup_envvar = function(envvar) envvar,

        #' @description Method used to create PATH environment character
        #'
        #' @return An atomic character
        setup_envpath = function(envpath) envpath,

        #' @description Method used to locate command
        #'
        #' @return An string of command path
        command_locate = function(cmd) Sys.which(cmd),

        #' @description Method used to change command temporary directory.
        #'
        #' This function was only used by specific command, which will write a
        #' temp file, for these commands, we create a `setup_temporary` method
        #' to write the temporaty file into R-level temporaty directory.
        #'
        #' See `trust4_imgt_annot` function for usage.
        #'
        #' Note: we shouldn't rely on this method to changing user-level working
        #' directory (wd), since user will expect to locate command from their
        #' `wd`, but `setup_temporary` was run after `command_locate`, so
        #' command will only located from user current woking directory.
        #'
        #' You must use absolute path in `command_params` if you'll change the
        #' temporary directory.
        #'
        #' if you want to provide an argument for user to changing the `wd`,
        #' it's expected to changing wd in the beginning of `run_command`
        #' function. So defining a specific `setup_wd` method would be better.
        #'
        #' @return A string of path or `NULL`.
        setup_temporary = function() NULL,

        #' Method used to prepare parameters to print help document
        #'
        #' @return An atomic character, a list of all length one or `NULL`.
        setup_help_params = function() {
            .subset2(.subset2(private, "params"), "help_params")
        },
        setup_command_params = function() {
            .subset2(.subset2(private, "params"), "command_params")
        },

        #' Function to run after execute command
        #'
        #' - `final`: No matter command success or fail, will be run.
        final = function(status, verbose) NULL,

        #' What to do and to return if command run success.
        success = function(status, verbose) status,

        #' If command run failure, the output files, which will be extracted by
        #' function `private$setup_opath()`, will be removed.
        #'
        #' @note All files in the directory will be removed, you must use this
        #'  carefully.
        #' @return An atomic character or `NULL`.
        setup_opath = function() {
            .subset2(.subset2(private, "params"), "opath")
        },

        #' What to do and to return if command run failure
        error = function(status, verbose) status
    )
)

#' R6 Class to define a specific command
#' @noRd
Command <- R6::R6Class(
    "Command",
    inherit = Sys,
    private = list(
        name = NULL,
        command_locate = function(cmd) {
            if (is.null(cmd)) {
                name <- .subset2(private, "name")
                command <- Sys.which(name)
                if (!nzchar(command)) {
                    cli::cli_abort("Cannot locate {.field {name}} command")
                }
            } else {
                command <- super$command_locate(cmd)
            }
            command
        }
    )
)

inject2 <- function(fn, params) {
    args <- rlang::fn_fmls_names(fn)
    params <- params[intersect(args, names(params))]
    rlang::inject(fn(!!!params))
}

system3 <- function(command, command_params,
                    stdout = TRUE, stderr = TRUE, stdin = "",
                    wait = TRUE, timeout = 0L,
                    verbose) {
    if (verbose) {
        msg <- c_msg("Running command", style_field(command))
        if (length(command_params)) {
            msg <- c_msg(
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

check_wait <- function(wait, arg = substitute(x), call = parent.frame()) {
    if (rlang::is_bool(wait)) return() # styler: off
    if (rlang::is_string(wait)) {
        if (any(wait == process_names())) {
            cli::cli_abort("Process {wait} already exist")
        }
        if (wait == "") {
            cli::cli_abort("{.arg {arg}} cannot be an empty string")
        }
        return()
    }
    cli::cli_abort("{.arg {arg}} must be a string or a bool value")
}

check_command <- function(command, fn) {
    if (!rlang::is_string(command)) {
        cli::cli_abort(c_msg(style_fn(fn), "must return a string of command"))
    } else if (!nzchar(command)) {
        cli::cli_abort(c_msg(style_fn(fn), "cannot locate command"))
    }
}

check_command_params <- function(params, fn) {
    if (is.null(params)) return(character()) # styler: off
    if (is.character(params)) return(params) # styler: off
    if (is.list(params)) {
        if (any(lengths(params) == 1L)) {
            return(unlist(params, recursive = FALSE, use.names = FALSE))
        }
        cli::cli_abort(c_msg(
            style_fn(fn), "must return a list of all length one"
        ))
    }
    cli::cli_abort(c_msg(style_fn(fn), "must return a string of command"))
}

check_envvar <- function(envvar, fn) {
    if (!is.null(envvar) &&
        !(is.character(envvar) && rlang::is_named2(envvar))) {
        cli::cli_abort(c_msg(style_fn(fn), "must return a named character"))
    }
}

check_envpath <- function(envpath, fn) {
    if (!is.null(envpath) && !is.character(envpath)) {
        cli::cli_abort(c_msg(style_fn(fn), "must return a character"))
    }
}

# mimic withr::with_envvar
#' Temporarily change system environment variables.
#' @param envvar A named atomic character of environment variables.
#' @param code Code to execute in the temporary environment
#' @param action should `env` values "replace", "prefix" or "suffix" existing
#'  variables with the same name.
#' @param sep A string separates the elements in the environment variable.
#' @noRd
with_envvar <- function(envvar, code, action = "replace", sep = .Platform$path.sep) {
    action <- match.arg(action, c("replace", "prefix", "suffix"))
    if (length(envvar) > 0L) {
        old <- set_envvar(as_envvars(envvar), action = action, sep = sep)
        on.exit(set_envvar(old))
    }
    force(code)
}

#' @noRd
set_envvar <- function(envvar, action = "replace", sep = .Platform$path.sep) {
    old <- Sys.getenv(names(envvar), unset = NA_character_, names = TRUE)
    set <- !is.na(envvar)
    need_action <- set & !is.na(old)
    if (any(need_action)) {
        if (action == "prefix") {
            envvar[need_action] <- paste(
                envvar[need_action], old[need_action],
                sep = sep
            )
        } else if (action == "suffix") {
            envvar[need_action] <- paste(
                old[need_action], envvar[need_action],
                sep = sep
            )
        }
    }
    if (any(set)) do.call("Sys.setenv", as.list(envvar[set]))
    if (any(!set)) Sys.unsetenv(names(envvar)[!set])
    invisible(old)
}

as_envvars <- function(envvar) {
    # if there are duplicated entries keep only the last one
    envvar[!duplicated(names(envvar), fromLast = TRUE)]
}

parse_envpath <- function(envpath, sep = .Platform$path.sep) {
    paste0(path.expand(rev(envpath)), collapse = sep)
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
                        call = rlang::caller_env()) {
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

envvar_parse_path <- function(envvar, name, value) {
    if (!is.null(value)) {
        value <- envpath_add(name, value)
        names(value) <- name
        envvar <- c(envvar, value)
    }
    envvar
}

envpath_add <- function(name, value, sep = .Platform$path.sep) {
    value <- parse_envpath(value)
    old_value <- Sys.getenv(name, unset = NA_character_)
    if (is.na(old_value)) value else paste(value, old_value, sep = sep)
}
