#' R6 Class to invoke a system command
Sys <- R6::R6Class("Sys",
    public = list(
        #' Invoke a the System Command
        #' @inheritParams exec
        #' @noRd
        exec = function(cmd, ..., help = FALSE,
                        stdout = TRUE, stderr = TRUE, stdin = "",
                        wait = TRUE, timeout = 0L, abort = TRUE,
                        verbose = TRUE) {
            # check arguments
            assert_bool(help)
            check_io(stdout)
            check_io(stderr)
            assert_bool(wait)
            assert_bool(abort)
            assert_bool(verbose)

            # Used for message
            Class <- fclass(self) # nolint

            # collect all parameters, we cannot evaluate it since if help =
            # TRUE, it's much possible there were some missing argument
            # we only evaluate necessary parameters
            params <- rlang::enquos(...)

            # Split up params between
            # `envpath`: params used to define running PATH environment
            # `envvar`: params used to define running environment variables
            # `command_params`: params used by regular command:
            #                   setup_command_params() setup_opath(), and
            #                   extra_params
            # `help_params`: params used by command to print help document
            #                (setup_help_params()).
            sys_params <- params[
                intersect(names(params), private$parameters(help = help))
            ]
            sys_params <- lapply(sys_params, rlang::eval_tidy)

            # save current environment
            private$environment <- environment()

            # prepare common parameters for usage -------
            # usually insert the script file: see `SysKraken2Mpa` and
            # `SysTrust4ImgtAnnot`
            sys_params <- private$setup_params(params = sys_params)
            if (!is.list(sys_params)) {
                cli::cli_abort(c_msg(
                    style_fn("{Class}$setup_params"), "must be a {.cls list}"
                ))
            }
            private$params <- sys_params

            # setting environment variables -------------
            envvar <- inject2(private$setup_envvar, private$params)
            check_envvar(envvar, "{Class}$setup_envvar")
            if (length(envvar) > 0L) {
                if (verbose) {
                    cli::cli_inform("Setting environment variables: {names(envvar)}")
                }
                old_envvar <- set_envvar(as_envvars(envvar), action = "replace")
                on.exit(set_envvar(old_envvar), add = TRUE)
            }

            # setting PATH environment variables -------
            envpath <- inject2(private$setup_envpath, private$params)
            check_envpath(envpath, "{Class}$setup_envpath")
            if (length(envpath) > 0L) {
                if (verbose) cli::cli_inform("Setting {.field PATH} environment")
                envpath <- c(PATH = parse_envpath(envpath))
                old_envpath <- set_envvar(envpath, action = "prefix")
                # since `PATH` may exist in `envvar`,
                # so we must set `old_envpath` before `envvar`
                on.exit(set_envvar(old_envpath), add = TRUE, after = FALSE)
            }

            # set working directory -------------
            wd <- inject2(private$setup_wd, private$params)
            if (rlang::is_string(wd)) {
                if (wd == "") {
                    cli::cli_abort(
                        "{.fn {Class}$setup_wd} return an empty string"
                    )
                }
                if (!dir.exists(wd)) cli::cli_abort("No directory {.path {wd}}")
                old_wd <- getwd()
                setwd(wd)
                on.exit(setwd(old_wd), add = TRUE)
            } else if (!is.null(wd)) {
                cli::cli_abort(
                    "{.fn {Class}$setup_wd} must return a string of path"
                )
            }

            # locate command path ------------------------------
            command <- private$command_locate(cmd)
            check_command(command, "{Class}$command_locate")

            # run command ------------------------------
            if (help) {
                help_params <- inject2(
                    private$setup_help_params,
                    private$params
                )
                help_params <- check_command_params(
                    help_params, "{Class}$setup_help_params"
                )
                status <- system3(
                    command = command,
                    command_params = help_params,
                    stdout = stdout, stderr = stderr, stdin = stdin,
                    wait = TRUE, timeout = timeout,
                    verbose = verbose
                )
                invisible(status) # always return status for help = TRUE
            } else {
                command_params <- inject2(
                    private$setup_command_params,
                    private$params
                )
                command_params <- check_command_params(
                    command_params, "{Class}$setup_command_params"
                )

                # `dots` is used to pass the additional arguments for the
                # command, they must be un-named and each element must be of
                # length one
                dots <- params[
                    !rlang::names2(params) %in% private$parameters(all = TRUE)
                ]
                dots <- lapply(dots, rlang::eval_tidy)
                # remove empty items
                dots <- dots[lengths(dots) > 0L]
                if (isTRUE(private$add_dots)) {
                    named <- dots[rlang::have_name(dots)]
                    if (any(named)) {
                        cli::cli_abort("Unknown argument{?s}: {names(named)}")
                    }
                    if (any(lengths(dots) > 1L)) {
                        cli::cli_abort(c_msg(
                            "additional paramters passed to",
                            style_field(command), "by", style_arg("..."),
                            "must be each of length one"
                        ))
                    }
                    dots <- unlist(dots, recursive = FALSE, use.names = FALSE)
                } else if (length(dots)) {
                    if (rlang::is_named(dots)) {
                        note <- c(i = "Did you forget to name an argument?")
                    } else {
                        note <- NULL
                    }
                    cli::cli_abort(c(
                        "`...` must be empty for {.cls {Class}} object", note
                    ))
                }
                status <- system3(
                    command = command,
                    command_params = c(dots, command_params),
                    stdout = stdout, stderr = stderr, stdin = stdin,
                    wait = wait, timeout = timeout,
                    verbose = verbose
                )
                private$final(
                    status = status,
                    command = command, verbose = verbose
                )
                if (status == 0L) {
                    private$success(
                        status = status,
                        command = command, verbose = verbose
                    )
                    if (verbose) {
                        cli::cli_inform(sprintf(
                            "Running command %s success", style_field(command)
                        ))
                    }
                } else {
                    private$error(
                        status = status,
                        command = command, verbose = verbose
                    )
                    # if command run failed, we remove the output
                    opath <- inject2(private$setup_opath, private$params)
                    if (!is.null(opath)) remove_opath(as.character(opath))
                    msg <- c(
                        sprintf(
                            "something wrong when running command {.field %s}", command
                        ),
                        i = "error code: {.val {status}}"
                    )
                    if (abort) cli::cli_abort(msg) else cli::cli_warn(msg)
                }
                private$return(status = status, command, verbose)
            }
        }
    ),
    private = list(
        #' @field environment A environment used to Execution command which
        #' should be the function environment of `self$exec`.
        environment = NULL,

        #' @field params A list of parameters used in the internal of object.
        params = list(),

        #' @description Extract parameters used by this object.
        #' @return A character of the used parameter of this object.
        parameters = function(help, all = FALSE) {
            argv <- c(
                rlang::fn_fmls_names(.subset2(private, "setup_envpath")),
                rlang::fn_fmls_names(.subset2(private, "setup_envvar")),
                rlang::fn_fmls_names(.subset2(private, "setup_wd"))
            )
            command_params <- c(
                rlang::fn_fmls_names(.subset2(
                    private, "setup_command_params"
                )),
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
            argv
        },

        #' @field extra_params Additional parameters to be added into
        #' `private$params`. Usually used by `final`, `success` and `error`
        #' function
        extra_params = NULL,
        insert_param = function(name, value) {
            private$params[[name]] <- value
            invisible(self)
        },
        get_param = function(name) .subset2(private$params, name),

        #' @description Method used to prepare parameters
        #'
        #' @return An named list, the output list will be saved in
        #' `private$params`
        setup_params = function(params) params,

        #' @field add_dots A bool indicates whether `...` should be collected
        #' passed into command
        add_dots = TRUE,

        #' @description Method used to create environment variables
        #'
        #' @return An named character
        setup_envvar = function(envvar) envvar,

        #' @description Method used to create PATH environment character
        #'
        #' @return An atomic character
        setup_envpath = function(envpath) envpath,

        #' @description Method used to change working directory
        #'
        #' @return A string of path or `NULL`.
        setup_wd = function() NULL,

        #' @description Method used to locate command
        #'
        #' @return An string of command path
        command_locate = function(cmd) {
            if (file.exists(cmd)) path.expand(cmd) else Sys.which(cmd)
        },

        #' Method used to prepare output path
        #'
        #' If command run failure, every existed path returned by `$setup_opath`
        #' will be removed.
        #'
        #' @note All files in the directory will be removed, you must use this
        #'  carefully.
        #' @return An atomic character or `NULL`.
        setup_opath = function() {
            .subset2(.subset2(private, "params"), "opath")
        },

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
        #' - `success`: when command success, will be run.
        #' - `error`: when command fail, will be run.
        final = function(status, command, verbose) NULL,
        success = function(status, command, verbose) NULL,
        error = function(status, command, verbose) NULL,
        return = function(status, command, verbose) invisible(status)
    )
)

SysName <- R6::R6Class(
    "SysName",
    inherit = Sys,
    public = list(
        initialize = function(name = NULL) {
            if (is.null(name)) {
                if (is.null(private$name)) {
                    cli::cli_abort(c_msg(
                        "You must provide {.arg name}",
                        "to initialize {.cls fclass(self)} object"
                    ))
                }
            } else if (!rlang::is_string(name) || name == "") {
                cli::cli_abort("{.arg name} must be a non-empty string")
            } else {
                private$name <- name
            }
        }
    ),
    private = list(
        name = NULL,
        command_locate = function(cmd) {
            if (is.null(cmd)) {
                command <- private$command_locate_by_name()
                if (!nzchar(command)) {
                    cli::cli_abort(
                        "Cannot locate {.field {private$name}} command"
                    )
                }
            } else {
                command <- super$command_locate(cmd)
            }
            command
        },
        command_locate_by_name = function() {
            Sys.which(.subset2(private, "name"))
        }
    )
)

inject2 <- function(fn, params) {
    args <- rlang::fn_fmls_names(fn)
    params <- params[intersect(args, names(params))]
    rlang::inject(fn(!!!params))
}

on_exit <- function(expr, add = FALSE, after = TRUE, envir = parent.frame()) {
    expr <- rlang::enquo(expr)
    expr <- rlang::expr(on.exit(expr = !!expr, add = !!add, after = !!after))
    rlang::eval_tidy(expr, env = envir)
}

system3 <- function(command, command_params,
                    stdout = TRUE, stderr = TRUE, stdin = "",
                    wait = TRUE, timeout = 0L,
                    verbose) {
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
    opath <- sub("(\\\\+|/+)$", "", opath, perl = TRUE)
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
