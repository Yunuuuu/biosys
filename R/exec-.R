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
        run = function(..., help = FALSE,
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
            params <- rlang::enquos(..., .ignore_empty = "all")

            if (!help) {
                # for regular command (not help document), there were usually
                # additional arguments passed into command by `...`, they must
                # be un-named
                dots <- params[
                    !rlang::names2(params) %in% private$parameters(all = TRUE)
                ]
                dots <- lapply(dots, rlang::eval_tidy)
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
            }

            # Extract params used by `Sys` object internal
            # `envpath`: params used to define running PATH environment
            # `envvar`: params used to define running environment variables
            # `command_locate`: params used to locate command path.
            # `command_params`: params used by regular command:
            #                   setup_command_params(),
            #                   setup_temporary(), setup_opath(),
            #                   final(), success(), and error()
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
            params <- params[intersect(names(params), param_names)]

            # we collect and evaluate params since all params have been defused
            private$params <- lapply(params, rlang::eval_tidy)

            # Some function will use the verbose parameters
            # final(), success()
            # attach verbose
            private$params$verbose <- verbose

            # we collect params for system2
            private$system2_params <- list(
                stdout = stdout,
                stderr = stderr,
                stdin = stdin,
                timeout = timeout
            )

            # for help `TRUE`, always use current session
            if (help || isTRUE(wait)) {
                o <- private$exec(help = help, abort = abort)
            } else {
                # to implement wait function, we starts a parallel R
                # process to conduct Asynchronous operations
                job <- parallel::mcparallel(
                    private$exec(help = help, abort = abort),
                    silent = TRUE
                )
                o <- job$pid
                if (isFALSE(wait)) name <- NULL else name <- wait
                process_add(o, name)
            }
            invisible(o)
        }
    ),
    private = list(

        #' @field params A list of parameters used by command.
        dots = list(),

        #' @field system2_params A list of parameters used by system2.
        system2_params = list(),

        #' @field params A list of parameters used by `Sys` object methods.
        params = list(),

        #' @field environment A environment used to Execution command which
        #' should be the function environment of `private$exec_command`.
        environment = NULL,

        #' @field status Exit code of command.
        status = NULL,

        #' @description Used to prepare command environment including
        #' working directory, and environment variables, then this method call
        #' `private$exec_command` to invoke a system command.
        exec = function(help, abort) {
            # prepare common parameters for usage -------
            # usually insert the script file:
            # see `SysKraken2Mpa` and `SysTrust4ImgtAnnot`
            private$params <- private$setup_params(.subset2(private, "params"))

            # set working directory ---------------------
            wd <- do_call(
                .subset2(private, "setup_wd"),
                .subset2(private, "params"),
                "setup_wd"
            )
            if (!is.null(wd)) {
                if (!dir.exists(wd)) {
                    cli::cli_abort(c(
                        "Cannot set working directory",
                        i = "No such path {.path {wd}}"
                    ))
                }
                old_wd <- getwd()
                setwd(wd)
                on.exit(setwd(old_wd), add = TRUE)
            }

            # setting environment variables -------------
            envvar <- do_call(
                .subset2(private, "setup_envvar"),
                .subset2(private, "params"),
                "setup_envvar"
            )
            envvar_msg <- "Setting environment variables: {names(envvar)}"

            # setting PATH environment variables -------
            envpath <- do_call(
                .subset2(private, "setup_envpath"),
                .subset2(private, "params"),
                "setup_envpath"
            )
            envpath_msg <- "Setting {.field PATH} environment"
            if (length(envvar) > 0L && length(envpath) > 0L) {
                if (private$get_param("verbose")) {
                    cli::cli_inform(envvar)
                    cli::cli_inform(envpath)
                }
                withr::with_envvar(envvar, withr::with_path(
                    envpath,
                    private$exec_command(help, abort)
                ))
            } else if (length(envpath) > 0L) {
                if (private$get_param("verbose")) cli::cli_inform(envpath)
                withr::with_path(
                    envpath,
                    private$exec_command(help, abort)
                )
            } else if (length(envvar) > 0L) {
                if (private$get_param("verbose")) cli::cli_inform(envvar)
                withr::with_envvar(
                    envvar,
                    private$exec_command(help, abort)
                )
            } else {
                private$exec_command(help, abort)
            }
        },

        #' @description The exact method to execute a system command.
        exec_command = function(help, abort) {
            # save current environment -------------------------
            # `setup_exit` will push expression into this environment
            private$environment <- environment()

            # locate command path ------------------------------
            command <- do_call(
                .subset2(private, "command_locate"),
                .subset2(private, "params"),
                "command_locate"
            )
            if (is.null(command) || !nzchar(command)) {
                cli::cli_abort("cannot locate command")
            }

            # run command ------------------------------
            if (help) {
                help_params <- do_call(
                    .subset2(private, "setup_help_params"),
                    .subset2(private, "params"),
                    "setup_help_params"
                )
                help_params <- build_command_params(help_params)

                private$status <- do.call(system3, c(
                    list(
                        command = command,
                        command_params = help_params,
                        verbose = private$get_param("verbose")
                    ),
                    private$system2_params
                ))
                # always return status for help = TRUE
                o <- .subset2(private, "status")
            } else {
                # compute command params
                command_params <- do_call(
                    .subset2(private, "setup_command_params"),
                    .subset2(private, "params"),
                    "setup_command_params"
                )
                command_params <- private$combine_params(
                    dots = build_command_params(.subset2(private, "dots")),
                    params = build_command_params(command_params)
                )

                # set temporaty working directory -------------
                tmp <- do_call(
                    .subset2(private, "setup_temporary"),
                    .subset2(private, "params"),
                    "setup_temporary"
                )
                if (!is.null(tmp)) {
                    dir_create(tmp)
                    old_wd2 <- getwd()
                    setwd(tmp)
                    on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
                }

                # run command --------------------------------
                private$status <- do.call(system3, c(
                    list(
                        command = command,
                        command_params = command_params,
                        verbose = private$get_param("verbose")
                    ),
                    private$system2_params
                ))

                # restore original working directory --------
                if (!is.null(tmp)) setwd(old_wd2)

                ############################################
                do_call(
                    .subset2(private, "final"),
                    .subset2(private, "params"),
                    "final"
                )
                if (.subset2(private, "status") == 0L) {
                    # if command run success, we run function `$success`
                    o <- do_call(
                        .subset2(private, "success"),
                        .subset2(private, "params"),
                        "success"
                    )
                    if (private$get_param("verbose")) {
                        cli::cli_inform(c(
                            v = sprintf(
                                "Running command %s success",
                                style_field(command)
                            )
                        ))
                    }
                } else {
                    # if command run failed, we remove the output
                    opath <- do_call(
                        .subset2(private, "setup_opath"),
                        .subset2(private, "params"),
                        "setup_opath"
                    )
                    if (!is.null(opath)) remove_opath(as.character(opath))

                    # then we run function `$error`
                    o <- do_call(
                        .subset2(private, "error"),
                        .subset2(private, "params"),
                        "error"
                    )
                    msg <- c(
                        paste(
                            "something wrong when running command",
                            style_field(command)
                        ),
                        `x` = sprintf(
                            "error code: %s",
                            style_val(.subset2(private, "status"))
                        )
                    )
                    if (abort) cli::cli_abort(msg) else cli::cli_warn(msg)
                }
            }
            o
        },

        #' @description Used to attach an expression to be evaluated when
        #' exiting `private$exec_command`.
        setup_exit = function(expr, after = TRUE, add = TRUE) {
            on_exit(!!rlang::enquo(expr),
                add = add, after = after,
                envir = private$environment
            )
        },

        #' @description Extract parameters used by this object.
        #' @return A character of the used parameter of this object.
        parameters = function(help, all = FALSE) {
            argv <- c(
                rlang::fn_fmls_names(.subset2(private, "setup_wd")),
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
                rlang::fn_fmls_names(.subset2(private, "final")),
                rlang::fn_fmls_names(.subset2(private, "success")),
                rlang::fn_fmls_names(.subset2(private, "error"))
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
            setdiff(argv, c(.subset2(private, "internal_params"), "verbose"))
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
        #' @field collect_dots A bool indicates whether `...` should be
        #' collected passed into command
        collect_dots = TRUE,

        #' @field internal_params Additional parameters used by `Sys` object but
        #' shouldn't collected from user input.
        internal_params = NULL,

        #' @description Method used to prepare parameters, ususally the common
        #' preparations for both `help_params` and `command_params`.
        #'
        #' * Parameters in `private$internal_params` should be added in this
        #'   function.
        #'
        #' Note: we shouln't convert relative path into absolute path in this
        #' mehtod, as we'll changing working directory after `setup_params`.
        #'
        #' @return An named list, the output list will be saved in
        #' `private$params`
        setup_params = function(params) params,

        #' @description Method used to change working directory
        #'
        #' @return A string of path or `NULL`.
        setup_wd = function() NULL,

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

        #' Method used to prepare parameters to print help document
        #'
        #' @return An atomic character, or `NULL`.
        setup_help_params = function() {
            .subset2(.subset2(private, "params"), "help_params")
        },

        #' @description Method used to change command temporary directory.
        #'
        #' This function was only used by specific command, which will write a
        #' temp file, for these commands, we create a `setup_temporary` method
        #' to write the temporaty file into R-level temporaty directory.
        #'
        #' See `trust4_imgt_annot` function for usage.
        #'
        #' Note:
        #' 1. we shouldn't rely on this method to changing user-level working
        #' directory (wd), since user will expect to locate command from their
        #' `wd`, but `setup_temporary` was run after `command_locate`.
        #'
        #' 2. You must use absolute path in `command_params` if you'll change
        #' the temporary directory.
        #'
        #' @return A string of path or `NULL`.
        setup_temporary = function() NULL,

        #' Method used to prepare parameters to run regular command
        #'
        #' @return An atomic character, or `NULL`.
        setup_command_params = function() {
            .subset2(.subset2(private, "params"), "command_params")
        },

        #' Method used to combine `dots` and `params`
        #'
        #' @return An atomic character, or `NULL`.
        combine_params = function(dots, params) c(dots, params),

        #' Function to run after execute command
        #'
        #' - `final`: No matter command success or fail, will be run.
        final = function() NULL,

        #' What to do and to return if command run success.
        success = function() .subset2(private, "status"),

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
        error = function() .subset2(private, "status")
    )
)

#' R6 Class to define a specific command, which is bound to the command name. In
#' this way, we won't always need to provide the command.
#' @noRd
Command <- R6::R6Class(
    "Command",
    inherit = Sys,
    private = list(
        #' @field names Command names
        names = NULL,
        command_locate = function(cmd) {
            if (is.null(cmd)) {
                names <- .subset2(private, "names")
                for (name in names) {
                    if (nzchar(command <- Sys.which(name))) {
                        break
                    }
                }
                if (!nzchar(command)) {
                    cli::cli_abort(sprintf(
                        "Cannot locate %s command",
                        oxford_comma(style_field(names), final = "or")
                    ))
                }
            } else {
                command <- super$command_locate(cmd)
            }
            command
        }
    )
)

do_call <- function(fn, params, name) {
    args <- rlang::fn_fmls_names(fn)
    params <- params[intersect(args, names(params))]
    # we rebuild function name,
    # in this way, error message can indicates the call
    assign(name, value = fn)
    do.call(name, params)
}

system3 <- function(command, command_params,
                    stdout = TRUE, stderr = TRUE, stdin = "",
                    wait = TRUE, timeout = 0L,
                    verbose) {
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

build_command_params <- function(params) {
    if (is.character(params)) return(params) # styler: off
    if (is.null(params)) return(character()) # styler: off
    if (is.list(params)) {
        return(as.character(unlist(params, use.names = FALSE)))
    }
    cli::cli_abort("Unsupported command parameters")
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
    value <- parse_envpath(value, sep = sep)
    old_value <- Sys.getenv(name, unset = NA_character_)
    if (is.na(old_value)) value else paste(value, old_value, sep = sep)
}

parse_envpath <- function(envpath, sep = .Platform$path.sep) {
    envpath <- as.character(envpath)
    paste0(normalizePath(envpath, "/", mustWork = FALSE), collapse = sep)
}
