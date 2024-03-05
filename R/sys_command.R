#' Invoke a System Command
#' @param cmd Command to be invoked, as a character string.
#' @param ... Arguments passed to command.
#' @param envpath A character define the environment PATH to add before running
#'  command.
#' @param env A named atomic vector define running environment of the command.
#' @param output Specifying the output file or directory to be removed if
#'  command running error. Note: all files in the directory will be removed, you
#'  must use this argument carefully.
#' @param abort A scalar logical indicates whether to report error if command
#'  return non-zero status.
#' @param sys_args A list of arguments passed to [system2].
#' @param verbose A logical value indicates output running command message.
#' @return See [system2] for details
#' @export
run_command <- function(cmd, ..., envpath = NULL, env = NULL, output = NULL, abort = FALSE, sys_args = list(), verbose = TRUE) {
    run_sys_command(
        cmd = cmd, args = c(...), envpath = envpath, env = env, name = NULL,
        output = output, abort = abort,
        sys_args = sys_args, verbose = verbose
    )
}

#' Don't provide default value for name, in this way, we must provide name
#' manually for every internal function.
#' @keywords internal
#' @noRd
run_sys_command <- function(cmd = NULL, name, args = character(), envpath = NULL, env = NULL, output = NULL, abort = FALSE, sys_args = list(), verbose = TRUE) {
    assert_(
        env, function(x) is.atomic(x) && rlang::is_named2(x),
        "named {.cls atomic}",
        null_ok = TRUE
    )
    assert_bool(verbose)
    assert_(output, is.character, "character path", null_ok = TRUE)
    assert_bool(abort)
    if (!is.null(sys_args$env)) {
        cli::cli_warn(
            "!" = "{.arg env} in {.arg sys_args} will not work",
            "i" = "Please use {.arg env} argument directly."
        )
        sys_args$env <- NULL
    }
    call <- quote(sys_command(
        cmd = cmd, name = name, args = args, sys_args = sys_args,
        verbose = verbose
    ))
    if (!is.null(envpath)) {
        # alyways override PATH in env
        env <- env[names(env) != "PATH"]
        env <- c(env, PATH = paste(
            paste0(envpath, collapse = .Platform$path.sep),
            Sys.getenv("PATH", unset = ""),
            sep = .Platform$path.sep
        ))
    }
    if (!is.null(env)) {
        if (verbose) {
            cli::cli_inform("Setting environment")
        }
        status <- with_envvar(env = env, eval(call), action = "replace")
    } else {
        status <- eval(call)
    }
    return_command(status, name = name, output = output, abort = abort)
}

#' @noRd
sys_command <- function(cmd = NULL, name, args = character(), sys_args = list(), verbose = TRUE) {
    assert_string(cmd, null_ok = !is.null(name))
    command <- define_command(cmd = cmd, name = name)
    if (verbose) {
        cli_args <- cli::cli_vec( # nolint
            args,
            list("vec-sep" = " ", "vec-last" = " ")
        )
        cli::cli_inform("Running command {.field {command} {cli_args}}")
    }
    sys_args <- c(
        list(command = command, args = as.character(args)),
        sys_args
    )
    do.call(system2, sys_args)
}

define_command <- function(cmd = NULL, name) {
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
            cli::cli_abort("Cannot find {.field {name}} command")
        }
    } else {
        cli::cli_abort("One of {.arg cmd} or {.arg name} must not be {.val NULL}")
    }
    command
}

return_command <- function(status, name = NULL, output = NULL, abort = FALSE) {
    if (is.null(name)) {
        msg <- "command"
    } else {
        msg <- "command {.filed {name}}"
    }
    if (status == 0L) {
        msg <- sprintf("Running %s successfully", msg)
        cli::cli_inform(msg)
    } else {
        msg <- sprintf("Something wrong when running %s", msg)
        delete_output(output)
        if (abort) {
            cli::cli_abort(msg)
        } else {
            cli::cli_warn(msg)
        }
    }
    status
}

delete_output <- function(output) {
    if (!is.null(output)) {
        # remove trailing backslash or slash
        output <- sub("(\\\\+|/+)$", "", output, perl = TRUE)
        output <- output[file.exists(output)] # can also check dir
        if (length(output)) {
            failed_unlink_files <- vapply(output, unlink, integer(1L),
                recursive = TRUE
            ) != 0L
            if (any(failed_unlink_files)) {
                cli::cli_warn("Cannot remove {.file {output[failed_unlink_files]}}")
            }
        }
    }
}
