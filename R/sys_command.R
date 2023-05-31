#' Invoke a System Command
#' @param cmd Command to be invoked, as a character string.
#' @param ... Arguments passed to command.
#' @param env A named atomic vector define running environment of the command.
#' @param sys_args A list of arguments passed to [system2].
#' @param verbose A logical value indicates output running command message.
#' @return See [system2] for details
#' @export
run_command <- function(cmd, ..., env = NULL, sys_args = list(), verbose = TRUE) {
    run_sys_command(
        cmd = cmd, args = c(...), env = env, name = NULL,
        sys_args = sys_args, verbose = verbose
    )
}

#' Don't provide default value for name, in this way, we must provide name
#' manually for every internal function.
#' @keywords internal
#' @noRd 
run_sys_command <- function(cmd = NULL, name, args = character(), env = NULL, sys_args = list(), verbose = TRUE) {
    assert_class(
        env, function(x) is.atomic(x) && is_named2(x), "named {atomic}",
        null_ok = TRUE
    )
    assert_class(verbose, is_scalar_logical, "scalar logical")
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
    if (!is.null(env)) {
        if (verbose) {
            cli::cli_inform("Setting environment")
        }
        with_envvar(env = env, eval(call))
    } else {
        eval(call)
    }
}

#' @noRd 
sys_command <- function(cmd = NULL, name, args = character(), sys_args = list(), verbose = TRUE) {
    assert_class(cmd, is_scalar_character, "scalar character",
        null_ok = !is.null(name)
    )
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
