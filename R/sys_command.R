#' Invoke a System Command
#' @param cmd Command to be invoked, as a character string.
#' @param ... Arguments passed to command.
#' @param env_path An atomic vector define the `PATH` to be added into the
#'  environment variable `PATH`. 
#' @param env A named character define the to environment variables when running
#'  `cmd`.
#' @param sys_args A list of arguments passed to [system2].
#' @param verbose A logical value indicates output running command message.
#' @return See [system2] for details
#' @export
run_command <- function(cmd, ..., env_path = NULL, env = character(), sys_args = list(), verbose = TRUE) {
    run_sys_command(
        cmd = cmd, args = c(...), name = NULL,
        env_path = env_path, env = env,
        sys_args = sys_args, verbose = verbose
    )
}

#' Don't provide default value for name, in this way, we must provide name
#' manually for every internal function.
#' @noRd
run_sys_command <- function(cmd = NULL, name, args = character(), env_path = NULL, env = character(), sys_args = list(), verbose = TRUE, cmd_null = TRUE) {
    assert_class(cmd, is_scalar_character, "scalar character",
        null_ok = !is.null(name)
    )
    assert_class(verbose, is_scalar_logical, "scalar logical")
    # sys_args$env can be a void atomic or NULL
    assert_class(env, function(x) is.atomic(x) && is_named2(x), "named atomic")
    command <- define_command(cmd = cmd, name = name)
    if (verbose) {
        cli_args <- cli::cli_vec( # nolint
            args,
            list("vec-sep" = " ", "vec-last" = " ")
        )
        cli::cli_alert("Running command {.field {command} {cli_args}}")
    }
    if (!is.null(env_path)) {
        env <- set_env(env, "PATH",
            add = normalizePath(env_path, mustWork = TRUE)
        )
    }
    sys_args <- c(
        list(
            command = command, args = as.character(args),
            env = parse_env(env)
        ),
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
