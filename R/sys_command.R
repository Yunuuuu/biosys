#' Invoke a System Command
#' @param cmd Command to be invoked, as a character string.
#' @param ... Arguments passed to command.
#' @param sys_args A list of arguments passed to [system2].
#' @param verbose A logical value indicates output running command message.
#' @return See [system2] for details
#' @export 
run_command <- function(cmd, ..., sys_args = list(), verbose = TRUE) {
    run_sys_command(
        cmd = cmd, args = c(...), name = NULL,
        sys_args = sys_args, verbose = verbose
    )
}

#' Don't provide default value for name, in this way, must provide name manually
#' for every internal function.
#' @keywords internal
#' @noRd 
run_sys_command <- function(cmd = NULL, name, args = character(), sys_args = list(), verbose = TRUE, cmd_null = TRUE) {
    assert_class(cmd, is_scalar_character, "scalar character",
        null_ok = !is.null(name)
    )
    assert_class(verbose, is_scalar_logical, "scalar logical")
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
    }
    sys_args <- c(list(command = command, args = as.character(args)), sys_args)
    if (verbose) {
        cli_args <- cli::cli_vec( # nolint
            args,
            list("vec-sep" = " ", "vec-last" = " ")
        )
        cli::cli_alert("Running command {.field {command} {cli_args}}")
    }
    do.call(system2, sys_args)
}
