#' @keywords internal
#' @noRd 
run_command <- function(args = character(), cmd = NULL, name = NULL, sys_args = list(), verbose = TRUE) {
    assert_length(cmd, 1L, null_ok = TRUE)
    assert_length(name, 1L, null_ok = TRUE)
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
        cli::cli_abort("One of {.arg cmd} or {.arg name} must be specified")
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

#' @keywords internal
#' @noRd 
handle_arg <- function(arg, tag, format = "%s", sep = " ") {
    if (is.null(arg) || isFALSE(arg)) {
        return(NULL)
    } else if (isTRUE(arg)) {
        return(tag)
    } else {
        return(sprintf(paste(tag, format, sep = sep), arg))
    }
}
