# cmd: user api
# name: internal api
exec_locate_command <- function(cmd = NULL, name) {
    if (!is.null(cmd)) {
        command <- command_locate_cmd(cmd)
    } else if (!is.null(name)) {
        command <- command_locate_name(name)
    } else {
        cli::cli_abort("{.arg cmd} or {.arg name} must be provided")
    }
    command
}

command_locate_cmd <- function(cmd) {
    if (!file.exists(cmd)) {
        command <- Sys.which(cmd)
        if (!nzchar(command)) {
            cli::cli_abort("Cannot locate {.field {cmd}} command")
        }
    } else {
        command <- path.expand(cmd)
    }
    command
}

# let each command define their own method based on `name`.
# used by `exec-python.R`
command_new_name <- function(x, ..., class = NULL) {
    assert_string(x, empty_ok = FALSE)
    structure(x, ..., class = c(class, "command_name"))
}

command_locate_name <- function(name) {
    command <- UseMethod("command_locate_name")
    if (!nzchar(command)) {
        cli::cli_abort("Cannot locate {.field {name}} command")
    }
    command
}
command_locate_name.command_name <- function(name) Sys.which(name)
command_locate_name.default <- function(name) {
    cli::cli_abort("{.arg name} must be a {.cls command_name} object")
}
