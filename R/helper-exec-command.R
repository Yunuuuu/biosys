# cmd: user api
# name: internal api
exec_locate_command <- function(cmd = NULL, name) {
    if (!is.null(cmd)) {
        command <- command_locate_cmd2(cmd)
    } else if (!is.null(name)) {
        command <- command_locate_name2(name)
    } else {
        cli::cli_abort("{.arg cmd} or {.arg name} must be provided")
    }
    command
}

command_new_cmd <- function(x, ..., class = x) {
    if (is.null(x)) return(NULL) # styler: off
    structure(x, ..., class = c(class, "command_cmd"))
}

command_locate_cmd2 <- function(cmd) {
    # cannot run code after UseMethod,
    # so we define another function to run the generic function
    command <- command_locate_cmd(cmd)
    if (!nzchar(command)) {
        cli::cli_abort("Cannot locate {.field {cmd}} command")
    }
    command
}

command_locate_cmd <- function(cmd) {
    UseMethod("command_locate_cmd")
}

#' @export
command_locate_cmd.command_cmd <- function(cmd) {
    if (!file.exists(cmd)) {
        command <- Sys.which(cmd)
    } else {
        command <- path.expand(cmd)
    }
    command
}

#' @export
command_locate_cmd.default <- function(cmd) {
    cli::cli_abort("{.arg cmd} must be a {.cls command_cmd} object")
}

# let each command define their own method based on `name`.
# used by `exec-python.R`
command_new_name <- function(x, ..., class = NULL) {
    assert_string(x, empty_ok = FALSE)
    structure(x, ..., class = c(class, "command_name"))
}

command_locate_name2 <- function(name) {
    # cannot run code after UseMethod,
    # so we define another function to run the generic function
    command <- command_locate_name(name)
    if (!nzchar(command)) {
        cli::cli_abort("Cannot locate {.field {name}} command")
    }
    command
}

command_locate_name <- function(name) UseMethod("command_locate_name")

#' @export
command_locate_name.command_name <- function(name) Sys.which(name)

#' @export
command_locate_name.default <- function(name) {
    cli::cli_abort("{.arg name} must be a {.cls command_name} object")
}
