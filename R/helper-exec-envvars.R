envvar_parse_var <- function(
    envvar, name, value,
    arg = rlang::caller_arg(value), call = rlang::caller_env()) {
    value <- envvar_override(envvar, name, value, arg = arg, call = call)
    if (!is.null(value)) {
        names(value) <- name
        envvar <- c(envvar, value)
    }
    envvar
}

envvar_parse_path <- function(
    envvar, name, value,
    arg = rlang::caller_arg(value), call = rlang::caller_env()) {
    value <- envvar_override(envvar, name, value, arg = arg, call = call)
    if (!is.null(value)) {
        value <- envpath_add(name, value)
        names(value) <- name
        envvar <- c(envvar, value)
    }
    envvar
}

envpath_add <- function(name, value, sep = .Platform$path.sep) {
    value <- paste0(path.expand(rev(value)), collapse = sep)
    old_value <- Sys.getenv(name, unset = NA_character_)
    if (is.na(old_value)) value else paste(value, old_value, sep = sep)
}

envvar_override <- function(
    envvar, name, value,
    arg = rlang::caller_arg(value), call = rlang::caller_env()) {
    if (!is.null(envvar) && any(names(envvar) == name) && !is.null(value)) {
        cli::cli_warn(c_msg(
            "{.field {name}} in {.arg envvar}",
            "will always override {.arg {arg}}"
        ))
        value <- NULL
    }
    value
}

assert_envvar <- function(envvar, ..., arg = rlang::caller_arg(envvar), call = rlang::caller_env()) {
    assert_(
        x = envvar,
        assert_fn = function(x) is.atomic(x) && rlang::is_named2(x),
        what = c_msg("a named", style_cls("atomic")),
        ...,
        arg = arg, call = call
    )
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

#' Parse an atomic character for system2 env argument
#'
#' @return An atomic character which can be used directly in [system2]
#' @noRd
parse_envvar <- function(envvar) {
    assert_envvar(envvar, null_ok = FALSE)
    # https://stackoverflow.com/questions/39908415/env-argument-does-not-work-in-system2
    if (length(envvar) > 0L) {
        paste0(names(envvar), "=", envvar, ";")
    } else {
        character()
    }
}
