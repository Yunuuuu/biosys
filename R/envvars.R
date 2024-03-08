#' Parse an atomic character for system2 env argument
#'
#' @return An atomic character which can be used directly in [system2]
#' @noRd
parse_envvar <- function(envvar) {
    assert_(
        envvar, function(x) is.atomic(x) && rlang::is_named2(x),
        "named {.cls atomic}"
    )
    # https://stackoverflow.com/questions/39908415/env-argument-does-not-work-in-system2
    if (length(envvar) > 0L) {
        paste0(names(envvar), "=", envvar, ";")
    } else {
        character()
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
    old <- set_envvar(as_envvars(envvar), action = action, sep = sep)
    old_set <- !is.na(old)
    if (any(old_set)) {
        on.exit(do.call("Sys.setenv", as.list(old[old_set])))
    }
    if (any(!old_set)) {
        on.exit(Sys.unsetenv(names(old)[!old_set]), add = TRUE)
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
    if (length(envvar) == 0L) return(envvar) # styler: off
    # if there are duplicated entries keep only the last one
    envvar[!duplicated(names(envvar), fromLast = TRUE)]
}

envpath_add <- function(paths, sep = .Platform$path.sep) {
    path <- paste0(paths, collapse = sep)
    old_path <- Sys.getenv("PATH", unset = NA_character_)
    if (is.na(old_path)) path else paste(path, old_path, sep = sep)
}
