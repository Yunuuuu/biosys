#' Parse an atomic character for system2 env argument
#'
#' @param env A named atomic character of environment variables.
#' @return A string can be used in [system2]
#' @export
parse_envvar <- function(env) {
    assert_class(env, function(x) is.atomic(x) && is_named2(x), "named atomic")
    # https://stackoverflow.com/questions/39908415/env-argument-does-not-work-in-system2
    if (length(env) > 0L) {
        paste0(names(env), "=", env, ";")
    } else {
        character()
    }
}

#' @importFrom withr with_envvar
#' @export 
withr::with_envvar

#' @importFrom withr with_path
#' @export 
withr::with_path

# mimic withr::with_envvar
#' Temporarily change system environment variables.
#' @param env A named character
#' @param call Code to execute in the temporary environment
#' @param action should `env` values "replace", "prefix" or "suffix" existing
#'  variables with the same name.
#' @param sep A string separates the elements in the environment variable.
#' @noRd 
NULL

with_envvar2 <- function(env, call, action = "replace", sep = .Platform$path.sep) {
    assert_class(env, function(x) is.atomic(x) && is_named2(x), "named atomic")
    action <- match.arg(action, c("replace", "prefix", "suffix"))
    old <- set_envvar(as_envvars(env), action = action, sep = sep)
    old_set <- !is.na(old)
    if (any(old_set)) {
        on.exit(do.call("Sys.setenv", as.list(old[old_set])))
    }
    if (any(!old_set)) {
        on.exit(Sys.unsetenv(names(old)[!old_set]), add = TRUE)
    }
    eval(substitute(call))
}

#' @param env A named atomic character
#' @noRd
set_envvar <- function(env, action = "replace", sep = .Platform$path.sep) {
    old <- Sys.getenv(names(env), unset = NA, names = TRUE)
    set <- !is.na(env)
    need_action <- set & !is.na(old)
    if (any(need_action)) {
        if (action == "prefix") {
            env[need_action] <- paste(
                env[need_action], old[need_action],
                sep = sep
            )
        } else if (action == "suffix") {
            env[need_action] <- paste(
                old[need_action], env[need_action],
                sep = sep
            )
        }
    }
    if (any(set)) do.call("Sys.setenv", as.list(env[set]))
    if (any(!set)) Sys.unsetenv(names(env)[!set])
    invisible(old)
}

as_envvars <- function(env) {
    if (length(env) == 0L) {
        return(env)
    }
    # if there are duplicated entries keep only the last one
    env[!duplicated(names(env), fromLast = TRUE)]
}

get_env_sep <- function(win = ";", other = ":") {
    if (.Platform$OS.type == "windows") {
        win
    } else {
        other
    }
}
