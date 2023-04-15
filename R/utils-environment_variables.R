#' @param env A named atomic character
#' @param name A string
#' @param add An atomic vector
#' @param value An atomic vector
#' @return A modified `env`
#' @noRd
set_env <- function(env, name, add = NULL, value = NULL, sep = get_env_sep()) {
    if (is.null(value)) {
        if (is.null(name)) return(env)
        if (has_name(env, name)) {
            old <- env[name]
        } else {
            old <- Sys.getenv(name, unset = "", names = FALSE)
        }
        added_value <- paste0(add, collapse = sep)
        env[name] <- add_env_value(old, added_value, sep = sep)
    } else {
        env[name] <- paste0(value, collapse = sep)
    }
    env
}

#' @param x A named atomic character
#' @return A string can be used in system2
#' @noRd
parse_env <- function(x) {
    # https://stackoverflow.com/questions/39908415/env-argument-does-not-work-in-system2
    if (length(x) > 0L) {
        paste0(names(x), "=", x, ";")
    } else {
        character()
    }
}

#' @param sep A string separates the elements in the environment variable.
#' @param value A string to add.
#' @noRd
add_env_value <- function(old, value, sep, first = TRUE) {
    if (!is.null(old) && nzchar(old)) {
        if (first) {
            paste0(value, sep, old)
        } else {
            paste0(old, sep, value)
        }
    } else {
        value
    }
}

get_env_sep <- function(win = ";", other = ":") {
    if (.Platform$OS.type == "windows") {
        win
    } else {
        other
    }
}
