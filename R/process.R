#' Biosys asynchronous process management
#'
#' @description
#'  - `process_echo`: Show `Biosys` background process.
#'  - `process_is_running`: Check if the process is running.
#'  - `process_is_finished`: Check if the process is finished.
#'  - `process_kill`: Terminate the process.
#'  - `process_value`: Extract process value.
#' @name process
NULL

#' @return
#' - `process_echo`: A logical value indicates if process were running.
#' @rdname process
process_echo <- function() {
    process_refresh()
    running <- Process$running
    finished <- Process$finished
    killed <- Process$killed
    len_running <- length(running) # nolint
    len_finished <- length(finished) # nolint
    len_killed <- length(killed) # nolint
    cli::cli_inform(c(
        c_msg(
            "A total of {.val {len_running + len_finished + len_killed}}",
            "process{?es}"
        ),
        i = c_msg("Running:", style_pids(running)),
        i = c_msg("Finished:", style_pids(finished)),
        i = c_msg("Killed:", style_pids(killed))
    ))
}

#' @param x Process names or an integer vector of process ID. For
#' `process_value`, only 1 process is supporeted.
#' @return
#' - `process_is_running`: A logical value indicates if process were running.
#' @rdname process
process_is_running <- function(x) {
    process_refresh()
    pids <- setup_pids(x)
    pids %in% Process$running
}

#' @return
#' - `process_is_finished`: A logical value indicates if process were finished.
#' @rdname process
process_is_finished <- function(x) {
    process_refresh()
    pids <- setup_pids(x)
    pids %in% Process$finished
}

#' @return
#' - `process_kill`: A logical value indicates if process were killed
#'   successfully.
#' @rdname process
process_kill <- function(x) {
    process_refresh()
    pids <- setup_pids(x)
    o <- rep_len(NA, length(pids))
    finished <- pids %in% Process$finished
    if (length(finished)) {
        cli::cli_inform(c_msg(
            "Process already finished:", style_pids(pids[finished])
        ))
        o[finished] <- TRUE
    }
    killing <- pids %in% Process$running
    if (length(killing)) {
        cli::cli_inform(c_msg("Killing Process:", style_pids(pids[killing])))
        o[killing] <- tools::pskill(pids[killing])
        killed_pids <- pids[killing][o[killing]]
        killed_pids <- killed_pids[!duplicated(killed_pids)]
        Process$killed <- c(Process$killed, killed_pids)
    }
    o
}

#' @param clean A bool, indicates whether to remove collected value.
#' @return
#' - `process_value`: Value returned by process command.
#' @rdname process
process_value <- function(x, clean = TRUE) {
    assert_(x, function(x) length(x) == 1L && !is.na(x), "a scalar")
    assert_bool(clean)
    process_refresh()
    pid <- setup_pids(x)
    if (is.na(pid)) return(NA) # styler: off
    if (pid %in% Process$running) {
        cli::cli_warn(c_msg("Process", style_pids(x), "is still running"))
        return(NULL)
    }
    if (pid %in% Process$killed) {
        cli::cli_warn(c_msg("Process", style_pids(x), "has been killed"))
        return(NULL)
    }
    nm <- as.character(pid)
    if (exists(nm, envir = Process$collects)) {
        o <- Process$collects[[nm]]
        if (clean) rm(envir = Process$collects, list = nm)
        return(o)
    } else {
        cli::cli_warn(c_msg(
            "Collected value of process", style_pids(pid), "has been removed"
        ))
    }
}

################################################
Process <- new.env(parent = emptyenv())
Process$collects <- new.env(parent = emptyenv())
Process$running <- integer()
Process$finished <- integer()
Process$killed <- integer()

process_refresh <- function() {
    collects <- parallel::mccollect(Process$running, wait = FALSE)
    if (is.list(collects)) {
        rlang::env_bind(Process$collects, !!!collects)
        pids <- as.integer(names(collects))
        index <- match(Process$running, pids)
        Process$finished <- c(Process$finished, Process$running[index])
        Process$running <- Process$running[-index]
    }
}

process_add <- function(pid, name = NULL) {
    if (!is.null(name)) names(pid) <- name
    Process$running <- c(Process$running, pid)
}

process_names <- function() {
    pids <- c(Process$running, Process$finished)
    names(pids)[rlang::have_name(pids)]
}

setup_pids <- function(x, arg = rlang::caller_arg(),
                       call = rlang::caller_env()) {
    if (length(x) == 0L) {
        cli::cli_abort("empty value provided in {.arg {arg}}", call = call)
    }
    full <- c(Process$running, Process$finished, Process$killed)
    named <- rlang::have_name(full)
    named_value <- full[named]
    matches <- c(full[!named], named_value, names(named_value))
    values <- c(full[!named], named_value, named_value)
    pids <- values[match(x, matches)]
    if (anyNA(pids)) {
        cli::cli_warn(c_msg(
            "Don't know process:", style_pids(unname(x[is.na(pids)]))
        ))
    }
    pids
}

style_pids <- function(x) {
    if (length(x)) {
        named <- rlang::have_name(x)
        x[named] <- paste0(names(x[named]), " (Pid: ", x[named], ")")
        style_field(oxford_comma(x))
    } else {
        style_field("none")
    }
}
