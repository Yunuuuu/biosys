#' Invoke a System Command
#'
#' @param cmd Command to be invoked, as a character string.
#' @param ... `r rd_dots("cmd")`.
#' @param envpath A character define the environment variables `PATH` to be
#'  `added` before running command. See [withr::with_path].
#' @param envvar A named atomic vector define running environment variables of
#' the command, all environment variables will be `replaced` before running
#' command. See [withr::with_envvar].
#' @param opath Specifying the output file or directory to be removed if
#'  command running error. Note: all files in the directory will be removed, you
#'  must use this argument carefully.
#' @param stdout,stderr Where output to ‘stdout’ or ‘stderr’ should be sent.
#' Possible values are:
#'
#'  - `TRUE`: print child output in R console
#'  - `FALSE`: suppress output stream
#'  - **string**: name or path of file to redirect output
#'
#' @inheritParams base::system2
#' @param wait A bool (not NA) indicating whether the R interpreter should wait
#' for the command to finish, or run it asynchronously  Only used when
#' `help=FALSE`. When wait is not `TRUE`, it can also be a string to name the
#' process, the asynchronous process can be managed with [process] tools.
#' @param abort A bool indicates whether to report error if command return
#'  non-zero status.
#' @param verbose A bool indicates whether to print running command message.
#' @return
#'  - if `wait=FALSE`, the process ID.
#'  - if `abort=TRUE` and `wait=TRUE`, zero if command success, otherwise, abort
#'    error.
#'  - if `abort=FALSE` and `wait=TRUE`, exit status returned by the command.
#' @export
exec <- function(cmd, ..., envpath = NULL, envvar = NULL, opath = NULL,
                 stdout = TRUE, stderr = TRUE, stdin = "",
                 wait = TRUE, timeout = 0L, abort = TRUE, verbose = TRUE) {
    SysExec$new()$run(
        cmd = cmd,
        ...,
        opath = opath, envpath = envpath, envvar = envvar,
        help = FALSE, stdout = stdout, stderr = stderr, stdin = stdin,
        wait = wait, timeout = timeout, abort = abort, verbose = verbose
    )
}

SysExec <- R6::R6Class("SysExec",
    inherit = Sys,
    private = list(setup_opath = function(opath) opath)
)
