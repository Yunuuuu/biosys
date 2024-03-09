#' Invoke a System Command
#'
#' @param cmd Command to be invoked, as a character string.
#' @param ... <[dynamic dots][rlang::dyn-dots]> Arguments passed to `cmd`.
#' @param envpath A character define the environment variables `PATH` to be
#'  added before running command.
#' @param envvar A named atomic vector define running environment variables of
#' the command, all environment variables will be replaced before running
#' command. Use `NA` to remove an environment variable.
#' @param opath Specifying the output file or directory to be removed if
#'  command running error. Note: all files in the directory will be removed, you
#'  must use this argument carefully.
#' @param abort A bool indicates whether to report error if command return
#'  non-zero status.
#' @param stdout,stderr Where output to ‘stdout’ or ‘stderr’ should be sent.
#' Possible values are:
#'
#'  - `TRUE`: print child output in R console
#'  - `FALSE`: suppress output stream
#'  - **string**: name or path of file to redirect output
#'
#' @inheritParams base::system2
#' @param wait A bool (not NA) indicating whether the R interpreter should wait
#' for the command to finish, or run it asynchronously. This will be ignored
#' (and the interpreter will always wait) if `abort = TRUE`.
#' @param verbose A logical value indicates whether to print running command
#'  message.
#' @return
#'  - if `abort=TRUE`, zero if command success, otherwise, abort error.
#'  - if `abort=FALSE` and `wait=FALSE`, always return `0`.
#'  - if `abort=FALSE` and `wait=TRUE`, exit status returned by the command.
#' @export
exec <- exec_build(NULL, cmd = "cmd", ... = , opath = NULL, opath_internal = quote(opath))
