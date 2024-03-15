#' Python is a programming language that lets you work quickly and integrate
#' systems more effectively.
#'
#' @param ... `r rd_dots("python")`. Details see: `python(help = TRUE)`
#' @param pythonpath A character define the environment variables `PYTHONPATH`
#'  to be added before running command.
#' @inheritParams allele_counter
#' @param python `r rd_cmd("python")`.
#' @seealso <https://www.python.org/>
#' @export
python <- function(...,
                   pythonpath = NULL,
                   envpath = NULL, envvar = NULL, help = FALSE,
                   stdout = TRUE, stderr = TRUE, stdin = "",
                   wait = TRUE, timeout = 0L, abort = TRUE,
                   verbose = TRUE, python = NULL) {
    SysPython$new()$exec(
        cmd = python, ..., pythonpath = pythonpath,
        envpath = envpath, envvar = envvar,
        help = help, stdout = stdout, stderr = stderr, stdin = stdin,
        wait = wait, timeout = timeout, abort = abort, verbose = verbose
    )
}

SysPython <- R6::R6Class(
    "SysPython",
    inherit = SysName,
    private = list(
        name = "python",
        setup_envvar = function(envvar, pythonpath) {
            envvar_parse_path(envvar, name = "PYTHONPATH", pythonpath)
        },
        command_locate_by_name = function() {
            python2 <- Sys.which("python2")
            python3 <- Sys.which("python3")
            if (nzchar(python2) && !nzchar(python3)) {
                python2
            } else {
                python3
            }
        },
        setup_help_params = function() "--help"
    )
)
