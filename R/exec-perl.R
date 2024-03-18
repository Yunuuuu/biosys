#' Perl is a highly capable, feature-rich programming language with over 36
#' years of development. 
#'
#' @param ... `r rd_dots("perl")`. Details see: `perl(help = TRUE)`
#' @inheritParams allele_counter
#' @param perl `r rd_cmd("perl")`.
#' @seealso <https://www.perl.org/>
#' @export
perl <- function(...,
                 envpath = NULL, envvar = NULL, help = FALSE,
                 stdout = TRUE, stderr = TRUE, stdin = "",
                 wait = TRUE, timeout = 0L, abort = TRUE,
                 verbose = TRUE, perl = NULL) {
    SysPerl$new()$run(
        cmd = perl, ...,
        envpath = envpath, envvar = envvar,
        help = help, stdout = stdout, stderr = stderr, stdin = stdin,
        wait = wait, timeout = timeout, abort = abort, verbose = verbose
    )
}

SysPerl <- R6::R6Class(
    "SysPerl",
    inherit = Command,
    private = list(
        names = "perl",
        setup_help_params = function() "--help"
    )
)
