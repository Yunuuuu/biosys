#' Run seqkit
#' @param subcmd Sub-Command of seqkit.
#' @param ... `r rd_dots("seqkit subcmd")`.
#' @inheritParams allele_counter
#' @param seqkit `r rd_cmd("seqkit")`.
#' @seealso <https://bioinf.shenwei.me/seqkit/>
#' @export
seqkit <- function(subcmd = NULL, ...,
                   envpath = NULL, envvar = NULL, help = FALSE,
                   stdout = TRUE, stderr = TRUE, stdin = "",
                   wait = TRUE, timeout = 0L, abort = TRUE,
                   verbose = TRUE, seqkit = NULL) {
    assert_string(subcmd, empty_ok = FALSE, null_ok = TRUE)
    SysSeqkit$new()$run(
        cmd = seqkit,
        ..., subcmd = subcmd, envpath = envpath, envvar = envvar,
        help = help, stdout = stdout, stderr = stderr, stdin = stdin,
        wait = wait, timeout = timeout, abort = abort, verbose = verbose
    )
}

SysSeqkit <- R6::R6Class(
    "SysSeqkit",
    inherit = Command,
    private = list(
        names = "seqkit",
        setup_command_params = function(subcmd) subcmd,
        setup_help_params = function(subcmd) c(subcmd, "--help")
    )
)
