#' Run seqkit
#' @param subcmd Sub-Command of seqkit.
#' @param ... `r rd_dots("seqkit subcmd")`.
#' @param seqkit `r rd_cmd("seqkit")`.
#' @seealso <https://bioinf.shenwei.me/seqkit/>
#' @export
seqkit <- function(subcmd = NULL, ..., seqkit = NULL) {
    assert_string(subcmd, empty_ok = FALSE, null_ok = TRUE)
    Execute$new(SysSeqkit$new(cmd = seqkit, ..., subcmd = subcmd))
}

SysSeqkit <- R6::R6Class(
    "SysSeqkit",
    inherit = Command,
    private = list(
        name = "seqkit", help = "--help",
        combine_params = function(subcmd) {
            c(subcmd, .subset2(private, ".dots"), .subset2(private, ".params"))
        }
    )
)
