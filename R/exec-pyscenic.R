#' Run pyscenic
#'
#' @param subcmd Sub-Command of pyscenic.
#' @param ... `r rd_dots("pyscenic subcmd")`.
#' @param pyscenic `r rd_cmd("pyscenic")`.
#' @inherit exec return
#' @references <https://github.com/aertslab/pySCENIC>
#' @export
pyscenic <- function(subcmd = NULL, ..., pyscenic = NULL) {
    assert_string(subcmd, empty_ok = FALSE, null_ok = TRUE)
    Execute$new(SysPyscenic$new(cmd = pyscenic, ..., .subcmd = subcmd))
}

SysPyscenic <- R6::R6Class(
    "SysPyscenic",
    inherit = Command,
    private = list(name = "pyscenic", help = "--help")
)
