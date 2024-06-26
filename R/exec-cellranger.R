#' Run cellranger
#' @param subcmd Sub-Command of cellranger.
#' @param ... `r rd_dots("cellranger subcmd")`.
#' @param cellranger `r rd_cmd("cellranger")`.
#' @inherit exec return
#' @export
cellranger <- function(subcmd = NULL, ..., cellranger = NULL) {
    assert_string(subcmd, empty_ok = FALSE, null_ok = TRUE)
    Execute$new(SysCellranger$new(cmd = cellranger, ..., .subcmd = subcmd))
}

SysCellranger <- R6::R6Class(
    "SysCellranger",
    inherit = Command,
    private = list(name = "cellranger", help = "--help")
)
