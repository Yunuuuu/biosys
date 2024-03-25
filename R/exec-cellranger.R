#' Run cellranger
#' @param subcmd Sub-Command of cellranger.
#' @param ... `r rd_dots("cellranger subcmd")`.
#' @param cellranger `r rd_cmd("cellranger")`.
#' @export
cellranger <- function(subcmd = NULL, ..., cellranger = NULL) {
    assert_string(subcmd, empty_ok = FALSE, null_ok = TRUE)
    Execute$new(SysCellranger$new(cmd = cellranger, ..., subcmd = subcmd))
}

SysCellranger <- R6::R6Class(
    "SysCellranger",
    inherit = Command,
    private = list(
        name = "cellranger", help = "--help",
        combine_params = function(subcmd) {
            c(subcmd, super$combine_params())
        }
    )
)
