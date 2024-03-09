#' Run cellranger
#' @param subcmd Sub-Command of cellranger.
#' @param ... `r rd_dots("cellranger subcmd")`.
#' @inheritParams allele_counter
#' @param cellranger `r rd_cmd("cellranger")`.
#' @export
cellranger <- exec_build(
    command_new_name("cellranger"),
    subcmd = , ... = , help = "--help",
    setup_params = expression(
        assert_string(subcmd, empty_ok = FALSE, null_ok = TRUE),
        required_args <- subcmd
    )
)
