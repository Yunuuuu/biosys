#' Run cellranger
#' @param subcmd Sub-Command of cellranger.
#' @param ... Additional arguments passed to `cellranger subcmd` command.
#' @inheritParams allele_counter
#' @param cellranger Path to cellranger command.
#' @export
cellranger <- exec_fn("cellranger",
    subcmd = , ... = , help = "--help",
    prepare = expression(
        assert_string(subcmd, empty_ok = FALSE, null_ok = TRUE),
        required_args <- subcmd
    )
)
