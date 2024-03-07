#' Run cellranger
#' @param subcmd Sub-Command of cellranger.
#' @param ... Additional arguments passed to `cellranger subcmd` command.
#' @param cellranger Path to cellranger command.
#' @inheritParams exec
#' @export
cellranger <- exec_fn("cellranger",
    subcmd = , ... = ,
    before = expression(
        assert_string(subcmd, empty_ok = FALSE, null_ok = TRUE),
        required_args <- subcmd
    )
)
