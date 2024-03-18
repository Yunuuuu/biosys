#' Run cellranger
#' @param subcmd Sub-Command of cellranger.
#' @param ... `r rd_dots("cellranger subcmd")`.
#' @inheritParams allele_counter
#' @param cellranger `r rd_cmd("cellranger")`.
#' @export
cellranger <- function(subcmd = NULL, ...,
                       envpath = NULL, envvar = NULL, help = FALSE,
                       stdout = TRUE, stderr = TRUE, stdin = "",
                       wait = TRUE, timeout = 0L, abort = TRUE,
                       verbose = TRUE, cellranger = NULL) {
    assert_string(subcmd, empty_ok = FALSE, null_ok = TRUE)
    SysCellranger$new()$run(
        cmd = cellranger,
        ..., subcmd = subcmd, envpath = envpath, envvar = envvar,
        help = help, stdout = stdout, stderr = stderr, stdin = stdin,
        wait = wait, timeout = timeout, abort = abort, verbose = verbose
    )
}

SysCellranger <- R6::R6Class(
    "SysCellranger",
    inherit = Command,
    private = list(
        names = "cellranger",
        setup_command_params = function(subcmd) subcmd,
        setup_help_params = function(subcmd) c(subcmd, "--help")
    )
)
