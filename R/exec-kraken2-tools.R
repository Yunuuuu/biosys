#' KrakenTools is a suite of scripts to be used alongside the Kraken,
#' KrakenUniq, Kraken 2, or Bracken programs.
#'
#' @description These scripts are designed to help Kraken users with downstream
#' analysis of Kraken results.
#'
#' @param script Name of the kraken2 script. One of
#' `r oxford_comma(rd_format_code(KrakenToolsScripts))`
#' @param ... `r rd_dots("script")`. Details see: `kraken_tools(script, help =
#' TRUE)`.
#' @inheritParams python
#' @seealso <https://github.com/jenniferlu717/KrakenTools>
#' @export
kraken_tools <- function(script, ..., pythonpath = NULL,
                         envpath = NULL, envvar = NULL, help = FALSE,
                         stdout = TRUE, stderr = TRUE, stdin = "",
                         wait = TRUE, timeout = 0L, abort = TRUE,
                         verbose = TRUE, python = NULL) {
    script <- match.arg(script, KrakenToolsScripts)
    SysKrakenTools$new()$run(
        cmd = python, ..., script = script,
        pythonpath = pythonpath, envpath = envpath, envvar = envvar,
        help = help, stdout = stdout, stderr = stderr, stdin = stdin,
        wait = wait, timeout = timeout, abort = abort, verbose = verbose
    )
}

SysKrakenTools <- R6::R6Class(
    "SysKrakenTools",
    inherit = SysPython,
    private = list(
        setup_params = function(params) {
            script <- internal_file("KrakenTools", paste0(params$script, ".py"))
            if (file.access(script, mode = 1L) != 0L) {
                Sys.chmod(script, "555")
            }
            params$script <- script
            params
        },
        setup_help_params = function(script) c(script, "--help"),
        setup_command_params = function(script) script,
        combine_params = function(dots, params) c(params, dots)
    )
)

KrakenToolsScripts <- c(
    "combine_kreports", "combine_mpa", "extract_kraken_reads",
    "filter_bracken_out", "fix_unmapped", "kreport2krona",
    "kreport2mpa", "make_kreport", "make_ktaxonomy"
)
