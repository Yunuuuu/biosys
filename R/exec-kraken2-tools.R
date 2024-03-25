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
kraken_tools <- function(script, ..., python = NULL) {
    script <- match.arg(script, KrakenToolsScripts)
    Execute$new(SysKrakenTools$new(cmd = python, ..., script = script))
}

SysKrakenTools <- R6::R6Class(
    "SysKrakenTools",
    inherit = SysPython,
    private = list(
        combine_params = function(script) {
            script <- internal_file("KrakenTools", paste0(script, ".py"))
            if (file.access(script, mode = 1L) != 0L) {
                Sys.chmod(script, "555")
            }
            c(script, super$combine_params())
        }
    )
)

KrakenToolsScripts <- c(
    "combine_kreports", "combine_mpa", "extract_kraken_reads",
    "filter_bracken_out", "fix_unmapped", "kreport2krona",
    "kreport2mpa", "make_kreport", "make_ktaxonomy"
)
