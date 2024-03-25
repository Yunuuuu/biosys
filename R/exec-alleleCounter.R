#' Run alleleCount
#'
#' The alleleCount package primarily exists to prevent code duplication between
#' some other projects, specifically AscatNGS and Battenberg.
#'
#' @param hts_file A string of path to sample HTS file.
#' @param loci_file A string of path to loci file.
#' @param ofile A string of path to the output file.
#' @param odir A string of path to the output directory.
#' @param ... `r rd_dots("alleleCounter")`.
#' @param alleleCounter `r rd_cmd("alleleCounter")`.
#' @seealso <https://github.com/cancerit/alleleCount>
#' @inherit exec return
#' @export
allele_counter <- function(hts_file, loci_file, ofile, ..., odir = getwd(),
                           alleleCounter = NULL) {
    Execute$new(SysAlleleCounter$new(,
        cmd = alleleCounter, ...,
        hts_file = hts_file, loci_file = loci_file, ofile = ofile, odir = odir
    ))
}

SysAlleleCounter <- R6::R6Class(
    "SysAlleleCounter",
    inherit = Command,
    private = list(
        name = "alleleCounter", help = "--help",
        setup_command_params = function(hts_file, loci_file, ofile, odir) {
            opath <- build_opath(odir, ofile)
            c(
                arg_internal("-l", loci_file),
                arg_internal("-b", hts_file),
                arg_internal("-o", opath)
            )
        }
    )
)
