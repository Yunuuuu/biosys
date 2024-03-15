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
#' @inheritParams exec
#' @param help If `TRUE`, will print the help document for this command.
#' @param alleleCounter `r rd_cmd("alleleCounter")`.
#' @seealso <https://github.com/cancerit/alleleCount>
#' @export
allele_counter <- function(hts_file, loci_file, ofile, ..., odir = getwd(),
                           envpath = NULL, envvar = NULL, help = FALSE,
                           stdout = TRUE, stderr = TRUE, stdin = "",
                           wait = TRUE, timeout = 0L, abort = TRUE,
                           verbose = TRUE, alleleCounter = NULL) {
    SysAlleleCounter$new()$exec(
        cmd = alleleCounter,
        ...,
        hts_file = hts_file, loci_file = loci_file,
        ofile = ofile, odir = odir,
        envpath = envpath, envvar = envvar,
        help = help, stdout = stdout, stderr = stderr, stdin = stdin,
        wait = wait, timeout = timeout, abort = abort, verbose = verbose
    )
}

SysAlleleCounter <- R6::R6Class(
    "SysAlleleCounter",
    inherit = SysName,
    private = list(
        name = "alleleCounter",
        setup_command_params = function(hts_file, loci_file, ofile, odir) {
            opath <- build_opath(odir, ofile)
            private$insert_param("opath", opath)
            c(
                arg_internal("-l", loci_file),
                arg_internal("-b", hts_file),
                arg_internal("-o", opath)
            )
        },
        setup_help_params = function() "--help"
    )
)
