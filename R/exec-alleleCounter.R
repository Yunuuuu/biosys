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
allele_counter <- exec_build(
    command_new_name("alleleCounter"),
    hts_file = , loci_file = , ofile = , ... = ,
    odir = getwd(), opath_symbol = quote(opath), help = "--help",
    setup_params = exprs({
        opath <- build_opath(odir, ofile)
        params <- c(
            arg_internal("-l", loci_file),
            arg_internal("-b", hts_file),
            arg_internal("-o", opath)
        )
    })
)
