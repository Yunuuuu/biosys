#' Run alleleCount
#'
#' The alleleCount package primarily exists to prevent code duplication between
#' some other projects, specifically AscatNGS and Battenberg.
#'
#' @param hts_file Path to sample HTS file.
#' @param loci_file Path to loci file.
#' @param ofile Output file.
#' @param odir Output directory.
#' @param ... Additional arguments passed to `alleleCounter` command.
#' @inheritParams exec
#' @param help If `TRUE`, will print the help document for this command.
#' @param alleleCounter `r rd_cmd("alleleCounter")`.
#' @seealso <https://github.com/cancerit/alleleCount>
#' @export
allele_counter <- exec_fn("alleleCounter",
    hts_file = , loci_file = , ofile = , ... = ,
    odir = getwd(), opath_internal = quote(opath), help = "--help",
    prepare = expression(
        opath <- build_opath(odir, ofile),
        required_args <- c(
            arg_internal("-l", loci_file),
            arg_internal("-b", hts_file),
            arg_internal("-o", opath)
        )
    )
)
