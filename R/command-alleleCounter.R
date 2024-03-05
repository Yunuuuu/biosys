#' Run alleleCount
#'
#' The alleleCount package primarily exists to prevent code duplication between
#' some other projects, specifically AscatNGS and Battenberg.
#' @param hts_file Path to sample HTS file.
#' @param loci_file Path to loci file.
#' @param ofile Output file.
#' @param odir Output directory.
#' @param ref_fasta Path to reference fasta index file.
#' @param min_base_qual Minimum base quality. Default: 20.
#' @param min_map_qual Minimum mapping quality. Default: 35.
#' @param dense_snp Improves performance where many positions are close
#'   together.
#' @param ... Other arguments passed to alleleCounter
#' @param allelecounter_cmd Path to alleleCounter command.
#' @inheritParams run_command
#' @seealso <https://github.com/cancerit/alleleCount>
#' @export
run_allele_counter <- function(
    hts_file, loci_file, ofile, odir = getwd(),
    ref_fasta = NULL, min_base_qual = 20L,
    min_map_qual = 35L, dense_snp = TRUE, ...,
    allelecounter_cmd = NULL, envpath = NULL,
    env = NULL, abort = FALSE,
    sys_args = list(), verbose = TRUE) {
    opath <- build_opath(odir, ofile)
    status <- run_sys_command(
        allelecounter_cmd,
        name = "alleleCounter",
        args = c(
            handle_sys_arg("-l", loci_file),
            handle_sys_arg("-b", hts_file),
            handle_sys_arg("-o", opath),
            handle_sys_arg("-m", min_base_qual, format = "%d"),
            handle_sys_arg("-q", min_map_qual, format = "%d"),
            handle_sys_arg("-d", dense_snp, indicator = TRUE),
            handle_sys_arg("-r", ref_fasta),
            ...
        ),
        envpath = envpath, env = env,
        output = opath, abort = abort,
        sys_args = sys_args,
        verbose = verbose
    )
    status
}
