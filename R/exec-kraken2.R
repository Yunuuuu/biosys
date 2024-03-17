#' Running Kraken2
#'
#' Kraken is a taxonomic sequence classifier that assigns taxonomic labels to
#' DNA sequences. Kraken examines the k-mers within a query sequence and uses
#' the information within those k-mers to query a database. That database maps
#' k-mers to the lowest common ancestor (LCA) of all genomes known to contain a
#' given k-mer.
#'
#' @param fq1,fq2 A string of fastq file path.
#' @param ... `r rd_dots("kraken2")`. Details see: `kraken2(help = TRUE)`
#' @param ofile A string of path to save kraken2 output.
#' @param report A string of path to save kraken2 report.
#' @param classified_out A string of path to save classified sequences, which
#' should be a fastq file.
#' @param unclassified_out A string of path to save unclassified sequences,
#' which should be a fastq file.
#' @inheritParams allele_counter
#' @param kraken2 `r rd_cmd("kraken2")`.
#' @seealso <https://github.com/DerrickWood/kraken2/wiki/Manual>
#' @export
kraken2 <- function(fq1, ..., fq2 = NULL,
                    ofile = "kraken_output.txt", report = "kraken_report.txt",
                    classified_out = NULL, unclassified_out = NULL,
                    odir = getwd(), envpath = NULL, envvar = NULL, help = FALSE,
                    stdout = TRUE, stderr = TRUE, stdin = "",
                    wait = TRUE, timeout = 0L, abort = TRUE,
                    verbose = TRUE, kraken2 = NULL) {
    SysKraken2$new()$exec(
        cmd = kraken2,
        ...,
        fq1 = fq1, fq2 = fq2, ofile = ofile, report = report,
        classified_out = classified_out, unclassified_out = unclassified_out,
        odir = odir, envpath = envpath, envvar = envvar,
        help = help, stdout = stdout, stderr = stderr, stdin = stdin,
        wait = wait, timeout = timeout, abort = abort, verbose = verbose
    )
}

SysKraken2 <- R6::R6Class(
    "SysKraken2",
    inherit = Command,
    private = list(
        name = "kraken2",
        setup_command_params = function(fq1, fq2, ofile, report,
                                        classified_out, unclassified_out,
                                        odir) {
            assert_string(ofile, null_ok = TRUE)
            assert_string(report, null_ok = TRUE)
            assert_string(classified_out, null_ok = TRUE)
            # https://github.com/DerrickWood/kraken2/wiki/Manual
            # Usage of --paired also affects the --classified-out and
            # --unclassified-out options; users should provide a # character in
            # the filenames provided to those options, which will be replaced by
            # kraken2 with "_1" and "_2" with mates spread across the two files
            # appropriately. For example:
            odir <- build_opath(odir)
            if (!is.null(classified_out)) {
                if (!is.null(fq2)) {
                    classified_out <- sprintf("%s#", classified_out)
                }
                classified_out <- file_path(odir, classified_out)
            }
            if (!is.null(unclassified_out)) {
                if (!is.null(fq2)) {
                    unclassified_out <- sprintf("%s#", unclassified_out)
                }
                unclassified_out <- file_path(odir, unclassified_out)
            }
            if (!is.null(ofile)) ofile <- file_path(odir, ofile)
            if (!is.null(report)) report <- file_path(odir, report)
            opath <- c(ofile, report)
            c(
                arg_internal("--classified-out",
                    classified_out,
                    null_ok = TRUE
                ),
                arg_internal("--unclassified-out",
                    classified_out,
                    null_ok = TRUE
                ),
                arg_internal("--output", ofile, null_ok = TRUE),
                arg_internal("--report", report, null_ok = TRUE),
                if (!is.null(fq2)) "--paired", fq1, fq2
            )
        },
        setup_help_params = function() "--help"
    )
)

#' @param report A string of path or the report file returned by `kraken2`.
#' @inheritParams python
#' @export
#' @rdname kraken2
kraken2_mpa <- function(report, ..., ofile = NULL, odir = getwd(),
                        pythonpath = NULL,
                        envpath = NULL, envvar = NULL, help = FALSE,
                        stdout = TRUE, stderr = TRUE, stdin = "",
                        wait = TRUE, timeout = 0L, abort = TRUE,
                        verbose = TRUE, python = NULL) {
    SysKraken2Mpa$new()$exec(
        cmd = python,
        ..., report = report, odir = odir, pythonpath = pythonpath,
        envpath = envpath, envvar = envvar,
        help = help, stdout = stdout, stderr = stderr, stdin = stdin,
        wait = wait, timeout = timeout, abort = abort, verbose = verbose
    )
}

SysKraken2Mpa <- R6::R6Class(
    "SysKraken2",
    inherit = SysPython,
    private = list(
        setup_params = function(params) {
            kreport2mpa <- internal_file("kraken2", "kreport2mpa.py")
            if (file.access(kreport2mpa, mode = 1L) != 0L) {
                Sys.chmod(kreport2mpa, "555")
            }
            params$script <- kreport2mpa
            params
        },
        internal_params = "script",
        setup_command_params = function(script, report, ofile, odir) {
            assert_string(report)
            assert_string(ofile, null_ok = TRUE)
            ofile <- ofile %||% paste0(basename(report), ".mpa")
            opath <- build_opath(odir, ofile)
            c(
                script,
                arg_internal("-r", report),
                arg_internal("-o", opath)
            )
        },
        setup_help_params = function(script) c(script, "--help")
    )
)
