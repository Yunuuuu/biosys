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
kraken2 <- exec_build(
    command_new_name("kraken2"),
    fq1 = , ... = , fq2 = NULL,
    ofile = "kraken_output.txt", report = "kraken_report.txt",
    classified_out = NULL, unclassified_out = NULL, odir = getwd(),
    opath_symbol = quote(opath), help = "--help",
    setup_params = exprs({
        assert_string(ofile, null_ok = TRUE)
        assert_string(report, null_ok = TRUE)
        assert_string(classified_out, null_ok = TRUE)
        # https://github.com/DerrickWood/kraken2/wiki/Manual
        # Usage of --paired also affects the --classified-out and
        # --unclassified-out options; users should provide a # character in the
        # filenames provided to those options, which will be replaced by kraken2
        # with "_1" and "_2" with mates spread across the two files
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
        params <- c(
            arg_internal("--classified-out", classified_out, null_ok = TRUE),
            arg_internal("--unclassified-out", classified_out, null_ok = TRUE),
            arg_internal("--output", ofile, null_ok = TRUE),
            arg_internal("--report", report, null_ok = TRUE),
            if (!is.null(fq2)) "--paired", fq1, fq2
        )
    })
)

#' @param report A string of path or the report file returned by `kraken2`.
#' @inheritParams python
#' @export
#' @rdname kraken2
kraken2_mpa <- exec_build(
    command_new_name("python", class = "python"),
    report = , ... = , ofile = NULL, odir = getwd(), pythonpath = NULL,
    opath_symbol = quote(opath),
    help = quote(sprintf("%s --help", kreport2mpa)),
    setup_envvar = exprs({
        kreport2mpa <- internal_file("kraken2", "kreport2mpa.py")
        if (file.access(kreport2mpa, mode = 1L) != 0L) {
            Sys.chmod(kreport2mpa, "555")
        }
        envvar <- envvar_parse_path(envvar, name = "PYTHONPATH", pythonpath)
    }),
    setup_params = exprs({
        ofile <- ofile %||% paste0(basename(report), ".mpa")
        opath <- build_opath(odir, ofile)
        params <- c(
            kreport2mpa,
            arg_internal("-r", report),
            arg_internal("-o", opath)
        )
    })
)
