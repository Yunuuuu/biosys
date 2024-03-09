#' Running Kraken2
#'
#' Kraken is a taxonomic sequence classifier that assigns taxonomic labels to
#' DNA sequences. Kraken examines the k-mers within a query sequence and uses
#' the information within those k-mers to query a database. That database maps
#' k-mers to the lowest common ancestor (LCA) of all genomes known to contain a
#' given k-mer.
#'
#' @param fq1,fq2 Path to fastq 1 file.
#' @param ... `r rd_dots("kraken2")`. Details see: `kraken2(help = TRUE)`
#' @param classified_out A string of path to save classified sequences.
#' @inheritParams allele_counter
#' @param kraken2 `r rd_cmd("kraken2")`.
#' @seealso <https://github.com/DerrickWood/kraken2/wiki/Manual>
#' @export
kraken2 <- exec_build(
    command_new_name("kraken2"),
    fq1 = , ... = , fq2 = NULL, classified_out = NULL,
    help = "--help", setup_params = expression(
        assert_string(classified_out, null_ok = TRUE),
        # https://github.com/DerrickWood/kraken2/wiki/Manual
        # Usage of --paired also affects the --classified-out and
        # --unclassified-out options; users should provide a # character in the
        # filenames provided to those options, which will be replaced by kraken2
        # with "_1" and "_2" with mates spread across the two files
        # appropriately. For example:
        if (!is.null(fq2) && !is.null(classified_out)) {
            classified_out <- sprintf("%s#", classified_out)
        },
        classified_out <- arg_internal(
            "--classified-out", classified_out,
            null_ok = TRUE
        ),
        required_args <- c(classified_out, fq1, fq2)
    )
)

#' @param report A string of path or the report file returned by `kraken2`.
#' @inheritParams python
#' @export
#' @rdname kraken2
kraken2_mpa <- exec_build(
    command_new_name("python", class = "python"),
    report = , ofile = NULL, odir = getwd(), pythonpath = NULL,
    opath_internal = quote(opath),
    setup_envvar = expression(
        envvar <- envvar_parse_path(envvar, name = "PYTHONPATH", pythonpath)
    ),
    setup_params = expression(
        ofile <- ofile %||% paste0(basename(report), ".mpa"),
        kreport2mpa <- internal_file("kraken2", "kreport2mpa.py"),
        if (file.access(kreport2mpa, mode = 1L) != 0L) {
            Sys.chmod(kreport2mpa, "555")
        },
        opath <- build_opath(odir, ofile),
        required_args <- c(
            kreport2mpa,
            arg_internal("-r", report),
            arg_internal("-o", opath),
            "--intermediate-ranks"
        )
    )
)
