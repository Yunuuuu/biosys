#' Running Kraken2
#'
#' @description
#' Metagenomic classification of paired-end reads from single-cell RNA
#' sequencing fastq files can be performed using any k-mer based mapper that
#' identifies a taxonomic ID for each k-mer and read. However, `SAHMI` is
#' optimized to run with `Kraken2Uniq`, which finds exact matches of candidate
#' 35-mer genomic substrings to the lowest common ancestor of genomes in a
#' reference metagenomic database. It is essential that all realistically
#' possible genomes are included as mapping references at this stage (e.g. host,
#' known vectors, etc.), or that host mappable reads are excluded. The required
#' outputs from this step are: a Kraken summary report with sample level
#' metagenomic counts, a Kraken output file with read and k-mer level taxonomic
#' classifications, an MPA-style report, and raw sequencing fastq files with
#' taxonomic classification for each read. Please see
#' <https://github.com/DerrickWood/kraken2/blob/master/docs/MANUAL.markdown> for
#' more details on installation and usage of Kraken2/KrakenUniq.
#'
#' @param fq1,fq2 Path to fastq 1 file.
#' @param ... `r rd_dots("kraken2")`. Details see: `kraken2(help = TRUE)`
#' @param classified_out A string of path to save classified sequences.
#' @inheritParams allele_counter
#' @param kraken2 `r rd_cmd("kraken2")`.
#' @seealso <https://github.com/DerrickWood/kraken2/wiki/Manual>
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
