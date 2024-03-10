#' FASTQ PAIR
#'
#' @description
#' Rewrite paired end fastq files to make sure that all reads have a mate and to
#' separate out singletons.
#'
#' Usually when you get paired end read files you have two files with a /1
#' sequence in one and a /2 sequence in the other (or a /f and /r or just two
#' reads with the same ID). However, often when working with files from a third
#' party source (e.g. the SRA) there are different numbers of reads in each file
#' (because some reads fail QC). Spades, bowtie2 and other tools break because
#' they demand paired end files have the same number of reads.
#'
#' @param fq1,fq2 A string of fastq file path.
#' @param ... `r rd_dots("fastq_pair")`. Details see: `fastq_pair(help = TRUE)`.
#' @param keep_uncompressed A bool, If `fq1` or `fq2` are compressed,
#' `fastq_pair` function will automatically uncompress them, this argument
#' controls whether we should remove the uncompressed temporary files.
#' @param compress A bool, whether compress final results.
#' @inheritParams allele_counter
#' @param kraken2 `r rd_cmd("kraken2")`.
#' @seealso <https://github.com/DerrickWood/kraken2/wiki/Manual>
#' @export
fastq_pair <- exec_build(
    command_new_name("fastq_pair"),
    fq1 = , fq2 = , ... = , keep_uncompressed = TRUE, compress = TRUE,
    help = "--help", odir = getwd(),
    setup_params = expression(
        assert_string(fq1, empty_ok = FALSE),
        assert_string(fq2, empty_ok = FALSE),
        assert_bool(keep_uncompressed),
        odir <- build_opath(odir),
        new_fq1 <- uncompress_file(fq1, exdir = odir),
        new_fq2 <- uncompress_file(fq2, exdir = odir),
        suffix <- c(".paired.fq", ".single.fq"),
        opath1 <- file_path(
            dirname(new_fq1),
            paste0(basename(new_fq1), suffix)
        ),
        opath2 <- file_path(
            dirname(new_fq2),
            paste0(basename(new_fq2), suffix)
        ),
        opath <- c(opath1, opath2),
        if (!keep_uncompressed) {
            if (!identical(fq1, new_fq1)) {
                on.exit(file.remove(new_fq1), add = TRUE)
            }
            if (!identical(fq2, new_fq2)) {
                on.exit(file.remove(new_fq2), add = TRUE)
            }
        },
        required_args <- c(new_fq1, new_fq2, ">/dev/null")
    ),
    final = expression(
        if (compress) {
            for (file in opath) {
                gzip(file, odir = odir, keep = FALSE, verbose = FALSE)
            }
        } else {
            file.rename(opath, file_path(odir, basename(opath)))
        }
    )
)
