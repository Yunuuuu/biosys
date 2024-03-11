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
#' @param keep_decompressed A bool, If `fq1` or `fq2` are compressed,
#' `fastq_pair` function will automatically decompress them, this argument
#' controls whether we should remove the decompressed temporary files.
#' @param keep_unpaired A bool indicates whether to keep the unpaired sequence.
#' @param compress A bool, whether compress final results.
#' @inheritParams allele_counter
#' @param fastq_pair `r rd_cmd("fastq_pair")`.
#' @seealso <https://github.com/linsalrob/fastq-pair>
#' @export
fastq_pair <- exec_build(
    command_new_name("fastq_pair"),
    fq1 = , fq2 = , ... = ,
    keep_decompressed = FALSE, keep_unpaired = TRUE, compress = TRUE,
    odir = getwd(), opath_internal = quote(opath), help = "--help",
    setup_params = exprs({
        assert_string(fq1, empty_ok = FALSE)
        assert_string(fq2, empty_ok = FALSE)
        assert_bool(keep_decompressed)
        odir <- build_opath(odir)
        new_fq1 <- decompress_file(fq1, exdir = odir)
        new_fq2 <- decompress_file(fq2, exdir = odir)
        suffix <- c(".paired.fq", ".single.fq")
        opath1 <- file_path(
            dirname(new_fq1),
            paste0(basename(new_fq1), suffix)
        )
        opath2 <- file_path(
            dirname(new_fq2),
            paste0(basename(new_fq2), suffix)
        )
        opath <- c(opath1, opath2)
        if (!keep_decompressed) {
            if (!identical(fq1, new_fq1)) {
                on.exit(file.remove(new_fq1), add = TRUE)
            }
            if (!identical(fq2, new_fq2)) {
                on.exit(file.remove(new_fq2), add = TRUE)
            }
        }
        params <- c(new_fq1, new_fq2, ">/dev/null")
    }),
    final = exprs({
        if (status != 0L) return(status) # styler: off
        if (!keep_unpaired) {
            file.remove(opath[c(2L, 4L)])
            opath <- opath[c(1L, 3L)]
        }
        if (compress) {
            for (file in opath) {
                compress("gzip", file,
                    odir = odir, keep = FALSE, verbose = FALSE
                )
            }
        } else {
            file.rename(opath, file_path(odir, basename(opath)))
        }
    })
)

#' @param fastq_files A character of the fastq file paths.
#' @rdname fastq_pair
#' @export
fastq_read_pair <- function(fastq_files) {
    header_list <- lapply(fastq_files, function(file) {
        header <- read_lines(file, n = 1L)
        strsplit(header, ":| ", perl = TRUE)[[1L]]
    })
    # @HWI-ST1276:71:C1162ACXX:1:1101:1208:2458 2:N:0:CGATGT
    # HWI-ST1276 <- Unique instrument name
    # 71 <- Run ID
    # C1162ACXX <- Flowcell ID
    # 1 <- Flowcell lane
    # 1101 <- Tile number within the flowcell lane
    # 1208 <- 'x'-coordinate of the cluster within the tile
    # 2458 <- 'y'-coordinate of the cluster within the tile

    # 2 <- Member of a pair,1 or 2 (paired-end or mate-pair reads only)
    # N <- Y if the read fails filter (read is bad), N otherwise
    # 0 <- 0 when none of the control bits are on,otherwise it is an even number
    # CGATGT -> Index sequence

    # In Illumina data, read group IDs are composed using the flowcell name and
    # lane number, making them a globally unique identifier across all
    # sequencing data in the world.
    # Use for BQSR: ID is the lowest denominator that differentiates factors
    # contributing to technical batch effects: therefore, a read group is
    # effectively treated as a separate run of the instrument in data processing
    # steps such as base quality score recalibration (unless you have PU
    # defined), since they are assumed to share the same error model.

    # https://gatk.broadinstitute.org/hc/en-us/articles/360035890671-Read-groups
    # https://samtools.github.io/hts-specs/SAMv1.pdf
    # https://angus.readthedocs.io/en/2017/Read_group_info.html
    # header <- header_list[[1L]]
    # id <- sub("^@", "", paste(header[3:4], collapse = "."), perl = TRUE)
    # flowcell_id <- header[[3L]]
    # lane_id <- header[[4L]]

    # Platform/technology used to produce the reads. Valid values: CAPILLARY,
    # DNBSEQ (MGI/BGI), ELEMENT, HELICOS, ILLUMINA, IONTORRENT, LS454, ONT
    # (Oxford Nanopore), PACBIO (Pacific Bio-sciences), SOLID, and ULTIMA. This
    # field should be omitted when the technology is not in this list (though
    # the PM field may still be present in this case) or is unknown
    # platform_id <- "ILLUMINA"

    # extract pair_id ----------------------------
    vapply(header_list, function(header) as.integer(header[[8L]]),
        integer(1L), USE.NAMES = FALSE # styler: off
    )
}
