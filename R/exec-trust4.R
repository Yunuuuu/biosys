#' TRUST4: immune repertoire reconstruction from bulk and single-cell RNA-seq
#' data
#'
#' @param file1 Path to bam file or fastq file.
#' @param file2 Path to the second paired-end read fastq file, only used for
#' `mode = "fastq"`.
#' @param mode One of "bam" or "fastq". If `NULL`, will be inferred from
#' `file1`.
#' @param ref_coordinate Path to the fasta file coordinate and sequence of
#' V/D/J/C genes.
#' @param ref_annot Path to detailed V/D/J/C gene reference file, such as from
#' IMGT database. (default: not used). (recommended).
#' @param ofile
#'  - `trust4`: Prefix of output files. (default: inferred from file
#'    prefix).
#'  - `trust4_imgt_annot`: Output file name.
#'  - `trust4_gene_names`: Output file name.
#' @param ... `r rd_dots("run-trust4")`. Details see:
#' <https://github.com/liulab-dfci/TRUST4>.
#' @inheritParams allele_counter
#' @param trust4 `r rd_cmd("run-trust4")`.
#' @seealso <https://github.com/liulab-dfci/TRUST4>
#' @export
trust4 <- exec_build(
    command_new_name("run-trust4"),
    cmd = "trust4",
    file1 = , ref_coordinate = , ... = , file2 = NULL, mode = NULL,
    ref_annot = NULL, ofile = NULL, odir = getwd(), help = NULL,
    setup_params = exprs({
        assert_string(file1, empty_ok = FALSE)
        if (is.null(mode)) {
            if (grepl("(fastq|fq)(\\.gz)?$", file1, perl = TRUE)) {
                mode <- "fastq"
            } else if (endsWith(file1, "bam")) {
                mode <- "bam"
            } else {
                cli::cli_abort("Cannot infer {.arg mode} from {.arg file1}")
            }
        } else {
            mode <- match.arg(mode, c("bam", "fastq"))
        }
        if (mode == "bam") {
            if (!is.null(file2)) {
                cli::cli_abort(
                    "{.arg file2} must be {.code NULL} for {.code mode = \"bam\"}"
                )
            }
            params <- arg_internal("-b", file1)
        } else {
            if (is.null(file2)) {
                params <- arg_internal("-u", file1)
            } else {
                params <- c(
                    arg_internal("-1", file1),
                    arg_internal("-2", file2)
                )
            }
        }
        odir <- build_opath(odir)
        params <- c(
            params,
            arg_internal("-f", ref_coordinate),
            arg_internal("-ref", ref_annot, null_ok = TRUE),
            arg_internal("-o", ofile, null_ok = TRUE),
            arg_internal("--od", odir)
        )
    })
)

# Normally, the file specified by "--ref" is downloaded from IMGT website, For
# example, for human, you can use command
# `perl BuildImgtAnnot.pl Homo_sapien > IMGT+C.fa`
#' @param species Species to extract IMGT annotation, details see
#' <https://www.imgt.org//download/V-QUEST/IMGT_V-QUEST_reference_directory/>.
#' @param perl Path to `perl` command.
#' @export
#' @rdname trust4
trust4_imgt_annot <- exec_build(
    command_new_name("perl"),
    species = "Homo_sapien",
    ofile = "IMGT+C.fa", odir = getwd(),
    opath_internal = quote(opath),
    setup_params = exprs({
        assert_string(species, empty_ok = FALSE)
        opath <- build_opath(odir, ofile, abs = TRUE)
        cur_dir <- getwd()
        tmp_dir <- tempdir()
        on.exit(setwd(cur_dir))
        setwd(tmp_dir)
        params <- c(
            internal_file("TRUST4", "BuildImgtAnnot.pl"),
            shQuote(species), ">", opath
        )
    })
)

#' @param imgt_annot Path of IMGT annotation file, created via
#' `trust4_imgt_annot`.
#' @export
#' @rdname trust4
trust4_gene_names <- function(imgt_annot, ofile = "bcr_tcr_gene_name.txt", odir = getwd()) {
    assert_string(imgt_annot, empty_ok = FALSE)
    opath <- build_opath(odir, ofile)
    lines <- read_lines(imgt_annot)
    gene_lines <- grep("^>", lines, value = TRUE, perl = TRUE)
    genes <- sub("^>", "", gene_lines, perl = TRUE)
    genes <- vapply(strsplit(genes, "*", fixed = TRUE),
        `[[`, character(1L), 1L, USE.NAMES = FALSE # styler: off
    )
    write_lines(genes, opath)
}
