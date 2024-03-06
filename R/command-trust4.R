#' TRUST4: immune repertoire reconstruction from bulk and single-cell RNA-seq
#' data
#'
#' @param file1 Path to bam file or fastq file.
#' @param file2 Path to the second paired-end read fastq file, only used for
#' `type = "fastq"`.
#' @param type One of "bam" or "fastq". If `NULL`, will be inferred from
#' `file1`.
#' @param ref_coordinate Path to the fasta file coordinate and sequence of
#' V/D/J/C genes.
#' @param ref_annot Path to detailed V/D/J/C gene reference file, such as from
#' IMGT database. (default: not used). (recommended).
#' @param ofile
#'  - `run_trust4`: Prefix of output files. (default: inferred from file
#'    prefix).
#'  - `trust4_imgt_annot`: Output file name.
#'  - `trust4_gene_names`: Output file name.
#' @inheritParams run_allele_counter
#' @param ... Other arguments passed to `run-trust4`. Details see:
#' <https://github.com/liulab-dfci/TRUST4>.
#' @param trust4_cmd Path to `run-trust4` command.
#' @inheritParams run_command
#' @seealso <https://github.com/liulab-dfci/TRUST4>
#' @export
run_trust4 <- function(file1, ref_coordinate, ..., file2 = NULL, type = NULL, ref_annot = NULL, ofile = NULL, odir = getwd(), trust4_cmd = NULL, envpath = NULL, env = NULL, abort = FALSE, sys_args = list(), verbose = TRUE) {
    assert_string(file1, empty_ok = FALSE)
    if (is.null(type)) {
        if (grepl("(fastq|fq)(\\.gz)?$", file1, perl = TRUE)) {
            type <- "fastq"
        } else if (endsWith(file1, "bam")) {
            type <- "bam"
        } else {
            cli::cli_abort("Cannot infer {.arg type} from {.arg file1}")
        }
    } else {
        type <- match.arg(type, c("bam", "fastq"))
    }
    if (type == "bam") {
        if (!is.null(file2)) {
            cli::cli_abort(
                "{.arg file2} must be {.code NULL} for {.code type = \"bam\"}"
            )
        }
        args <- handle_sys_arg("-b", file1, null_ok = FALSE)
    } else {
        if (is.null(file2)) {
            args <- handle_sys_arg("-u", file1, null_ok = FALSE)
        } else {
            args <- c(
                handle_sys_arg("-1", file1, null_ok = FALSE),
                handle_sys_arg("-2", file2)
            )
        }
    }
    odir <- build_opath(odir)
    args <- c(
        args,
        handle_sys_arg("-f", ref_coordinate, null_ok = FALSE),
        handle_sys_arg("-ref", ref_annot),
        handle_sys_arg("-o", ofile),
        handle_sys_arg("--od", odir),
        ...
    )
    run_sys_command(
        trust4_cmd,
        name = "run-trust4",
        args = args,
        envpath = envpath, env = env,
        output = NULL, abort = abort,
        sys_args = sys_args,
        verbose = verbose
    )
}

# Normally, the file specified by "--ref" is downloaded from IMGT website, For
# example, for human, you can use command
# `perl BuildImgtAnnot.pl Homo_sapien > IMGT+C.fa`
#' @param species Species to extract IMGT annotation, details see
#' <https://www.imgt.org//download/V-QUEST/IMGT_V-QUEST_reference_directory/>.
#' @param perl_cmd Path to `perl` command.
#' @export
#' @rdname run_trust4
trust4_imgt_annot <- function(species = "Homo_sapien", ofile = "IMGT+C.fa", odir = getwd(), perl_cmd = NULL, envpath = NULL, env = NULL, abort = FALSE, sys_args = list(), verbose = TRUE) {
    assert_string(species, empty_ok = FALSE)
    opath <- build_opath(odir, ofile, abs = TRUE)
    cur_dir <- getwd()
    tmp_dir <- tempdir()
    on.exit(setwd(cur_dir))
    setwd(tmp_dir)
    run_sys_command(
        perl_cmd,
        name = "perl",
        args = c(
            internal_file("extdata", "TRUST4", "BuildImgtAnnot.pl"),
            shQuote(species), ">", opath
        ),
        envpath = envpath, env = env,
        output = opath, abort = abort,
        sys_args = sys_args,
        verbose = verbose
    )
}

#' @param imgt_annot Path of IMGT annotation file, created via
#' `trust4_imgt_annot`.
#' @export
#' @rdname run_trust4
trust4_gene_names <- function(imgt_annot, ofile = "bcr_tcr_gene_name.txt", odir = getwd()) {
    assert_string(imgt_annot, empty_ok = FALSE)
    opath <- build_opath(odir, ofile)
    lines <- read_lines(imgt_annot)
    gene_lines <- grep("^\\^", lines, value = TRUE, perl = TRUE)
    genes <- sub("^\\^", "", gene_lines, perl = TRUE)
    genes <- vapply(strsplit(genes, "*", fixed = TRUE),
        `[[`, character(1L), 1L, USE.NAMES = FALSE # styler: off
    )
    write_lines(genes, opath)
}
