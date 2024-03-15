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
trust4 <- function(file1, ref_coordinate, ..., file2 = NULL, mode = NULL,
                   ref_annot = NULL, ofile = NULL, odir = getwd(),
                   envpath = NULL, envvar = NULL, help = FALSE,
                   stdout = TRUE, stderr = TRUE, stdin = "",
                   wait = TRUE, timeout = 0L, abort = TRUE,
                   verbose = TRUE, perl = NULL) {
    SysTrust4$new()$exec(
        cmd = perl,
        ..., file1 = file1, ref_coordinate = ref_coordinate,
        file2 = file2, mode = mode, ref_annot = ref_annot,
        ofile = ofile, odir = odir, envpath = envpath, envvar = envvar,
        help = help, stdout = stdout, stderr = stderr, stdin = stdin,
        wait = wait, timeout = timeout, abort = abort, verbose = verbose
    )
}

SysTrust4 <- R6::R6Class(
    "SysTrust4",
    inherit = SysName,
    private = list(
        name = "trust4",
        setup_command_params = function(file1, file2, mode, ref_annot,
                                        ofile, odir) {
            assert_string(file1, empty_ok = FALSE)
            if (is.null(mode)) {
                if (grepl("(fastq|fq)(\\.gz)?$", file1, perl = TRUE)) {
                    mode <- "fastq"
                } else if (endsWith(file1, "bam")) {
                    mode <- "bam"
                } else {
                    cli::cli_abort(c(
                        "Cannot infer {.arg mode} from {.arg file1}",
                        i = "Please specify {.arg mode} manually"
                    ))
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
            c(
                params,
                arg_internal("-f", ref_coordinate),
                arg_internal("-ref", ref_annot, null_ok = TRUE),
                arg_internal("-o", ofile, null_ok = TRUE),
                arg_internal("--od", odir)
            )
        },
        setup_help_params = function() NULL
    )
)

# Normally, the file specified by "--ref" is downloaded from IMGT website, For
# example, for human, you can use command
# `perl BuildImgtAnnot.pl Homo_sapien > IMGT+C.fa`
#' @param species Species to extract IMGT annotation, details see
#' <https://www.imgt.org//download/V-QUEST/IMGT_V-QUEST_reference_directory/>.
#' @inheritParams perl
#' @export
#' @rdname trust4
trust4_imgt_annot <- function(species = "Homo_sapien", ...,
                              ofile = "IMGT+C.fa", odir = getwd(),
                              envpath = NULL, envvar = NULL, help = FALSE,
                              stdout = TRUE, stderr = TRUE, stdin = "",
                              wait = TRUE, timeout = 0L, abort = TRUE,
                              verbose = TRUE, perl = NULL) {
    SysTrust4ImgtAnnot$new()$exec(
        cmd = perl,
        ..., species = species, ofile = ofile, odir = odir,
        envpath = envpath, envvar = envvar,
        help = help, stdout = stdout, stderr = stderr, stdin = stdin,
        wait = wait, timeout = timeout, abort = abort, verbose = verbose
    )
}

SysTrust4ImgtAnnot <- R6::R6Class(
    "SysTrust4ImgtAnnot",
    inherit = SysPerl,
    private = list(
        setup_params = function(params) {
            build_imgt_annot <- internal_file("TRUST4", "BuildImgtAnnot.pl")
            if (file.access(build_imgt_annot, mode = 1L) != 0L) {
                Sys.chmod(build_imgt_annot, "555")
            }
            params$script <- build_imgt_annot
            params
        },
        setup_wd = function() tempdir(),
        setup_command_params = function(species, ofile, odir) {
            assert_string(species, empty_ok = FALSE)
            opath <- build_opath(odir, ofile, abs = TRUE)
            c(private$get_param("script"), shQuote(species), ">", opath)
        },
        setup_help_params = function() c(private$get_param("script")),
        add_dots = FALSE
    )
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
