#' Run cellranger-count
#' @description Count gene expression (targeted or whole-transcriptome) and/or
#' feature barcode reads from a single sample and GEM well.
#' @param id A unique run id and output folder name.
#' @param fastqs Path to input FASTQ data.
#' @param transcriptome Path of folder containing 10x-compatible transcriptome
#'   reference.
#' @param sample Prefix of the filenames of FASTQs to select.
#' @param cores Set max cores the pipeline may request at one time. Only applies
#'   to local jobs.
#' @param other_args Other arguments passed to cellranger count.
#' @inheritParams run_cellranger
#' @export
run_cellranger_count <- function(id, fastqs, transcriptome = NULL, sample = NULL, cores = NULL, other_args = c("--nosecondary"), cellranger_cmd = NULL, sys_args = list(), verbose = TRUE) {
    sep <- "="
    args <- c(
        handle_sys_arg("--id", id, sep = sep),
        handle_sys_arg("--fastqs", fastqs, sep = sep),
        handle_sys_arg("--transcriptome", transcriptome, sep = sep),
        handle_sys_arg("--sample", sample, sep = sep),
        handle_sys_arg("--localcores", cores, sep = sep),
        other_args
    )
    run_cellranger(
        subcmd = "count", cellranger_args = args,
        cellranger_cmd = cellranger_cmd, sys_args = sys_args,
        verbose = verbose
    )
}

#' Run cellranger
#' @param subcmd Sub-Command of cellranger.
#' @param args Arguments passed to cellranger sub-command.
#' @param cellranger_cmd Path to cellranger command.
#' @inheritParams run_command
#' @export
run_cellranger <- function(subcmd, args = character(), cellranger_cmd = NULL, sys_args = list(), verbose = TRUE) {
    run_sys_command(
        args = c(subcmd, args),
        cmd = cellranger_cmd,
        name = "cellranger",
        sys_args = sys_args,
        verbose = verbose
    )
}
