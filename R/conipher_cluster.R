#' Running CONIPHER Clustering
#'
#' @param mut_tsv A mutation table in long format (mutations x tumour regions).
#' @param seg_tsv A copy number segment .tsv used for plotting only.
#' @param case_id A string specifying the tumour case ID.
#' @param output_dir Path specifying the results output directory.
#' @param subclonal_copy_correction should subclonal copy number correction be
#'   used.
#' @param only_truncal_subclonal_copy_correction should only truncal subclonal
#'   copy number correction be used.
#' @param run_pyclone A logical, whether to run pyclone.
#' @param pyclone_cmd The path of pyclone.
#' @param pyclone_yaml A template yaml file for pyclone.
#' @param min_cluster_size minimum number of mutations in a cluster to be
#'   considered.
#' @param multiple_test_correction should multiple testing correction be applied
#' for the copy number correcting mutations.
#' @param clean_clusters should clusters be cleaned and merged?
#' @param clonal_cutOff lower threshold CCF to be considered clonal.
#' @param propClonal_threshold proportion of cluster that needs to be considered
#'   clonal to merge.
#' @param fix_absentCCFs should CCF of absent mutations be set to zero?
#' @param driver_filter what filter to use for drivers.
#' @param burn_in burn-in for DP clustering.
#' @param seed Random seed for pyclone.
#' @param cores Number of cores allocated to run script in parallel.
#' @inheritParams run_command
#' @seealso https://github.com/McGranahanLab/CONIPHER-wrapper
#' @export
run_conipher_cluster <- function(
    mut_tsv, seg_tsv, case_id, output_dir = getwd(),
    subclonal_copy_correction = TRUE,
    only_truncal_subclonal_copy_correction = TRUE,
    run_pyclone = TRUE, pyclone_cmd = NULL, pyclone_yaml = NULL,
    min_cluster_size = 5L, multiple_test_correction = TRUE,
    clean_clusters = TRUE, clonal_cutOff = 0.9, propClonal_threshold = 0.25,
    fix_absentCCFs = TRUE, driver_filter = "1A,1,2A",
    burn_in = 1000L, seed = 1024L, cores = 1L,
    envpath = NULL, env = NULL, sys_args = list()) {
    mut_tsv <- normalizePath(mut_tsv, mustWork = TRUE)
    seg_tsv <- normalizePath(seg_tsv, mustWork = TRUE)
    create_dir(output_dir)
    run_sys_command(
        file.path(R.home("bin"), "Rscript"),
        name = NULL,
        args = c(
            system.file("extdata", "CONIPHER", "run_clustering.R",
                package = "biomisc"
            ),
            sprintf("--case_id %s", case_id),
            "--working_dir", output_dir,
            "--script_dir", system.file("extdata",
                "CONIPHER",
                package = "biomisc"
            ),
            sprintf("--input_tsv %s", mut_tsv),
            sprintf("--input_seg_tsv %s", seg_tsv),
            "--subclonal_copy_correction", subclonal_copy_correction,
            "--only_truncal_subclonal_copy_correction",
            only_truncal_subclonal_copy_correction,
            sprintf("--pyclone %s", pyclone_cmd),
            "--run_pyclone", run_pyclone,
            sprintf(
                "--pyclone_yaml %s",
                pyclone_yaml %||% system.file("extdata",
                    "CONIPHER", "template.config.yaml",
                    package = "biomisc"
                )
            ),
            sprintf("--min_cluster_size %d", min_cluster_size),
            "--multiple_test_correction", multiple_test_correction,
            "--clean_clusters", clean_clusters,
            sprintf("--clonal_cutOff %s", clonal_cutOff),
            sprintf("--propClonal_threshold %s", propClonal_threshold),
            "--fix_absentCCFs", fix_absentCCFs,
            sprintf("--driver_filter %s", driver_filter),
            sprintf("--burn_in %d", burn_in),
            sprintf("--seed %d", seed),
            sprintf("--nProcs %d", cores)
        ),
        sys_args = sys_args,
        verbose = FALSE
    )
}
