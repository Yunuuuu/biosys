#' Run GISTIC2
#' @description The GISTIC module identifies regions of the genome that are
#' significantly amplified or deleted across a set of samples. Each aberration
#' is assigned a G-score that considers the amplitude of the aberration as well
#' as the frequency of its occurrence across samples. False Discovery Rate
#' q-values are then calculated for the aberrant regions, and regions with
#' q-values below a user-defined threshold are considered significant. For each
#' significant region, a "peak region" is identified, which is the part of the
#' aberrant region with greatest amplitude and frequency of alteration. In
#' addition, a "wide peak" is determined using a leave-one-out algorithm to
#' allow for errors in the boundaries in a single sample. The "wide peak"
#' boundaries are more robust for identifying the most likely gene targets in
#' the region. Each significantly aberrant region is also tested to determine
#' whether it results primarily from broad events (longer than half a chromosome
#' arm), focal events, or significant levels of both. The GISTIC module reports
#' the genomic locations and calculated q-values for the aberrant regions. It
#' identifies the samples that exhibit each significant amplification or
#' deletion, and it lists genes found in each "wide peak" region. 
#' @param seg A data.frame of segmented data.
#' @param refgene Path to reference genome data input file (REQUIRED, see below
#' for file description). 
#' @param outdir The output directory.
#' @param t_amp Threshold for copy number amplifications. Regions with a copy
#' number gain above this positive value are considered amplified. Regions with
#' a copy number gain smaller than this value are considered noise and set to 0.
#' (DEFAULT=0.1). 
#' @param t_del Threshold for copy number deletions. Regions with a copy number
#' loss below the negative of this positive value are considered deletions.
#' Regions with a smaller copy number loss are considered noise and set to 0.
#' (DEFAULT=0.1). 
#' @param qv_thresh Significance threshold for q-values. Regions with q-values
#' below this number are considered significant. (DEFAULT=0.25) 
#' @param remove_XY Flag indicating whether to remove data from the sex
#' chromosomes before analysis. Allowed values= {1,0}. (DEFAULT=1, remove X,Y). 
#' @param run_broad_analysis Flag indicating that an additional broad-level
#' analysis should be performed. Allowed values = {1,0}. (DEFAULT = 0, no broad
#' analysis). 
#' @param broad_len_cutoff Threshold used to distinguish broad from focal
#' events, given in units of fraction of chromosome arm. (DEFAULT = 0.98).
#' @param maxseg Maximum number of segments allowed for a sample in the input
#' data. Samples with more segments than this threshold are excluded from the
#' analysis. (DEFAULT=2500).
#' @param conf_level Confidence level used to calculate the region containing a
#' driver. (DEFAULT=0.75). 
#' @param genegistic Flag indicating that the gene GISTIC algorithm should be
#' used to calculate the significance of deletions at a gene level instead of a
#' marker level. Allowed values= {1,0}. (DEFAULT=0, no gene GISTIC). 
#' @param savegene  Flag indicating that gene tables should be saved. Allowed
#' values= {1,0}. (DEFAULT=0, don't save gene tables) 
#' @param arm_peeloff Flag set to enable arm-level peel-off of events during
#' peak definition. The arm-level peel-off enhancement to the arbitrated
#' peel-off method assigns all events in the same chromosome arm of the same
#' sample to a single peak. It is useful when peaks are split by noise or
#' chromothripsis. Allowed values= {1,0}. (DEFAULT=0, use normal arbitrated
#' peel-off).
#' @param gistic2_verbose Integer value indicating the level of verbosity to use
#' in the program execution log. Suggested values = {0,10,20,30}. 0 sets no
#' verbosity; 30 sets high level of verbosity. (DEFAULT=0) 
#' @param ... Other arguments passed to gistic2. Details see: <https://broadinstitute.github.io/gistic2/>.
#' @param gistic2_cmd Path to gistic2 command. 
#' @seealso <https://broadinstitute.github.io/gistic2/>
#' @inheritParams run_command
#' @export 
run_gistic2 <- function(seg, refgene, outdir = getwd(), t_amp = 0.1, t_del = 0.1, qv_thresh = 0.25, remove_XY = TRUE, run_broad_analysis = FALSE, broad_len_cutoff = 0.98, maxseg = 2500L, conf_level = 0.75, genegistic = FALSE, savegene = FALSE, arm_peeloff = FALSE, ..., gistic2_cmd = NULL, gistic2_verbose = 0L, env = NULL, sys_args = list(), verbose = TRUE) { # nolint

    assert_class(seg, "data.frame")
    seg_file <- tempfile("run_gistic2")
    data.table::fwrite(seg, file = seg_file, sep = "\t")
    on.exit(file.remove(seg_file))
    sep <- " "
    create_dir(outdir)
    args <- c(
        handle_sys_arg("-seg", seg_file, sep = sep),
        handle_sys_arg("-refgene", refgene, sep = sep),
        handle_sys_arg("-b", outdir, sep = sep),
        handle_sys_arg("-ta", t_amp, sep = sep),
        handle_sys_arg("-td", t_del, sep = sep),
        handle_sys_arg("-qvt", qv_thresh, sep = sep),
        handle_sys_arg("-rx", remove_XY, format = "%d", sep = sep),
        handle_sys_arg("-broad", run_broad_analysis, format = "%d", sep = sep),
        handle_sys_arg("-brlen", broad_len_cutoff, sep = sep),
        handle_sys_arg("-maxseg", maxseg, format = "%d", sep = sep),
        handle_sys_arg("-conf", conf_level, sep = sep),
        handle_sys_arg("-genegistic", genegistic, format = "%d", sep = sep),
        handle_sys_arg("-savegene", savegene, format = "%d", sep = sep),
        handle_sys_arg("-arm_peeloff", arm_peeloff, format = "%d", sep = sep),
        ...,
        handle_sys_arg("-v", gistic2_verbose, sep = sep)
    )
    run_sys_command(
        cmd = gistic2_cmd,
        name = "gistic2", args = args,
        env = env, sys_args = sys_args,
        verbose = verbose
    )
}
