#' Python is a programming language that lets you work quickly and integrate
#' systems more effectively.
#'
#' @param ... `r rd_dots("python")`. Details see: `python(help = TRUE)`
#' @param pythonpath A character define the environment variables `PYTHONPATH`
#'  to be added before running command.
#' @inheritParams allele_counter
#' @param python `r rd_cmd("python")`.
#' @seealso <https://www.python.org/>
#' @export
python <- exec_build(
    command_new_name("python", class = "python"),
    ... = , pythonpath = NULL,
    opath_internal = NULL, help = "--help",
    setup_envvar = exprs(
        envvar <- envvar_parse_path(envvar, name = "PYTHONPATH", pythonpath)
    )
)

#' @export
command_locate_name.python <- function(name) {
    if (name == "python") {
        if (nzchar(Sys.which("python2")) && !nzchar(Sys.which("python3"))) {
            name <- "python2"
        } else {
            name <- "python3"
        }
    } else if (!any(name == c("python2", "python3"))) {
        cli::cli_abort(c_msg(
            "{.arg name} must be",
            oxford_comma(c("python", "python2", "python3"), final = "or")
        ))
    }
    Sys.which(name)
}
