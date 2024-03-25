#' Python is a programming language that lets you work quickly and integrate
#' systems more effectively.
#'
#' @param ... `r rd_dots("python")`. Details see: `python(help = TRUE)`
#' @param python `r rd_cmd("python")`.
#' @seealso <https://www.python.org/>
#' @inherit exec return
#' @export
python <- function(..., python = NULL) {
    Execute$new(SysPython$new(cmd = python, ...))
}

SysPython <- R6::R6Class(
    "SysPython",
    inherit = Command,
    private = list(
        name = "python", alias = c("python3", "python2"), help = "--help"
    )
)
