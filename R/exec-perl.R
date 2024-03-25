#' Perl is a highly capable, feature-rich programming language with over 36
#' years of development.
#'
#' @param ... `r rd_dots("perl")`. Details see: `perl(help = TRUE)`
#' @param perl `r rd_cmd("perl")`.
#' @seealso <https://www.perl.org/>
#' @export
perl <- function(..., perl = NULL) {
    Execute$new(SysPerl$new(cmd = perl, ...))
}

SysPerl <- R6::R6Class(
    "SysPerl",
    inherit = Command,
    private = list(name = "perl", help = "--help")
)
