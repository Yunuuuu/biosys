`%||%` <- function(x, y) {
    if (is.null(x)) y else x
}

is_scalar <- function(x) {
    length(x) == 1L
}

is_scalar_numeric <- function(x) {
    is_scalar(x) && is.numeric(x)
}

is_number <- function(x) is_scalar_numeric(x) && !is.na(x)

pkg_nm <- function() {
    utils::packageName(topenv(environment()))
}

c_msg <- function(..., sep = " ") paste(..., sep = sep)
