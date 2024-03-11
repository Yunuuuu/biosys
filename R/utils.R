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

# mimic rlang::exprs, but will omit `{`
exprs <- function(...) {
    dots <- rlang::exprs(...)
    if (...length() == 1L && identical(dots[[1L]][[1L]], as.symbol("{"))) {
        dots <- as.list(dots[[1L]][-1L])
    }
    dots
}
