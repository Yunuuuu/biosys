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

# mimic rlang::exprs, but will omit `{`
exprs <- function(...) {
    dots <- rlang::exprs(...)
    if (...length() == 1L && identical(dots[[1L]][[1L]], as.symbol("{"))) {
        dots <- as.list(dots[[1L]][-1L])
    }
    dots
}

fclass <- function(x) class(x)[1L]

on_exit <- function(expr, add = FALSE, after = TRUE, envir = parent.frame()) {
    expr <- rlang::enquo(expr)
    expr <- rlang::expr(on.exit(expr = !!expr, add = !!add, after = !!after))
    rlang::eval_tidy(expr, env = envir)
}
