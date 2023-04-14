`%||%` <- function(x, y) {
    if (is.null(x)) y else x
}

#' Report if an argument is a specific class
#'
#' @keywords internal
#' @noRd
assert_class <- function(x, is_class, msg, null_ok = FALSE, arg = substitute(x), call = parent.frame()) {
    if (is.character(is_class) && length(is_class) == 1L) {
        class <- is_class
        is_class <- function(x) {
            inherits(x, what = class)
        }
        if (missing(msg)) {
            msg <- "{.cls {class}} object"
        }
    }
    if (null_ok) {
        msg <- paste(msg, "or {.code NULL}", sep = " ")
    }
    msg <- sprintf("{.arg {arg}} must be a %s", msg)
    is_right_class <- is_class(x)
    # is_class sometimes return `TRUE` for`NULL`
    if (is.null(x) && !is_right_class) {
        if (!null_ok) {
            cli::cli_abort(c(msg,
                "x" = "You've supplied a {.code NULL}"
            ), call = call)
        }
    } else if (!is_right_class) {
        cli::cli_abort(c(msg,
            "x" = "You've supplied a {.cls {class(x)}} object"
        ), call = call)
    }
}

#' Report if an argument has specific length
#' @keywords internal
#' @noRd
assert_length <- function(x, length, null_ok = FALSE, arg = substitute(x), call = parent.frame()) {
    length <- as.integer(length)
    if (length == 1L) {
        msg <- "{.field scalar} object"
    } else {
        msg <- "length {.val {length}} object"
    }
    if (null_ok) {
        msg <- paste(msg, "or {.code NULL}", sep = " ")
    }
    msg <- sprintf("{.arg {arg}} must be a %s", msg)
    is_right_length <- length(x) == length
    if (is.null(x) && !is_right_length) {
        if (!null_ok) {
            cli::cli_abort(c(msg,
                "x" = "You've supplied a {.code NULL}"
            ), call = call)
        }
    } else if (!is_right_length) {
        cli::cli_abort(c(msg,
            "x" = "You've supplied a length {.val {length(x)}} object"
        ), call = call)
    }
}

assert_pkg <- function(pkg, fun = NULL, call = parent.frame()) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
        if (is.null(fun)) {
            fn_call <- eval(quote(match.call()), envir = parent.frame())
            fun <- as.character(fn_call[[1L]])
        }
        cli::cli_abort(
            "{.pkg {pkg}} must be installed to use {.fn {fun}}.",
            call = call
        )
    }
}

is_scalar <- function(x) {
    length(x) == 1L
}

is_scalar_numeric <- function(x) {
    is_scalar(x) && is.numeric(x)
}

is_scalar_character <- function(x) {
    is_scalar(x) && is.character(x)
}

is_scalar_logical <- function(x) {
    is_scalar(x) && is.logical(x)
}
