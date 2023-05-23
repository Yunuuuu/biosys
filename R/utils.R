`%||%` <- function(x, y) {
    if (is.null(x)) y else x
}

#' Report if an argument is a specific class
#'
#' @keywords internal
#' @noRd
assert_class <- function(x, is_class, msg, null_ok = FALSE, arg = substitute(x), call = parent.frame()) {
    if (is_scalar_character(is_class)) {
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

#' mimic rlang::is_named
#' @noRd 
is_named <- function(x) {
    nms <- names(x)
    if (is.null(nms) || any(nms == "" | is.na(nms))) {
        FALSE
    } else {
        TRUE
    }
}

#' mimic rlang::is_named2
#' @noRd 
is_named2 <- function(x) {
    nms <- names(x)
    if (is.null(nms)) {
        return(!length(x))
    }
    if (any(nms == "" | is.na(nms))) {
        return(FALSE)
    }
    TRUE
}

#' mimic rlang::names2
#' @noRd 
names2 <- function(x) {
    nms <- names(x)
    if (is.null(nms)) {
        return(rep_len("", length(x)))
    }
    if (anyNA(nms)) {
        nms[is.na(nms)] <- ""
    }
    nms
}

#' This function returns a logical value that indicates if a data frame or
#' another named object contains an element with a specific name.  
#' NA can match NA in names(x)
#' @noRd 
has_name <- function(x, name) {
    name %in% names(x)
}

create_dir <- function(dir) {
    if (!dir.exists(dir)) {
        cli::cli_alert_info("Create {.path {dir}}")
        dir.create(dir)
    }
}
