#' Deliver arguments of command
#' @param tag A string specifying argument tag, like "-i", "-o".
#' @param value Value passed to the argument.
#' @param indicator A logical value specifying whether value should be a
#'   ndicator of tag. If TRUE, logical value will explain the set (TRUE or NULL)
#'  or unset (FALSE) of tag. If `NULL`, `indicator` will be `TRUE` for logical
#'  `value` argument, otherwise, will be `FALSE`.
#' @param lgl2int A logical value indicates whether transfrom value TRUE to 1 or
#'  FALSE to 0. If `TRUE`, format will always be set to "%d".
#' @param format The format of the value, details see [sprintf].
#' @param sep A character string used to separate "tag" and "value", usually " "
#'  or "=".
#' @return A string or NULL which can be used directly by [run_command].
#' @export
handle_arg <- function(tag, value = TRUE, indicator = NULL, lgl2int = FALSE, format = "%s", sep = " ") {
    assert_string(tag, empty_ok = FALSE)
    assert_bool(indicator, null_ok = TRUE)
    indicator <- indicator %||% rlang::is_scalar_logical(value)
    handle_sys_arg(
        tag = tag, value = value,
        indicator = indicator, lgl2int = lgl2int,
        format = format, sep = sep
    )
}

#' @keywords internal
#' @noRd
handle_sys_arg <- function(tag, value, indicator = FALSE, lgl2int = FALSE, format = "%s", sep = " ", call = rlang::caller_env()) {
    if (is.null(value)) return(NULL) # styler: off
    assert_bool(lgl2int, call = call)
    if (indicator) {
        assert_bool(value, call = call)
        if (value) return(tag) else return(NULL) # styler: off
    }
    assert_string(sep, call = call)
    if (lgl2int) {
        assert_bool(value, call = call)
        format <- "%d"
        value <- as.integer(value)
    } else {
        assert_string(format, empty_ok = FALSE, call = call)
        if (format == "%d") {
            assert_(value, rlang::is_scalar_numeric,
                "scalar {.cls numeric}",
                call = call
            )
        } else {
            assert_(value, is_scalar, "scalar", call = call)
        }
    }
    sprintf(paste(tag, format, sep = sep), value)
}
