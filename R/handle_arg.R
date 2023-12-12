#' Deliver arguments of command
#' @param tag A string specifying argument tag, like "-i", "-o".
#' @param value Value passed to the argument.
#' @param indicator A logical value specifying whether value should be a
#'   ndicator of tag. If TRUE, logical value will explain the set (TRUE or NULL)
#'  or unset (FALSE) of tag.
#' @param lgl2int A logical value indicates whether transfrom value TRUE to 1 or
#'  FALSE to 0. If `TRUE`, format will always be set to "%d".
#' @param format The format of the value, details see [sprintf].
#' @param sep A character string used to separate "tag" and "value", usually " "
#'  or "=".
#' @return A string or NULL which can be used directly by [run_command].
#' @export
#' @importFrom rlang is_scalar_logical
handle_arg <- function(tag, value = TRUE, indicator = is_scalar_logical(value), lgl2int = FALSE, format = "%s", sep = " ") {
    assert_string(tag, empty_ok = FALSE)
    handle_sys_arg(
        tag = tag, value = value,
        indicator = indicator, lgl2int = lgl2int,
        format = format, sep = sep
    )
}

#' @keywords internal
#' @noRd 
handle_sys_arg <- function(
    tag, value, indicator = FALSE, lgl2int = FALSE,
    format = NULL, sep = " ", call = parent.frame()) {
    if (is.null(value)) {
        return(NULL)
    }
    if (indicator || lgl2int) {
        assert_bool(value, call = call)
    }
    if (indicator) {
        if (value) {
            return(tag)
        } else { # NA will also return NULL
            return(NULL)
        }
    } else {
        assert_bool(lgl2int, call = call)
        assert_string(sep, call = call)
        if (lgl2int) {
            format <- "%d"
            value <- as.integer(value)
        } else {
            assert_string(format, null_ok = TRUE)
            if (!is.null(format) && format == "%d") {
                assert_(value, is_scalar_numeric, "scalar {.cls numeric}",
                    call = call
                )
            } else {
                assert_(value, is_scalar, "scalar", call = call)
                format <- format %||% "%s"
            }
        }
        return(sprintf(paste(tag, format, sep = sep), value))
    }
}
