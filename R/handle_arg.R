#' Deliver arguments of command
#' @param tag A string specifying argument tag, like "-i", "-o".
#' @param value Value passed to the argument.
#' @param lgl2int A logical value indicates whether transfrom value TRUE to 1 or
#'  FALSE to 0. If `TRUE`, format will always be set to "%d".
#' @param format The format of the value, details see [sprintf].
#' @param sep A character string used to separate "tag" and "value", usually " "
#'  or "=".
#' @return A string or NULL which can be used directly by [run_command].
#' @export
handle_arg <- function(tag, value = TRUE, lgl2int = FALSE, format = "%s", sep = " ") {
    assert_class(tag, is_scalar_character, "scalar {.cls character}")
    assert_class(lgl2int, is_scalar_logical, "scalar {.cls logical}")
    assert_class(format, is_scalar_character, "scalar {.cls character}")
    assert_class(sep, is_scalar_character, "scalar {.cls character}")
    handle_sys_arg(tag = tag, value = value, format = format, sep = sep)
}

handle_sys_arg <- function(
    tag, value, lgl2int = FALSE,
    format = NULL, sep = " ", call = parent.frame()) {
    if (lgl2int) {
        assert_class(
            value,
            is_scalar_logical,
            "scalar {.cls logical}",
            call = call
        )
        format <- "%d"
        value <- as.integer(value)
    } else {
        assert_length(value, 1L, null_ok = TRUE, call = call)
        format <- format %||% "%s"
    }
    if (is.null(value) || isFALSE(value)) {
        return(NULL)
    } else if (isTRUE(value)) {
        return(tag)
    } else {
        return(sprintf(paste(tag, format, sep = sep), value))
    }
}
