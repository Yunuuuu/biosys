#' Deliver arguments of command
#' @param tag A string specifying argument tag, like "-i", "-o".
#' @param value Value passed to the argument.
#' @param format The format of the value, details see [sprintf].
#' @param sep A character string used to separate "tag" and "value", usually " "
#'  or "=".
#' @export
handle_arg <- function(tag, value, format = "%s", sep = " ") {
    assert_class(tag, is_scalar_character, "scalar character")
    assert_class(format, is_scalar_character, "scalar character")
    assert_class(sep, is_scalar_character, "scalar character")
    handle_sys_arg(tag = tag, value = value, format = format, sep = sep)
}

handle_sys_arg <- function(tag, value, format = "%s", sep = " ") {
    if (is.null(value) || isFALSE(value)) {
        return(NULL)
    } else if (isTRUE(value)) {
        return(tag)
    } else {
        return(sprintf(paste(tag, format, sep = sep), value))
    }
}
