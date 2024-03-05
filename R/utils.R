`%||%` <- function(x, y) {
    if (is.null(x)) y else x
}

is_scalar <- function(x) {
    length(x) == 1L
}

is_scalar_numeric <- function(x) {
    is_scalar(x) && is.numeric(x)
}

dir_create <- function(dir, ...) {
    if (!dir.exists(dir)) {
        cli::cli_alert_info("Create {.path {dir}}")
        dir.create(path = dir, ...)
    }
}
