rd_format_code <- function(x) sprintf("`%s`", x)

rd_cmd <- function(cmd) {
    sprintf("A string of path to `%s` command", cmd)
}

rd_dots <- function(cmd) {
    sprintf(c_msg(
        "<[dynamic dots][rlang::dyn-dots]>",
        "Additional arguments passed to `%s` command,",
        "empty arguments are automatically trimed"
    ), cmd)
}
