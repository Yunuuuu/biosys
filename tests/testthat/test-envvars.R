testthat::test_that("with_envvar() works well", {
    testthat::expect_identical(
        with_envvar(c("TEMP_SECRET" = "secret"), Sys.getenv("TEMP_SECRET")),
        "secret"
    )
    testthat::expect_identical(
        with_envvar(c(PATH = NA), Sys.getenv("PATH", unset = "")),
        ""
    )
})

testthat::test_that("parse_envvar() works well", {
    # one item ---------------------
    testthat::expect_identical(
        system2("echo", "$TEST",
            env = parse_envvar(c(TEST = shQuote("I'm from parse_envvar()"))),
            stdout = TRUE
        ),
        "I'm from parse_envvar()"
    )
    # multiple items ---------------------
    testthat::expect_identical(
        system2("echo", "$FIRST",
            env = parse_envvar(
                c(
                    FIRST = shQuote("I'm the first"),
                    SECOND = shQuote("I'm the second")
                )
            ),
            stdout = TRUE
        ),
        "I'm the first"
    )
    testthat::expect_identical(
        system2("echo", "$SECOND",
            env = parse_envvar(
                c(
                    FIRST = shQuote("I'm the first"),
                    SECOND = shQuote("I'm the second")
                )
            ),
            stdout = TRUE
        ),
        "I'm the second"
    )
    testthat::expect_identical(
        system2("echo", c("$FIRST", "$SECOND"),
            env = parse_envvar(
                c(
                    FIRST = shQuote("I'm the first"),
                    SECOND = shQuote("I'm the second")
                )
            ),
            stdout = TRUE
        ),
        "I'm the first I'm the second"
    )
})
