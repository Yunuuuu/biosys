testthat::test_that("with_envvar works well", {
    testthat::expect_identical(
        with_envvar(c("TEMP_SECRET" = "secret"), Sys.getenv("TEMP_SECRET")),
        "secret"
    )
    testthat::expect_identical(
        with_envvar(c(PATH = NA), Sys.getenv("PATH", unset = "")),
        ""
    )
})
