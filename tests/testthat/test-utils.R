testthat::test_that("`utils-path` works well", {
    # remove extension
    testthat::expect_identical(path_ext_remove("a.b"), "a")
    testthat::expect_identical(path_ext_remove("a.b.c"), "a.b")
    testthat::expect_identical(path_ext_remove("a."), "a")

    # set extension
    testthat::expect_identical(path_ext_set("a", "b"), "a.b")
    testthat::expect_identical(path_ext_set("a.b", "c"), "a.c")
    testthat::expect_identical(path_ext_set("a.", "b"), "a.b")

    # extract extension
    testthat::expect_identical(path_ext("a.b"), "b")
    testthat::expect_identical(path_ext("a.b.c"), "c")
    testthat::expect_identical(path_ext("a."), "")
})
