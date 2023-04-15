testthat::test_that("names2() works well", {
    # null name with length
    x <- letters
    testthat::expect_equal(names2(x), rep_len("", length(x)))
    # null name without lengh
    x <- character()
    testthat::expect_equal(names2(x), rep_len("", length(x)))
    # names with NA
    x <- letters[1:5]
    names(x) <- c("a", "b", NA_character_, "c", NA_character_)
    testthat::expect_equal(names2(x), c("a", "b", "", "c", ""))
})

testthat::test_that("has_name() works well", {
    # null name with length
    testthat::expect_false(has_name(letters, "a"))
    testthat::expect_false(has_name(letters, NA_character_))
    # null name without lengh
    testthat::expect_false(has_name(character(), "a"))
    testthat::expect_false(has_name(logical(), "a"))
    testthat::expect_false(has_name(numeric(), "a"))
    testthat::expect_false(has_name(NULL, "a"))
    testthat::expect_false(has_name(character(), NA_character_))
    testthat::expect_false(has_name(logical(), NA_character_))
    testthat::expect_false(has_name(numeric(), NA_character_))
    testthat::expect_false(has_name(NULL, NA_character_))

    # names with NA
    x <- letters[1:5]
    names(x) <- c("a", "b", NA_character_, "c", NA_character_)
    testthat::expect_true(has_name(x, "a"))
    testthat::expect_true(has_name(x, NA_character_))
})
