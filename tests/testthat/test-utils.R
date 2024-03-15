testthat::test_that("`utils-file` works well", {
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

testthat::test_that("`read_lines()` workds as expected", {
    # prepare file
    tmpdir <- tempdir()
    file <- tempfile(tmpdir = tmpdir)
    write_lines(letters, path = file)
    on.exit(file.remove(file))
    # gzip compressed file
    gzip_file <- compress("gzip", file, odir = tmpdir)
    testthat::expect_identical(read_lines(gzip_file), letters)
    file.remove(gzip_file)

    # bzip2 compressed file
    bzip2_file <- compress("bzip2", file, odir = tmpdir)
    testthat::expect_identical(read_lines(bzip2_file), letters)
    file.remove(bzip2_file)

    # xz compressed file
    xz_file <- compress("xz", file, odir = tmpdir)
    testthat::expect_identical(read_lines(xz_file), letters)
    file.remove(xz_file)
})
