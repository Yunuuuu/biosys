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

testthat::test_that("`compress` works as expected", {
    # prepare file
    tmpdir <- tempdir()
    file <- tempfile(tmpdir = tmpdir, fileext = "gz")
    write_lines(letters, path = file)

    # basic compress works
    compress("gzip", file, odir = tmpdir)
    gzip_file <- file_path(tmpdir, basename(file), ext = "gz")
    testthat::expect_true(file.exists(gzip_file))
    file.remove(gzip_file)

    compress("bzip2", file, odir = tmpdir)
    bzip2_file <- file_path(tmpdir, basename(file), ext = "bz2")
    testthat::expect_true(file.exists(bzip2_file))
    file.remove(bzip2_file)

    compress("xz", file, odir = tmpdir)
    xz_file <- file_path(tmpdir, basename(file), ext = "xz")
    testthat::expect_true(file.exists(xz_file))
    file.remove(xz_file)

    # override argument works
    compress("gzip", file, odir = tmpdir)
    testthat::expect_error(compress("gzip", file, odir = tmpdir))
    testthat::expect_no_error(
        compress("gzip", file, odir = tmpdir, override = TRUE)
    )
    file.remove(gzip_file)

    # keep argument works
    testthat::expect_true(file.exists(file))
    compress("gzip", file, odir = tmpdir, keep = FALSE, override = TRUE)
    testthat::expect_false(file.exists(file))
    file.remove(gzip_file)
})

testthat::test_that("`decompress` works as expected", {
    # prepare file
    tmpdir <- tempdir()
    file <- tempfile(tmpdir = tmpdir)
    write_lines(letters, path = file)

    # gzip decompress works
    gzip_file <- compress("gzip", file, odir = tmpdir)
    testthat::expect_no_error(
        decompress("gzip", gzip_file,
            ofile = "gzip_decompressed", odir = tmpdir
        )
    )
    file.remove(file.path(tmpdir, "gzip_decompressed"))
    gzip_file_no_ext <- file.path(dirname(gzip_file), "gzip_no_ext")
    file.rename(gzip_file, gzip_file_no_ext)
    testthat::expect_no_error(
        decompress("gzip", gzip_file_no_ext, odir = tmpdir)
    )
    file.remove(file.path(tmpdir, paste0(basename(gzip_file_no_ext), ".out")))


    # bzip2 decompress works
    bzip2_file <- compress("bzip2", file, odir = tmpdir)
    testthat::expect_no_error(
        decompress("bzip2", bzip2_file,
            ofile = "bzip2_decompressed", odir = tmpdir
        )
    )
    file.remove(file.path(tmpdir, "bzip2_decompressed"))
    bzip2_file_no_ext <- file.path(dirname(bzip2_file), "bzip2_no_ext")
    file.rename(bzip2_file, bzip2_file_no_ext)
    testthat::expect_no_error(
        decompress("bzip2", bzip2_file_no_ext, odir = tmpdir)
    )
    file.remove(file.path(tmpdir, paste0(basename(bzip2_file_no_ext), ".out")))


    # xz decompress works
    xz_file <- compress("xz", file, odir = tmpdir)
    testthat::expect_no_error(
        decompress("xz", xz_file,
            ofile = "xz_decompressed", odir = tmpdir
        )
    )
    file.remove(file.path(tmpdir, "xz_decompressed"))
    xz_file_no_ext <- file.path(dirname(xz_file), "xz_no_ext")
    file.rename(xz_file, xz_file_no_ext)
    testthat::expect_no_error(
        decompress("xz", xz_file_no_ext, odir = tmpdir)
    )
    file.remove(file.path(tmpdir, paste0(basename(xz_file_no_ext), ".out")))
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
