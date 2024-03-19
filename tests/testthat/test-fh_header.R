testthat::test_that("Output is data.frame", {
        x <- system.file("extdata", "DOEL1.fh", package = "fellingdateR")
        x <- read_fh(x, verbose = FALSE, header = FALSE)
        x <- fh_header(x)
        testthat::expect_s3_class(x, "data.frame")
})

testthat::test_that("Output is data.frame with 29 columns", {
        x <- system.file("extdata", "DOEL1.fh", package = "fellingdateR")
        x <- read_fh(x, verbose = FALSE, header = FALSE)
        x <- fh_header(x)
        testthat::expect_equal(ncol(x), 29)
})
