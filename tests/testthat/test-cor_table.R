testthat::test_that("Output is data.frame", {

     x <- system.file("extdata", "DOEL1.fh", package = "fellingdateR")
     x <- read_fh(x, verbose = FALSE)
     # crossdating ring-width series from Doel 1 against each other:
     x <- cor_table(x, output = "table", min_overlap = 80)
     testthat::expect_s3_class(x, "data.frame")
})

testthat::test_that("Output is data.frame with 14 columns", {

     x <- system.file("extdata", "DOEL1.fh", package = "fellingdateR")
     x <- read_fh(x, verbose = FALSE)
     # crossdating ring-width series from Doel 1 against each other:
     x <- cor_table(x, output = "table", min_overlap = 50)
     testthat::expect_equal(ncol(x), 14)
})


testthat::test_that("Output is list of matrices", {

     x <- system.file("extdata", "DOEL1.fh", package = "fellingdateR")
     x <- read_fh(x, verbose = FALSE)
     # crossdating ring-width series from Doel 1 against each other:
     x <- cor_table(x, output = "matrix", min_overlap = 80)
     testthat::expect_type(x, "list")
     testthat::expect_equal(length(x), 7)
})
