testthat::test_that("Output is a data.frame", {
     x <- sw_sum(fellingdateR:::dummy7, plot = FALSE)
     testthat::expect_s3_class(x, "data.frame")
     testthat::expect_true('SPD' %in% names(x))
})
