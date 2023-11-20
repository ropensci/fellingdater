testthat::test_that("Output is a data.frame", {
     x <- sw_combine(fellingdateR:::dummy2, plot = FALSE)
     testthat::expect_s3_class(x$rawData, "data.frame")
     testthat::expect_s3_class(x$individual_series, "data.frame")
     testthat::expect_type(x$model_summary, "character")
})

testthat::test_that("Output is a data.frame", {
     x <- sw_combine(fellingdateR:::dummy3, plot = FALSE)
     testthat::expect_s3_class(x$rawData, "data.frame")
     testthat::expect_s3_class(x$individual_series, "data.frame")
     testthat::expect_type(x$model_summary, "character")
})
