testthat::test_that("Output is a data.frame", {
        x <- sw_combine(trs_example2, plot = FALSE)
        testthat::expect_s3_class(x$raw_data, "data.frame")
        testthat::expect_s3_class(x$individual_series, "data.frame")
        testthat::expect_type(x$model_summary, "character")
})

testthat::test_that("Output is a data.frame", {
        x <- sw_combine(trs_example3, plot = FALSE)
        testthat::expect_s3_class(x$raw_data, "data.frame")
        testthat::expect_s3_class(x$individual_series, "data.frame")
        testthat::expect_type(x$model_summary, "character")
})
