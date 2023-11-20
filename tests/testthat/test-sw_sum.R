testthat::test_that("Output is a data.frame", {
     x <- sw_sum(fellingdateR:::dummy7, plot = FALSE)
     testthat::expect_s3_class(x, "data.frame")
     testthat::expect_true('SPD' %in% names(x))
})

testthat::test_that("Output is a data.frame", {
     x <- sw_sum(fellingdateR:::dummy6, plot = FALSE)
     testthat::expect_s3_class(x, "data.frame")
     testthat::expect_true('SPD' %in% names(x))
})

testthat::test_that("sw_sum does not work with invalid density function", {
     testthat::expect_error(
          sw_sum(
               fellingdateR:::dummy7,
               densfun = "nuka-cola"
          ),
          regexp = "not a supported distribution"
     )
})

testthat::test_that("sw_sum does not work with invalid credMass", {
     testthat::expect_error(
          sw_sum(
               fellingdateR:::dummy7,
               credMass = "ninety percent"
          )
     )
})
