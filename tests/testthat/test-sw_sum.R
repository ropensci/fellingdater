testthat::test_that("Output is a data.frame", {
        x <- sw_sum(trs_example7, plot = FALSE)
        testthat::expect_s3_class(x, "data.frame")
        testthat::expect_true('SPD' %in% names(x))
})

testthat::test_that("Output is a data.frame", {
        x <- sw_sum(trs_example6, plot = FALSE)
        testthat::expect_s3_class(x, "data.frame")
        testthat::expect_true('SPD' %in% names(x))
})

testthat::test_that("sw_sum does not work with invalid density function", {
        testthat::expect_error(sw_sum(trs_example7,
                                      densfun = "nuka-cola"),
                               regexp = "not a supported distribution")
})

testthat::test_that("sw_sum does not work with invalid credMass", {
        testthat::expect_error(sw_sum(trs_example7,
                                      credMass = "ninety percent"))
})


testthat::test_that("sw_sum warning series without waney edge removed", {
        testthat::expect_warning(sw_sum(trs_example4))
})

testthat::test_that("sw_sum warning no series wtih sapwood or waney edge removed", {
        testthat::expect_error(suppressWarnings(sw_sum(trs_example5)))
})
