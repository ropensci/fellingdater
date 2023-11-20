testthat::test_that("Output is a list", {
     x <- sw_data_info(sample(sw_data_overview(), 1))
     testthat::expect_type(x, "list")
     testthat::expect_true(all(c("data",
                             "citation",
                             "area",
                             "n_observations",
                             "summary_raw_data") %in% names(x)))
})


testthat::test_that("Does not work with random name", {
     testthat::expect_error(sw_data_info("Hollstein_2050"))
})

testthat::test_that("Does not work with no input variable", {
     testthat::expect_error(sw_data_info())
})
