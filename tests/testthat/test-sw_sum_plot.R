# Test if the function returns a ggplot object
testthat::test_that("sw_sum_plot() returns a ggplot object", {
     x <- sw_sum(sw_example6, plot = FALSE)
     x <- sw_sum_plot(x)
     testthat::expect_true("ggproto" %in% class(x$layers[[1]]$stat))
})


