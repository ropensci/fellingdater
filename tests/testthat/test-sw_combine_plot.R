# Test if the function returns a ggplot object
testthat::test_that("sw_combine_plot() returns a ggplot object", {
     int <- sw_combine(sw_example2, plot = FALSE)
     testthat::expect_true("ggproto" %in% class(sw_combine_plot(int)$layers[[1]]$stat))
})
