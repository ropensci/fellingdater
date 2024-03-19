# Test if the function returns a ggplot object
testthat::test_that("sw_interval_plot() returns a ggplot object", {
     int <- sw_interval(
          n_sapwood = 10,
          last = 1000,
          hdi = FALSE,
          cred_mass = .95,
          sw_data = "Hollstein_1980",
          densfun = "lognormal",
          plot = FALSE
     )
     testthat::expect_true("ggproto" %in% class(sw_interval_plot(int)$layers[[1]]$stat))
})
