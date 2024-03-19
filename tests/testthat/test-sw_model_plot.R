# Test if the function returns a ggplot object
testthat::test_that("sw_model_plot() returns a ggplot object", {
  p <- sw_model(
    sw_data = "Hollstein_1980",
    cred_mass = .95,
    densfun = "lognormal",
    plot = FALSE
  )
  testthat::expect_true(
    "ggproto" %in% class(sw_model_plot(p,
      bar_fill = "steelblue3",
      bar_color = "grey60",
      line_color = "red3"
    )$layers[[1]]$stat)
  )
})
