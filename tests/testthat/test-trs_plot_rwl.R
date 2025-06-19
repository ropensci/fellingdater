# Helper function to create test data
create_test_rwl <- function() {
  set.seed(123)
  years <- 1990:2020
  data.frame(
    series1 = rnorm(length(years), mean = 100, sd = 6),
    series2 = rnorm(length(years), mean = 95, sd = 5),
    series3 = rnorm(length(years), mean = 110, sd = 7),
    row.names = years
  )
}

# Test basic functionality
test_that("trs_plot_rwl creates ggplot object", {
  rwl_data <- create_test_rwl()

  # Test basic plot creation
  p <- trs_plot_rwl(rwl_data)
  expect_s3_class(p, "ggplot")

  # Test with zscore
  p_zscore <- trs_plot_rwl(rwl_data, zscore = TRUE)
  expect_s3_class(p_zscore, "ggplot")

  # Test without zscore
  p_raw <- trs_plot_rwl(rwl_data, zscore = FALSE)
  expect_s3_class(p_raw, "ggplot")
})

test_that("trs_plot_rwl handles faceting correctly", {
  rwl_data <- create_test_rwl()

  # Test faceted plot
  p_facet <- trs_plot_rwl(rwl_data, facet = TRUE)
  expect_s3_class(p_facet, "ggplot")

  # Test with custom ncol
  p_facet_ncol <- trs_plot_rwl(rwl_data, facet = TRUE, ncol = 2)
  expect_s3_class(p_facet_ncol, "ggplot")
})

test_that("trs_plot_rwl handles colors correctly", {
  rwl_data <- create_test_rwl()

  # Test single color
  p_single <- trs_plot_rwl(rwl_data, color = "red")
  expect_s3_class(p_single, "ggplot")

  # Test color vector
  colors <- c("red", "blue", "green")
  p_vector <- trs_plot_rwl(rwl_data, color = colors)
  expect_s3_class(p_vector, "ggplot")

  # Test wrong length color vector
  expect_error(
    trs_plot_rwl(rwl_data, color = c("red", "blue")),
    "Length of color vector"
  )
})

test_that("trs_plot_rwl validates input correctly", {
  # Test non-dataframe input
  expect_error(
    trs_plot_rwl(c(1, 2, 3)),
    "rwl must be a data.frame"
  )

  # Test empty dataframe
  empty_df <- data.frame()
  expect_error(
    trs_plot_rwl(empty_df),
    "rwl must have at least one column"
  )
})

test_that("trs_plot_rwl handles scale_y parameter", {
  rwl_data <- create_test_rwl()

  # Test different scale_y options
  scales <- c("fixed", "free", "free_x", "free_y")

  for (scale in scales) {
    p <- trs_plot_rwl(rwl_data, facet = TRUE, scale_y = scale)
    expect_s3_class(p, "ggplot")
  }
})

test_that("trs_plot_rwl handles NA values", {
  rwl_data <- create_test_rwl()

  # Introduce some NA values
  rwl_data$series1[1:3] <- NA
  rwl_data$series2[28:31] <- NA

  p <- trs_plot_rwl(rwl_data)
  expect_s3_class(p, "ggplot")

  # Check that plot data excludes NAs
  plot_data <- ggplot2::ggplot_build(p)$data[[1]]
  expect_false(any(is.na(plot_data$y)))
})

test_that("trs_plot_rwl legend positioning", {
  rwl_data <- create_test_rwl()

  # Non-faceted should have legend on right
  p_overlay <- trs_plot_rwl(rwl_data, facet = FALSE)
  legend_pos <- p_overlay$theme$legend.position
  expect_equal(legend_pos, "right")

  # Faceted should have no legend
  p_facet <- trs_plot_rwl(rwl_data, facet = TRUE)
  legend_pos_facet <- p_facet$theme$legend.position
  expect_equal(legend_pos_facet, "none")
})
