# Test data setup helper
create_test_data <- function() {
     # Create overlapping series for testing
     x <- data.frame(trs_001 = c(1.2, 1.5, 0.8, 1.1, 0.9, 1.3, 1.0, 1.4, 1.1, 0.7, 1.2, 1.0))
     rownames(x) <- 1990:2001

     y <- data.frame(ref_001 = c(0.9, 1.3, 0.7, 1.0, 1.2, 1.1, 1.5, 0.8, 1.0, 1.3, 0.9, 1.1, 1.4, 0.8))
     rownames(y) <- 1988:2001

     list(x = x, y = y)
}

# Test basic functionality
test_that("trs_plot_dated creates ggplot object", {
     skip_if_not_installed("ggplot2", minimum_version = "3.5.0")

     data <- create_test_data()

     # Should create a ggplot without errors
     expect_no_error(p <- trs_plot_dated(data$x, data$y))
     expect_s3_class(p, "ggplot")
     expect_s3_class(p, "gg")
})

# Test input validation
test_that("input validation catches errors", {
     skip_if_not_installed("ggplot2", minimum_version = "3.5.0")

     data <- create_test_data()

     # Test with insufficient overlap
     x_short <- data.frame(trs = c(1, 2, 3))
     rownames(x_short) <- 2010:2012

     expect_error(
          trs_plot_dated(x_short, data$y),
          "overlap less than 10 years"
     )

     # Test with non-numeric rownames (if function doesn't handle)
     x_bad <- data$x
     rownames(x_bad) <- paste0("year_", 1990:2001)

     # This should error in as.numeric conversion
     expect_error(suppressWarnings(trs_plot_dated(x_bad, data$y)))
})


# Test parameter effects
test_that("zscore parameter works correctly", {
     skip_if_not_installed("ggplot2", minimum_version = "3.5.0")

     data <- create_test_data()

     p1 <- trs_plot_dated(data$x, data$y, zscore = TRUE)
     p2 <- trs_plot_dated(data$x, data$y, zscore = FALSE)

     # Y-axis labels should be different
     expect_true(p1$labels$y != p2$labels$y)
     expect_equal(p1$labels$y, "z-score\n")
     expect_equal(p2$labels$y, "ring width\n")
})

test_that("pv_highlight parameter works correctly", {
     skip_if_not_installed("ggplot2", minimum_version = "3.5.0")

     data <- create_test_data()

     p1 <- trs_plot_dated(data$x, data$y, pv_highlight = TRUE)
     p2 <- trs_plot_dated(data$x, data$y, pv_highlight = FALSE)

     # Check number of layers differs
     expect_true(length(p1$layers) != length(p2$layers))

     # With highlighting should have geom_rect layer
     layer_types1 <- sapply(p1$layers, function(x) class(x$geom)[1])
     layer_types2 <- sapply(p2$layers, function(x) class(x$geom)[1])

     expect_true("GeomRect" %in% layer_types1)
     expect_false("GeomRect" %in% layer_types2)
})

test_that("end_year parameter works correctly", {
     skip_if_not_installed("ggplot2", minimum_version = "3.5.0")

     data <- create_test_data()

     # Test with different end year
     p1 <- trs_plot_dated(data$x, data$y, end_year = 2000)

     # Should complete without error
     expect_s3_class(p1, "ggplot")

     # Test with NULL end_year (default)
     p2 <- trs_plot_dated(data$x, data$y, end_year = NULL)
     expect_s3_class(p2, "ggplot")
})


# Test data requirements
test_that("data requirements are enforced", {
     skip_if_not_installed("ggplot2", minimum_version = "3.5.0")

     # Empty data frames
     expect_error(trs_plot_dated(data.frame(), data.frame()))

     # Data frames with no numeric rownames
     x_bad <- data.frame(series = c(1, 2, 3))
     rownames(x_bad) <- c("a", "b", "c")

     expect_error(trs_plot_dated(x_bad))
})

# Test with different data types/structures
test_that("handles different data structures", {
     skip_if_not_installed("ggplot2", minimum_version = "3.5.0")

     # Test with matrices converted to data.frames
     x_matrix <- matrix(runif(12), ncol = 1)
     rownames(x_matrix) <- 1990:2001
     x_df <- as.data.frame(x_matrix)

     y_matrix <- matrix(runif(12), ncol = 1)
     rownames(y_matrix) <- 1990:2001
     y_df <- as.data.frame(y_matrix)

     expect_no_error(trs_plot_dated(x_df, y_df))
})
