# Test data setup using trs_pseudo_rwl
setup_test_data <- function() {
  # Create realistic test data using trs_pseudo_rwl
  x_data <- trs_pseudo_rwl(
    n_series = 3, series_length = 50, end_date = 2020,
    common_signal = 0.4, seed = 123
  )

  y_data <- trs_pseudo_rwl(
    n_series = 2, series_length = 40, end_date = 2015,
    common_signal = 0.3, seed = 456
  )

  # Ensure some overlap between datasets
  common_years <- intersect(rownames(x_data), rownames(y_data))
  if (length(common_years) < 10) {
    # Adjust y_data to have more overlap
    y_data <- trs_pseudo_rwl(
      n_series = 2, series_length = 45, end_date = 2018,
      mean_width = 1.1, common_signal = 0.3, seed = 456
    )
  }

  list(x = x_data, y = y_data)
}

# Test 1: Basic functionality with pseudo-rwl data
test_that("trs_tho basic functionality works with realistic data", {
  test_data <- setup_test_data()

  result <- trs_tho(test_data$x, test_data$y, min_overlap = 10)

  expect_type(result, "list")
  expect_named(result, c("t_Ho", "overlap"))
  expect_equal(dim(result$t_Ho), c(3, 2)) # 3 test series vs 2 reference series
  expect_equal(dim(result$overlap), c(3, 2))
  expect_equal(rownames(result$t_Ho), colnames(test_data$x))
  expect_equal(colnames(result$t_Ho), colnames(test_data$y))

  # Check that we have some valid t-values
  expect_true(any(!is.na(result$t_Ho)))
  expect_true(all(result$overlap >= 0, na.rm = TRUE))
})

# Test 2: Data frame output format with realistic data
test_that("trs_tho data frame output works with realistic data", {
  test_data <- setup_test_data()

  result <- trs_tho(test_data$x, test_data$y, min_overlap = 10, as_df = TRUE)

  expect_s3_class(result, "data.frame")
  expect_named(result, c("series", "reference", "t_Ho", "overlap"))
  expect_equal(nrow(result), 6) # 3x2 combinations

  expect_type(result$t_Ho, "double")
  expect_type(result$overlap, "double")

  # Check that series names match original data
  expect_true(all(result$series %in% colnames(test_data$x)))
  expect_true(all(result$reference %in% colnames(test_data$y)))
})

# Test 3: Self-comparison with realistic data
test_that("trs_tho self-comparison works with realistic data", {
  test_data <- setup_test_data()

  result <- trs_tho(test_data$x, y = NULL, min_overlap = 10)

  expect_equal(dim(result$t_Ho), c(3, 3))
  expect_equal(rownames(result$t_Ho), colnames(result$t_Ho))

  # Diagonal should be infinite (perfect correlation with self)
  diag_values <- diag(result$t_Ho)
  expect_true(all(is.infinite(diag_values[!is.na(diag_values)])))

  # Off-diagonal values should be finite t-values or NA
  off_diag <- result$t_Ho[row(result$t_Ho) != col(result$t_Ho)]
  finite_values <- off_diag[!is.na(off_diag)]
  expect_true(all(is.finite(finite_values)))
})

# Test 4: Parameter validation
test_that("trs_tho parameter validation works", {
  test_data <- setup_test_data()

  # Non-data.frame input
  expect_error(trs_tho("not_a_dataframe"))
  expect_error(trs_tho(test_data$x, "not_a_dataframe"))

  # Invalid min_overlap
  expect_error(trs_tho(test_data$x, min_overlap = 2))
  expect_error(trs_tho(test_data$x, min_overlap = 5.5))
  expect_error(trs_tho(test_data$x, min_overlap = -1))
})

# Test 5: Edge cases with realistic data
test_that("trs_tho handles edge cases with realistic data", {
  # Single series
  single_series <- trs_pseudo_rwl(n_series = 1, series_length = 30, end_date = 2020, seed = 789)

  result <- trs_tho(single_series, min_overlap = 10)
  expect_equal(dim(result$t_Ho), c(1, 1))
  expect_true(is.infinite(result$t_Ho[1, 1]) || is.na(result$t_Ho[1, 1]))

  # No overlapping years
  x_early <- trs_pseudo_rwl(n_series = 2, series_length = 20, end_date = 1990, seed = 111)
  y_late <- trs_pseudo_rwl(n_series = 2, series_length = 20, end_date = 2020, seed = 222)

  expect_error(trs_tho(x_early, y_late))

  # Insufficient overlap due to high min_overlap threshold
  test_data <- setup_test_data()
  result <- trs_tho(test_data$x, test_data$y, min_overlap = 100) # Very high threshold
  expect_true(all(is.na(result$t_Ho)) || sum(!is.na(result$t_Ho)) == 0)
})

# Test 6: Transform parameter with realistic data
test_that("trs_tho transform parameter works with realistic data", {
  test_data <- setup_test_data()

  # With transformation (default)
  result1 <- trs_tho(test_data$x, test_data$y, min_overlap = 10, transform = TRUE)

  # Without transformation (assuming pre-transformed data)
  result2 <- trs_tho(test_data$x, test_data$y, min_overlap = 10, transform = FALSE)

  # Results should be different (unless there are no valid comparisons)
  if (any(!is.na(result1$t_Ho)) && any(!is.na(result2$t_Ho))) {
    expect_false(identical(result1$t_Ho, result2$t_Ho))
  }
})

# Test 7: Missing values handling with realistic data
test_that("trs_tho handles missing values in realistic data", {
  # Create data with intentional missing values
  test_data_na <- trs_pseudo_rwl(n_series = 3, series_length = 40, end_date = 2020, seed = 333)

  # Introduce additional missing values
  test_data_na[sample(nrow(test_data_na), 5), 1] <- NA
  test_data_na[sample(nrow(test_data_na), 3), 3] <- NA

  ref_data_na <- trs_pseudo_rwl(n_series = 2, series_length = 35, end_date = 2018, seed = 444)
  ref_data_na[sample(nrow(ref_data_na), 4), 2] <- NA

  result <- trs_tho(test_data_na, ref_data_na, min_overlap = 5)

  expect_type(result, "list")
  expect_true(all(!is.na(result$overlap))) # Overlap counts should be valid
  # Some t-values might be NA due to insufficient overlap, but structure should be intact
  expect_equal(dim(result$t_Ho), c(3, 2))
})

# Test 8: Rowname consistency
test_that("trs_tho requires proper rownames", {
  test_data <- setup_test_data()

  # Remove rownames
  x_no_names <- test_data$x
  rownames(x_no_names) <- NULL

  y_no_names <- test_data$y
  rownames(y_no_names) <- NULL

  # Should still work with default rownames
  expect_error(trs_tho(x_no_names, y_no_names), NA)
})

# Test 9: Correlation edge cases with realistic data
test_that("trs_tho handles correlation edge cases with realistic data", {
  # Create data with high correlation by using similar parameters
  perfect_data1 <- trs_pseudo_rwl(
    n_series = 2, series_length = 30, end_date = 2020,
    common_signal = 0.9, seed = 555
  )

  # Create nearly identical second series
  perfect_data2 <- perfect_data1
  colnames(perfect_data2) <- c("ref1", "ref2")
  # Add tiny random noise to avoid perfect correlation
  perfect_data2[, 1] <- perfect_data2[, 1] * (1 + rnorm(nrow(perfect_data2), 0, 0.001))

  result <- trs_tho(perfect_data1, perfect_data2, min_overlap = 10)

  # Should have high t-values due to strong correlation
  finite_t_values <- result$t_Ho[is.finite(result$t_Ho)]
  if (length(finite_t_values) > 0) {
    expect_true(any(abs(finite_t_values) > 5)) # High t-values expected
  }
})
