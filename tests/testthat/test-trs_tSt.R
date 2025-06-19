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

  # Ensure some overlap between datasets and align to common years
  common_years <- intersect(rownames(x_data), rownames(y_data))
  if (length(common_years) < 10) {
    # Adjust y_data to have more overlap
    y_data <- trs_pseudo_rwl(
      n_series = 2, series_length = 45, end_date = 2018,
      mean_width = 1.1, common_signal = 0.3, seed = 456
    )
    common_years <- intersect(rownames(x_data), rownames(y_data))
  }

  # Align both datasets to common years to avoid length mismatch warnings
  x_aligned <- x_data[common_years, , drop = FALSE]
  y_aligned <- y_data[common_years, , drop = FALSE]

  list(x = x_aligned, y = y_aligned)
}

# Test 1: Basic functionality with pseudo-rwl data
test_that("trs_tSt basic functionality works with realistic data", {
  test_data <- setup_test_data()

  result <- trs_tSt(test_data$x, test_data$y, min_overlap = 10)

  expect_type(result, "list")
  expect_named(result, c("r_pearson", "t_St", "overlap"))
  expect_equal(dim(result$r_pearson), c(3, 2)) # 3 test series vs 2 reference series
  expect_equal(dim(result$t_St), c(3, 2))
  expect_equal(dim(result$overlap), c(3, 2))
  expect_equal(rownames(result$r_pearson), colnames(test_data$x))
  expect_equal(colnames(result$r_pearson), colnames(test_data$y))
  expect_equal(rownames(result$t_St), colnames(test_data$x))
  expect_equal(colnames(result$t_St), colnames(test_data$y))

  # Check that we have some valid values
  expect_true(any(!is.na(result$r_pearson)))
  expect_true(any(!is.na(result$t_St)))
  expect_true(all(result$overlap >= 0, na.rm = TRUE))
})

# Test 2: Data frame output format with realistic data
test_that("trs_tSt data frame output works with realistic data", {
  test_data <- setup_test_data()

  result <- trs_tSt(test_data$x, test_data$y, min_overlap = 10, as_df = TRUE)

  expect_s3_class(result, "data.frame")
  expect_named(result, c("series", "reference", "r_pearson", "t_St", "overlap"))
  expect_equal(nrow(result), 6) # 3x2 combinations

  expect_type(result$r_pearson, "double")
  expect_type(result$t_St, "double")
  expect_type(result$overlap, "integer")

  # Check that series names match original data
  expect_true(all(result$series %in% colnames(test_data$x)))
  expect_true(all(result$reference %in% colnames(test_data$y)))

  # Check correlation coefficients are in valid range
  valid_r <- result$r_pearson[!is.na(result$r_pearson)]
  if (length(valid_r) > 0) {
    expect_true(all(valid_r >= -1 & valid_r <= 1))
  }
})

# Test 3: Self-comparison with realistic data
test_that("trs_tSt self-comparison works with realistic data", {
  test_data <- setup_test_data()

  result <- trs_tSt(test_data$x, y = NULL, min_overlap = 10)

  expect_equal(dim(result$r_pearson), c(3, 3))
  expect_equal(dim(result$t_St), c(3, 3))
  expect_equal(rownames(result$r_pearson), colnames(result$r_pearson))
  expect_equal(rownames(result$t_St), colnames(result$t_St))

  # Diagonal should be 1 for correlations (perfect correlation with self)
  diag_r <- diag(result$r_pearson)
  valid_diag_r <- diag_r[!is.na(diag_r)]
  if (length(valid_diag_r) > 0) {
    expect_true(all(abs(valid_diag_r - 1) < 1e-10))
  }

  # Diagonal should be infinite for t-statistics (perfect correlation with self)
  diag_t <- diag(result$t_St)
  valid_diag_t <- diag_t[!is.na(diag_t)]
  if (length(valid_diag_t) > 0) {
    expect_true(all(is.infinite(valid_diag_t)))
  }

  # Off-diagonal values should be finite or NA
  off_diag_t <- result$t_St[row(result$t_St) != col(result$t_St)]
  finite_values <- off_diag_t[!is.na(off_diag_t)]
  expect_true(all(is.finite(finite_values)))
})

# Test 4: Parameter validation
test_that("trs_tSt parameter validation works", {
  test_data <- setup_test_data()

  # Non-data.frame input
  expect_error(trs_tSt("not_a_dataframe"))
  expect_error(trs_tSt(test_data$x, "not_a_dataframe"))
})

# Test 5: Edge cases with realistic data
test_that("trs_tSt handles edge cases with realistic data", {
  # Single series
  single_series <- trs_pseudo_rwl(n_series = 1, series_length = 30, end_date = 2020, seed = 789)

  result <- trs_tSt(single_series, min_overlap = 10)
  expect_equal(dim(result$r_pearson), c(1, 1))
  expect_equal(dim(result$t_St), c(1, 1))
  expect_equal(result$r_pearson[1, 1], 1, tolerance = 1e-10) # Perfect self-correlation
  expect_true(is.infinite(result$t_St[1, 1]) || is.na(result$t_St[1, 1]))

  # No overlapping years - create datasets with same years but different values
  x_early <- trs_pseudo_rwl(n_series = 2, series_length = 20, end_date = 1990, seed = 111)
  y_late <- trs_pseudo_rwl(n_series = 2, series_length = 20, end_date = 1990, seed = 222) # Same years, different data

  # This should work and compute correlations since years align
  result_different_data <- trs_tSt(x_early, y_late, min_overlap = 5)
  expect_type(result_different_data, "list")
  expect_true(all(result_different_data$overlap >= 0))

  # Insufficient overlap due to high min_overlap threshold
  test_data <- setup_test_data()
  result <- trs_tSt(test_data$x, test_data$y, min_overlap = 100) # Very high threshold
  expect_true(all(is.na(result$r_pearson)) || sum(!is.na(result$r_pearson)) == 0)
  expect_true(all(is.na(result$t_St)) || sum(!is.na(result$t_St)) == 0)
})

# Test 6: Missing values handling with realistic data
test_that("trs_tSt handles missing values in realistic data", {
  # Create data with intentional missing values but ensure same row structure
  test_data <- setup_test_data() # This ensures aligned data
  test_data_na <- test_data$x
  ref_data_na <- test_data$y

  # Introduce additional missing values
  test_data_na[sample(nrow(test_data_na), 5), 1] <- NA
  test_data_na[sample(nrow(test_data_na), 3), 3] <- NA
  ref_data_na[sample(nrow(ref_data_na), 4), 2] <- NA

  result <- trs_tSt(test_data_na, ref_data_na, min_overlap = 5)

  expect_type(result, "list")
  expect_true(all(!is.na(result$overlap))) # Overlap counts should be valid
  # Some values might be NA due to insufficient overlap, but structure should be intact
  expect_equal(dim(result$r_pearson), c(3, 2))
  expect_equal(dim(result$t_St), c(3, 2))
})

# Test 7: Rowname consistency
test_that("trs_tSt works without explicit rownames", {
  test_data <- setup_test_data()

  # Remove rownames from aligned data
  test_data <- setup_test_data()
  x_no_names <- test_data$x
  rownames(x_no_names) <- NULL

  y_no_names <- test_data$y
  rownames(y_no_names) <- NULL

  # Should still work with default rownames
  expect_error(trs_tSt(x_no_names, y_no_names), NA)
})

# Test 8: Correlation edge cases with realistic data
test_that("trs_tSt handles correlation edge cases with realistic data", {
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

  result <- trs_tSt(perfect_data1, perfect_data2, min_overlap = 10)

  # Should have high correlations
  valid_r <- result$r_pearson[!is.na(result$r_pearson)]
  if (length(valid_r) > 0) {
    expect_true(any(abs(valid_r) > 0.8)) # High correlations expected
  }

  # Should have high t-values due to strong correlation
  finite_t_values <- result$t_St[is.finite(result$t_St) & !is.na(result$t_St)]
  if (length(finite_t_values) > 0) {
    expect_true(any(abs(finite_t_values) > 5)) # High t-values expected
  }
})
