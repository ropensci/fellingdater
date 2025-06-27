# Test 1: Basic functionality
test_that("Basic functionality works", {
  result <- trs_pseudo_rwl(n_series = 3, series_length = 10, seed = 123)

  expect_s3_class(result, "data.frame")
  expect_equal(ncol(result), 3)
  expect_true(nrow(result) <= 10)
  expect_true(all(grepl("^trs_", colnames(result))))
})

# Test 2: Input validation
test_that("Input validation works correctly", {
  # n_series validation
  expect_error(trs_pseudo_rwl(n_series = 0), "`n_series` must be a positive integer")
  expect_error(trs_pseudo_rwl(n_series = 1.5), "`n_series` must be a positive integer")
  expect_error(trs_pseudo_rwl(n_series = c(1, 2)), "`n_series` must be a positive integer")

  # series_length validation
  expect_error(
    trs_pseudo_rwl(series_length = c(10, 5)),
    "Second value of `series_length` should be larger than the first"
  )
  expect_error(
    trs_pseudo_rwl(series_length = 0),
    "`series_length` values must be positive"
  )
  expect_error(
    trs_pseudo_rwl(series_length = c(1, 2, 3)),
    "`series_length` should be numeric or a vector of length 2"
  )

  # end_date validation
  expect_error(
    trs_pseudo_rwl(end_date = c(2020, 2010)),
    "Second value of `end_date` should be larger than the first"
  )

  # trend validation
  expect_error(
    trs_pseudo_rwl(trend = "invalid"),
    "`trend` must be NULL, 'neg_lin', or 'neg_exp'"
  )

  # common_signal_strength validation
  expect_error(
    trs_pseudo_rwl(common_signal_strength = 1.5),
    "`common_signal_strength` must be a numeric value between 0 and 1"
  )
  expect_error(
    trs_pseudo_rwl(common_signal_strength = -0.1),
    "`common_signal_strength` must be a numeric value between 0 and 1"
  )

  # Other parameters
  expect_error(
    trs_pseudo_rwl(mean_rw = -10),
    "`mean_rw` must be a positive numeric value"
  )
  expect_error(
    trs_pseudo_rwl(noise_sd = 0),
    "`noise_sd` must be a positive numeric value"
  )
  expect_error(
    trs_pseudo_rwl(prefix = c("a", "b")),
    "`prefix` must be a single character string"
  )
})

# Test 3: Variable length and end dates
test_that("Variable length and end dates work", {
  result <- trs_pseudo_rwl(
    n_series = 5,
    series_length = c(8, 12),
    end_date = c(2020, 2024),
    seed = 456
  )

  expect_equal(ncol(result), 5)
  # Check that we have reasonable year range
  years <- as.numeric(rownames(result))
  expect_true(max(years) >= 2020 && max(years) <= 2024)
})

# Test 4: Related vs independent series
test_that("Related and independent series behave differently", {
  # Related series should have higher correlations
  related_data <- trs_pseudo_rwl(
    n_series = 5,
    series_length = 50,
    related = TRUE,
    common_signal_strength = 0.8,
    seed = 789
  )

  independent_data <- trs_pseudo_rwl(
    n_series = 5,
    series_length = 50,
    related = FALSE,
    seed = 789
  )

  # Calculate mean correlation (excluding diagonal)
  cor_mat_related <- cor(related_data, use = "complete.obs")
  cor_mat_independent <- cor(independent_data, use = "complete.obs")

  mean_cor_related <- mean(cor_mat_related[upper.tri(cor_mat_related)])
  mean_cor_independent <- mean(cor_mat_independent[upper.tri(cor_mat_independent)])

  expect_true(mean_cor_related > mean_cor_independent)
})

# Test 5: Autoregressive parameters
test_that("AR parameters work correctly", {
  # Test with AR
  ar_data <- trs_pseudo_rwl(
    n_series = 3,
    series_length = 30,
    ar = TRUE,
    ar_params = c(0.7, 0.2),
    seed = 654
  )

  # Test without AR
  no_ar_data <- trs_pseudo_rwl(
    n_series = 3,
    series_length = 30,
    ar = FALSE,
    seed = 654
  )

  expect_s3_class(ar_data, "rwl")
  expect_s3_class(no_ar_data, "rwl")
})

# Test 6: Reproducibility with seed
test_that("Seed parameter ensures reproducibility", {
  result1 <- trs_pseudo_rwl(n_series = 3, series_length = 10, seed = 999)
  result2 <- trs_pseudo_rwl(n_series = 3, series_length = 10, seed = 999)

  expect_identical(result1, result2)
})

# Test 7: Output format and structure
test_that("Output has correct format and structure", {
  result <- trs_pseudo_rwl(n_series = 4, series_length = 15, seed = 111)

  # Check classes
  expect_s3_class(result, c("data.frame"))

  # Check column names
  expect_true(all(grepl("^trs_", colnames(result))))

  # Check row names are years
  expect_true(all(grepl("^[0-9]{4}$", rownames(result))))

  # Check all values are numeric
  expect_true(all(sapply(result, is.numeric)))

  # Check for integer values (rounded ring widths)
  non_na_values <- unlist(result[!is.na(result)])
  expect_true(all(non_na_values == round(non_na_values)))
})
