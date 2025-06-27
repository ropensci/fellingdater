test_that("trs_tho_transform basic functionality", {
  # Test data setup using trs_pseudo_rwl
  setup_test_data <- function() {
    # Create realistic test data using trs_pseudo_rwl
    x_data <- trs_pseudo_rwl(
      n_series = 3,
      series_length = 50,
      end_date = 2020,
      common_signal = 0.4,
      seed = 123
    )
    y_data <- trs_pseudo_rwl(
      n_series = 2,
      series_length = 40,
      end_date = 2015,
      common_signal = 0.3,
      seed = 456
    )
    list(x = x_data, y = y_data)
  }

  test_data <- setup_test_data()
  x <- test_data$x

  result <- trs_tho_transform(x)

  # Check structure
  expect_true(is.data.frame(result))
  expect_equal(dim(result), dim(x))
  expect_equal(rownames(result), rownames(x))
  expect_equal(colnames(result), colnames(x))

  # First row should be all NA (no previous year for ratio calculation)
  expect_true(all(is.na(result[1, ])))

  # Check that result contains finite values in subsequent rows
  expect_true(any(is.finite(as.matrix(result[2:nrow(result), ]))))
})

test_that("trs_tho_transform handles NA values", {
  setup_test_data <- function() {
    x_data <- trs_pseudo_rwl(
      n_series = 2,
      series_length = 30,
      end_date = 2020,
      common_signal = 0.3,
      seed = 789
    )
    list(x = x_data)
  }

  test_data <- setup_test_data()
  x <- test_data$x

  # Introduce some NA values
  x[5:8, 1] <- NA
  x[15:18, 2] <- NA

  result <- trs_tho_transform(x)

  # Should not throw error
  expect_true(is.data.frame(result))
  expect_equal(dim(result), dim(x))

  # First row should still be all NA
  expect_true(all(is.na(result[1, ])))

  # Check that some values are still computed despite NAs
  expect_true(any(is.finite(as.matrix(result[2:nrow(result), ]))))
})

test_that("trs_tho_transform input validation", {
  # Non-data.frame input
  expect_error(trs_tho_transform(c(1, 2, 3)))
  expect_error(trs_tho_transform(matrix(1:6, nrow = 2)))

  # Non-numeric columns
  x_bad <- data.frame(series1 = c("a", "b", "c"))
  expect_error(trs_tho_transform(x_bad))

  # Mixed types
  x_mixed <- data.frame(
    series1 = c(1.0, 1.2, 1.5),
    series2 = c("a", "b", "c")
  )
  expect_error(trs_tho_transform(x_mixed))

  # Too few rows (< 2)
  x_short <- data.frame(series1 = c(1.0))
  expect_error(
    trs_tho_transform(x_short),
    "Input data must have at least 2 rows to compute growth ratios"
  )
})
