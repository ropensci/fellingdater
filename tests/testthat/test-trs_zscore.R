test_that("trs_zscore basic functionality", {
  # Create test data
  test_data <- data.frame(
    series1 = c(1, 2, 3, 4, 5),
    series2 = c(10, 20, 30, 40, 50)
  )
  rownames(test_data) <- 2000:2004

  result <- trs_zscore(test_data)

  # Check dimensions preserved
  expect_equal(dim(result), dim(test_data))
  expect_equal(rownames(result), rownames(test_data))
  expect_equal(colnames(result), colnames(test_data))

  # Check z-score properties (mean ≈ 0, sd ≈ 1)
  expect_equal(mean(result$series1, na.rm = TRUE), 0, tolerance = 1e-10)
  expect_equal(mean(result$series2, na.rm = TRUE), 0, tolerance = 1e-10)
  expect_equal(sd(result$series1, na.rm = TRUE), 1, tolerance = 1e-10)
  expect_equal(sd(result$series2, na.rm = TRUE), 1, tolerance = 1e-10)
})

test_that("trs_zscore handles NA values correctly", {
  test_data <- data.frame(
    series1 = c(1, 2, NA, 4, 5),
    series2 = c(NA, 20, 30, 40, NA)
  )
  rownames(test_data) <- 2000:2004

  result <- trs_zscore(test_data)

  # Check that NA positions are preserved in original locations
  expect_true(is.na(result$series1[3]))
  expect_true(is.na(result$series2[1]))
  expect_true(is.na(result$series2[5]))

  # Check z-score properties for non-NA values
  valid_s1 <- result$series1[!is.na(result$series1)]
  valid_s2 <- result$series2[!is.na(result$series2)]
  expect_equal(mean(valid_s1), 0, tolerance = 1e-10)
  expect_equal(mean(valid_s2), 0, tolerance = 1e-10)
})

test_that("trs_zscore handles edge cases", {
  # Test with all NA column
  test_data1 <- data.frame(
    series1 = c(1, 2, 3),
    series2 = c(NA, NA, NA)
  )
  rownames(test_data1) <- 2000:2002

  result1 <- trs_zscore(test_data1)
  expect_true(all(is.na(result1$series2)))
  expect_false(any(is.na(result1$series1)))

  # Test with zero variance (constant values)
  test_data2 <- data.frame(
    series1 = c(1, 2, 3),
    series2 = c(5, 5, 5)
  )
  rownames(test_data2) <- 2000:2002
  class(test_data2) <- c("rwl", "data.frame")

  result2 <- trs_zscore(test_data2)
  expect_true(all(is.na(result2$series2)))
  expect_false(any(is.na(result2$series1)))

  # Test with single valid value (insufficient data)
  test_data3 <- data.frame(
    series1 = c(1, 2, 3),
    series2 = c(5, NA, NA)
  )
  rownames(test_data3) <- 2000:2002
  class(test_data3) <- c("rwl", "data.frame")

  result3 <- trs_zscore(test_data3)
  expect_true(all(is.na(result3$series2)))
  expect_false(any(is.na(result3$series1)))

  # Test with zero variance after removing NAs
  test_data4 <- data.frame(
    series1 = c(1, 2, 3),
    series2 = c(5, 5, NA)
  )
  rownames(test_data4) <- 2000:2002
  class(test_data4) <- c("rwl", "data.frame")

  result4 <- trs_zscore(test_data4)
  expect_true(all(is.na(result4$series2)))
  expect_false(any(is.na(result4$series1)))
})

test_that("trs_zscore input validation", {
  # Test non-data.frame input
  expect_error(trs_zscore(c(1, 2, 3)), "Input must be a data.frame")
  expect_error(trs_zscore(matrix(1:6, nrow = 2)), "Input must be a data.frame")

  # Test empty data.frame
  expect_error(trs_zscore(data.frame()), "Input data.frame cannot be empty")

  # Test data.frame with zero rows
  empty_df <- data.frame(series1 = numeric(0))
  expect_error(trs_zscore(empty_df), "Input data.frame cannot be empty")
})

test_that("trs_zscore mathematical correctness", {
  # Test with known values
  test_data <- data.frame(
    series1 = c(0, 1, 2, 3, 4) # mean = 2, sd = sqrt(2.5) ≈ 1.58
  )
  rownames(test_data) <- 2000:2004
  class(test_data) <- c("rwl", "data.frame")

  result <- trs_zscore(test_data)

  # Manual calculation: (x - 2) / sqrt(2.5)
  expected <- (c(0, 1, 2, 3, 4) - 2) / sqrt(2.5)

  expect_equal(as.numeric(result$series1), expected, tolerance = 1e-10)
})
