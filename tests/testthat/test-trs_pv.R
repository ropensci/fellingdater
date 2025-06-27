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

test_that("trs_pv basic functionality", {
  test_data <- setup_test_data()
  x <- test_data$x
  y <- test_data$y

  # Use min_overlap = 20 but expect the warning
  expect_warning(
    result <- trs_pv(x, y, min_overlap = 20),
    "minimum number of overlap is lower than 50"
  )

  # Check structure
  expect_type(result, "list")
  expect_named(result, c("glk", "sgc", "ssgc", "overlap", "glk_p", "sgc_p"))

  # Check dimensions (3 series in x, 2 series in y)
  expect_equal(dim(result$glk), c(3, 2))
  expect_equal(dim(result$overlap), c(3, 2))

  # Check that overlap values are reasonable (should be around 25: 1976-2000)
  expect_true(all(result$overlap >= 20))
  expect_true(all(result$overlap <= 40))
})

test_that("trs_pv input validation", {
  test_data <- setup_test_data()
  x <- test_data$x

  # Non-data.frame inputs
  expect_error(trs_pv(as.matrix(x)), "is.data.frame\\(x\\) is not TRUE")
  expect_error(trs_pv(x, as.matrix(x)), "is.data.frame\\(y\\) is not TRUE")

  # Non-numeric data
  x_char <- x
  x_char[, 1] <- as.character(x_char[, 1])
  expect_error(trs_pv(x_char), "must be numeric data.frames")

  # Invalid min_overlap
  expect_error(trs_pv(x, min_overlap = 2), "should be a single integer >= 3")
  expect_error(trs_pv(x, min_overlap = 10.5), "should be a single integer >= 3")

  # Invalid logical parameters
  expect_error(trs_pv(x, prob = "TRUE"), "must be logical values")
  expect_error(trs_pv(x, as_df = 1), "must be logical values")
})


test_that("trs_pv data frame output", {
  test_data <- setup_test_data()
  x <- test_data$x
  y <- test_data$y

  result_df <- trs_pv(x, y, as_df = TRUE, min_overlap = 50)

  # Check structure
  expect_s3_class(result_df, "data.frame")
  expect_equal(nrow(result_df), 6) # 3x2 combinations

  # Check required columns
  expected_cols <- c("series", "reference", "glk", "glk_p", "sgc", "ssgc", "sgc_p", "overlap")
  expect_true(all(expected_cols %in% colnames(result_df)))

  # Check series names match the input data
  expect_equal(length(unique(result_df$series)), 3)
  expect_equal(length(unique(result_df$reference)), 2)
})

test_that("trs_pv min_overlap warning", {
  x <- trs_pseudo_rwl(n_series = 2, series_length = 50, end_date = 2020, seed = 123)

  # Should warn when min_overlap < 50
  expect_warning(
    trs_pv(x, min_overlap = 30),
    "minimum number of overlap is lower than 50"
  )

  # Should not warn when min_overlap >= 50
  expect_silent(trs_pv(x, min_overlap = 50))
})

test_that("trs_pv output consistency", {
  x <- trs_pseudo_rwl(n_series = 2, series_length = 50, end_date = 2020, seed = 123)

  result_list <- trs_pv(x, as_df = FALSE)
  result_df <- trs_pv(x, as_df = TRUE)

  # Check that values are consistent between formats
  expect_equal(result_list$glk[1, 1], result_df$glk[1])
  expect_equal(result_list$overlap[1, 2], result_df$overlap[2])
})
