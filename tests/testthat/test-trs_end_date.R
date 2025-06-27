# Test helper function to create test data
create_test_rwl_remove <- function() {
  set.seed(123)
  x <- data.frame(
    trs_1 = rnorm(10, 100, 6),
    trs_2 = rnorm(10, 120, 5),
    trs_3 = rnorm(10, 80, 5),
    trs_4 = rnorm(10, 110, 10),
    trs_5 = rnorm(10, 0, 5)
  )
  rownames(x) <- 1990:1999
  class(x) <- c("rwl", "data.frame")
  x
}

# Test helper for data with NA rows (for trim testing)
create_test_rwl_with_na <- function() {
  x <- create_test_rwl_remove()
  # Add leading NA rows
  na_start <- data.frame(
    trs_1 = c(NA, NA),
    trs_2 = c(NA, NA),
    trs_3 = c(NA, NA),
    trs_4 = c(NA, NA),
    trs_5 = c(NA, NA)
  )
  rownames(na_start) <- 1988:1989
  # Add trailing NA rows
  na_end <- data.frame(
    trs_1 = c(NA, NA),
    trs_2 = c(NA, NA),
    trs_3 = c(NA, NA),
    trs_4 = c(NA, NA),
    trs_5 = c(NA, NA)
  )
  rownames(na_end) <- 2000:2001
  result <- rbind(na_start, x, na_end)
  class(result) <- c("rwl", "data.frame")
  result
}

test_that("trs_end_date basic functionality", {
  x <- create_test_rwl_remove()

  # Test with message suppression since we expect a message
  expect_message(
    result <- trs_end_date(x, end_year = 2020),
    "End date set for all series in x"
  )

  expect_equal(rownames(result), as.character(2011:2020))
  expect_equal(result$trs_1, x$trs_1)
  expect_equal(result$trs_2, x$trs_2)
  expect_s3_class(result, "rwl")
  expect_equal(ncol(result), 5)
  expect_equal(nrow(result), 10)
})

test_that("trs_end_date input validation", {
  x <- create_test_rwl_remove()

  # Non-data.frame input
  expect_error(
    trs_end_date(matrix(1:10, nrow = 5)),
    "'x' must be a data.frame"
  )

  # Invalid end_year - updated error message
  expect_error(
    trs_end_date(x, end_year = c(2020, 2021)),
    "'end_year' should be a single numeric value representing a calendar year"
  )
  expect_error(
    trs_end_date(x, end_year = "2020"),
    "'end_year' should be a single numeric value representing a calendar year"
  )
  expect_error(
    trs_end_date(x, end_year = NA),
    "'end_year' should be a single numeric value representing a calendar year"
  )
  expect_error(
    trs_end_date(x, end_year = Inf),
    "'end_year' should be a single numeric value representing a calendar year"
  )

  # Invalid trim
  expect_error(
    trs_end_date(x, end_year = 2020, trim = "yes"),
    "'trim' must be a single logical value"
  )
  expect_error(
    trs_end_date(x, end_year = 2020, trim = c(TRUE, FALSE)),
    "'trim' must be a single logical value"
  )
})

test_that("trs_end_date edge cases", {
  # Single row
  x <- data.frame(series1 = 1)
  result <- trs_end_date(x, end_year = 2020)
  expect_equal(rownames(result), "2020")

  # Large dataset
  x <- data.frame(series1 = 1:1000)
  class(x) <- c("rwl", "data.frame")
  result <- trs_end_date(x, end_year = 2020)
  expect_equal(as.numeric(rownames(result)[1]), 1021)
  expect_equal(as.numeric(tail(rownames(result), 1)), 2020)
})

test_that("trs_end_date preserves data integrity", {
  x <- create_test_rwl_remove()

  expect_message(
    result <- trs_end_date(x, end_year = 1950),
    "End date set for all series in x"
  )

  # Check data values are unchanged
  expect_equal(result$trs_1, x$trs_1)
  expect_equal(result$trs_2, x$trs_2)
  expect_equal(result$trs_3, x$trs_3)
  expect_equal(result$trs_4, x$trs_4)
  expect_equal(result$trs_5, x$trs_5)

  # Check column names unchanged
  expect_equal(colnames(result), colnames(x))

  # Check years assigned correctly (10 rows, ending at 1950)
  expect_equal(rownames(result), as.character(1941:1950))
})


test_that("trs_end_date warnings", {
  # Test message for multiple series (ncol != 1)
  x <- create_test_rwl_remove() # This has 5 columns
  expect_message(
    trs_end_date(x, end_year = 2020),
    "End date set for all series in x"
  )

  # Test no message for single column
  x_single <- data.frame(trs_1 = rnorm(5))
  class(x_single) <- c("rwl", "data.frame")
  expect_silent(trs_end_date(x_single, end_year = 2020))
})
