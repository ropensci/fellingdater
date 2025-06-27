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

test_that("trs_remove basic functionality", {
  test_rwl <- create_test_rwl_remove()

  # Test single series removal
  result <- trs_remove(test_rwl, series = "trs_1")
  expect_equal(ncol(result), 4)
  expect_false("trs_1" %in% names(result))
  expect_true(all(c("trs_2", "trs_3", "trs_4", "trs_5") %in% names(result)))
  expect_equal(nrow(result), 10)
})

test_that("trs_remove multiple series", {
  test_rwl <- create_test_rwl_remove()

  result <- trs_remove(test_rwl, series = c("trs_1", "trs_3", "trs_5"))
  expect_equal(ncol(result), 2)
  expect_equal(names(result), c("trs_2", "trs_4"))
  expect_equal(nrow(result), 10)
})

test_that("trs_remove with rownames_to_years", {
  test_rwl <- create_test_rwl_remove()

  result <- trs_remove(test_rwl, series = "trs_1", rownames_to_years = TRUE)
  expect_true("year" %in% names(result))
  expect_equal(result$year, 1990:1999)
  expect_equal(ncol(result), 5) # 4 remaining series + year column
})

test_that("trs_remove with trim functionality", {
  # Note: This test assumes trs_trim() function exists
  # You may need to mock this function if it's not available during testing
  test_rwl <- create_test_rwl_remove()

  # Test without actual NA rows (trim should have no effect)
  result <- trs_remove(test_rwl, series = "trs_1", trim = TRUE)
  expect_equal(ncol(result), 4)
  expect_equal(nrow(result), 10)
})

test_that("trs_remove input validation", {
  test_rwl <- create_test_rwl_remove()

  # Test non-data.frame input
  expect_error(trs_remove(c(1, 2, 3), "trs_1"), "Input must be a data.frame")

  # Test non-character series
  expect_error(trs_remove(test_rwl, series = 123), "'series' must be a character vector")
  expect_error(trs_remove(test_rwl, series = c("trs_1", 2)), 'The following series were not found in the data.frame: "2"')

  # Test invalid rownames_to_years
  expect_error(
    trs_remove(test_rwl, "trs_1", rownames_to_years = "yes"),
    "'rownames_to_years' must be a single logical value"
  )
  expect_error(
    trs_remove(test_rwl, "trs_1", rownames_to_years = c(TRUE, FALSE)),
    "'rownames_to_years' must be a single logical value"
  )

  # Test invalid trim parameter
  expect_error(
    trs_remove(test_rwl, "trs_1", trim = "yes"),
    "'trim' must be a single logical value"
  )
  expect_error(
    trs_remove(test_rwl, "trs_1", trim = c(TRUE, FALSE)),
    "'trim' must be a single logical value"
  )
})

test_that("trs_remove missing series error", {
  test_rwl <- create_test_rwl_remove()

  # Test single missing series
  expect_error(
    trs_remove(test_rwl, series = "nonexistent"),
    'The following series were not found in the data.frame: "nonexistent"'
  )

  # Test multiple missing series
  expect_error(
    trs_remove(test_rwl, series = c("missing1", "missing2")),
    'The following series were not found in the data.frame: "missing1", "missing2"'
  )

  # Test mix of existing and missing series
  expect_error(
    trs_remove(test_rwl, series = c("trs_1", "missing", "trs_2")),
    'The following series were not found in the data.frame: "missing"'
  )
})

test_that("trs_remove removing all columns", {
  test_rwl <- create_test_rwl_remove()
  all_series <- names(test_rwl)

  # Should error when removing all columns without rownames_to_years
  expect_error(
    trs_remove(test_rwl, series = all_series),
    "Cannot remove all series unless 'rownames_to_years = TRUE'"
  )

  # Should work when removing all columns WITH rownames_to_years = TRUE
  result <- trs_remove(test_rwl, series = all_series, rownames_to_years = TRUE)
  expect_equal(ncol(result), 1)
  expect_equal(names(result), "year")
  expect_equal(result$year, 1990:1999)
})


test_that("trs_remove combined parameters", {
  test_rwl <- create_test_rwl_remove()

  # Test removing series with both rownames_to_years and trim
  result <- trs_remove(test_rwl,
    series = c("trs_1", "trs_2"),
    rownames_to_years = TRUE, trim = TRUE
  )
  expect_equal(ncol(result), 4) # 3 remaining series + year
  expect_true("year" %in% names(result))
  expect_true(all(c("trs_3", "trs_4", "trs_5") %in% names(result)))
})
