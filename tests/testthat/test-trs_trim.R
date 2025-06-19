# Test helper function to create test data
create_test_rwl <- function() {
  set.seed(123)
  x <- data.frame(
    series1 = c(NA, 222, 111, 123, 213, 100, 321, NA, NA),
    series2 = c(NA, NA, 50, 213, 200, 100, NA, NA, NA),
    series3 = c(NA, NA, -1.5, 0, 1.6, -1.3, 1.8, 1, NA)
  )
  rownames(x) <- 1990:1998
  class(x) <- c("rwl", "data.frame")
  x
}

test_that("trs_trim removes leading and trailing all-NA rows", {
  x <- create_test_rwl()
  result <- trs_trim(x)

  expect_equal(nrow(result), 7) # Should keep rows 2-8 (1991-1997)
  expect_equal(rownames(result), c("1991", "1992", "1993", "1994", "1995", "1996", "1997"))
  expect_false(any(apply(result, 1, function(row) all(is.na(row)))))
})

test_that("trs_trim preserves zero values", {
  x <- create_test_rwl()
  result <- trs_trim(x)

  # Check that zeros are preserved
  expect_true(any(result == 0, na.rm = TRUE))
  expect_equal(result$series3[3], 0) # Row 1994, should contain zero
})

test_that("trs_trim handles rownames_to_years parameter", {
  x <- create_test_rwl()

  # Without year column
  result1 <- trs_trim(x, rownames_to_years = FALSE)
  expect_false("year" %in% names(result1))

  # With year column
  result2 <- trs_trim(x, rownames_to_years = TRUE)
  expect_true("year" %in% names(result2))
  expect_equal(result2$year, c(1991:1997))
  expect_type(result2$year, "double")
})

test_that("trs_trim handles data with no trimming needed", {
  x <- data.frame(
    a = c(1, 2, 3),
    b = c(4, 5, 6)
  )
  rownames(x) <- 2000:2002

  result <- trs_trim(x)
  expect_identical(x, result)
})

test_that("trs_trim handles single row data", {
  x <- data.frame(a = 1, b = 2)
  rownames(x) <- "2000"

  result <- trs_trim(x)
  expect_identical(x, result)
})

test_that("trs_trim validates input types", {
  expect_error(trs_trim("not_a_dataframe"), "Input must be a data.frame")
  expect_error(trs_trim(list(a = 1, b = 2)), "Input must be a data.frame")
})

test_that("trs_trim preserves column names and structure", {
  x <- create_test_rwl()
  result <- trs_trim(x)

  expect_equal(names(result), names(x))
  expect_s3_class(result, "data.frame")
  expect_true("rwl" %in% class(result)) # Check rwl class is preserved
})
