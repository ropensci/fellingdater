# Test helper function to create test data
create_test_rwl_select <- function() {
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

test_that("trs_select extracts single series correctly", {
     x <- create_test_rwl_select()

     result <- trs_select(x, series = "trs_1")

     expect_equal(ncol(result), 1)
     expect_equal(names(result), "trs_1")
     expect_equal(nrow(result), nrow(x))
     expect_identical(result$trs_1, x$trs_1)
     expect_equal(rownames(result), rownames(x))
})

test_that("trs_select extracts multiple series correctly", {
     x <- create_test_rwl_select()

     result <- trs_select(x, series = c("trs_1", "trs_3", "trs_5"))

     expect_equal(ncol(result), 3)
     expect_equal(names(result), c("trs_1", "trs_3", "trs_5"))
     expect_equal(nrow(result), nrow(x))
     expect_identical(result$trs_1, x$trs_1)
     expect_identical(result$trs_3, x$trs_3)
     expect_identical(result$trs_5, x$trs_5)
})

test_that("trs_select adds year column when requested", {
     x <- create_test_rwl_select()

     # Single series with years
     result1 <- trs_select(x, series = "trs_1", rownames_to_years = TRUE)
     expect_true("year" %in% names(result1))
     expect_equal(result1$year, 1990:1999)
     expect_type(result1$year, "double")

     # Multiple series with years
     result2 <- trs_select(x, series = c("trs_1", "trs_2"), rownames_to_years = TRUE)
     expect_true("year" %in% names(result2))
     expect_equal(result2$year, 1990:1999)
     expect_equal(ncol(result2), 3) # 2 series + year column
})

test_that("trs_select validates input parameters", {
     x <- create_test_rwl_select()

     # Test non-data.frame input
     expect_error(trs_select("not_a_dataframe", "trs_1"), "Input must be a data.frame")
     expect_error(trs_select(list(a = 1), "trs_1"), "Input must be a data.frame")

     # Test invalid series parameter
     expect_error(trs_select(x, series = 123), "`series` must be a non-empty character vector")
     expect_error(trs_select(x, series = character(0)), "`series` must be a non-empty character vector")
     expect_error(trs_select(x, series = c()), "`series` must be a non-empty character vector")

     # Test invalid rownames_to_years parameter
     expect_error(trs_select(x, "trs_1", rownames_to_years = "yes"), "`rownames_to_years` must be a single logical value")
     expect_error(trs_select(x, "trs_1", rownames_to_years = c(TRUE, FALSE)), "`rownames_to_years` must be a single logical value")

     # Test invalid trim parameter
     expect_error(trs_select(x, "trs_1", trim = "yes"), "`trim` must be a single logical value")
     expect_error(trs_select(x, "trs_1", trim = c(TRUE, FALSE)), "`trim` must be a single logical value")
})

test_that("trs_select handles missing series gracefully", {
     x <- create_test_rwl_select()

     # Single missing series
     expect_error(trs_select(x, series = "nonexistent"), "Series.*nonexistent.*not found in dataframe")

     # Multiple missing series
     expect_error(
          trs_select(x, series = c("missing1", "missing2")),
          "Series.*missing1.*missing2.*not found in dataframe"
     )

     # Mix of existing and missing series
     expect_error(
          trs_select(x, series = c("trs_1", "missing", "trs_2")),
          "Series.*missing.*not found in dataframe"
     )
})

test_that("trs_select handles data integrity with year column", {
     x <- create_test_rwl_select()

     # Test that original data is not modified
     original_x <- x
     result <- trs_select(x, series = c("trs_1", "trs_2"))
     expect_identical(x, original_x)

     # Test that result maintains data relationships
     result <- trs_select(x, series = c("trs_2", "trs_4"), rownames_to_years = TRUE)
     for (i in seq_len(nrow(result))) {
          year <- result$year[i]
          row_idx <- which(as.numeric(rownames(x)) == year)
          expect_equal(result$trs_2[i], x$trs_2[row_idx])
          expect_equal(result$trs_4[i], x$trs_4[row_idx])
     }
})

test_that("trs_select maintains rownames correctly", {
     x <- create_test_rwl_select()

     result <- trs_select(x, series = c("trs_1", "trs_3"))
     expect_equal(rownames(result), rownames(x))

     # Test with subset of original data
     x_subset <- x[3:7, ]
     class(x_subset) <- c("rwl", "data.frame")
     result <- trs_select(x_subset, series = "trs_2", rownames_to_years = TRUE)
     expect_equal(rownames(result), rownames(x_subset))
     expect_equal(result$year, 1992:1996)
})

test_that("trs_select trim functionality works correctly", {
     x <- create_test_rwl_select()

     # Add leading and trailing NA rows to simulate need for trimming
     x_with_na <- rbind(
          data.frame(trs_1 = NA, trs_2 = NA, trs_3 = NA, trs_4 = NA, trs_5 = NA),
          x,
          data.frame(trs_1 = NA, trs_2 = NA, trs_3 = NA, trs_4 = NA, trs_5 = NA)
     )
     rownames(x_with_na) <- 1989:2000
     class(x_with_na) <- c("rwl", "data.frame")

     # Mock trs_trim function for testing (since it may not be available)
     # In real testing, ensure trs_trim is loaded

     # Test without trim
     result1 <- trs_select(x_with_na, series = "trs_1")
     expect_equal(nrow(result1), 12) # Should include NA rows

     result2 <- trs_select(x_with_na, series = "trs_1", trim = TRUE)
     expect_equal(nrow(result2), 10) # Should remove NA rows
     expect_equal(class(result2), c("rwl", "data.frame"))
})
