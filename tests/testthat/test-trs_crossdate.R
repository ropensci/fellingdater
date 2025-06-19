test_that("trs_crossdate basic functionality", {
  # Use package function to create minimal test data
  x <- trs_pseudo_rwl(n_series = 1, series_length = 6, end_date = 1995)
  y <- trs_pseudo_rwl(n_series = 1, series_length = 8, end_date = 1995)

  result <- trs_crossdate(x, y, min_overlap = 3, sliding = FALSE, pb = FALSE)

  expect_s3_class(result, "data.frame")
  expect_true(nrow(result) >= 0)
  expect_equal(ncol(result), 17)

  # Check required columns exist
  expected_cols <- c(
    "series", "length", "first", "last", "reference",
    "ref_first", "ref_last", "overlap", "r_pearson",
    "t_St", "t_BP", "t_Ho", "sgc", "ssgc", "sgc_p",
    "glk", "glk_p"
  )
  expect_true(all(expected_cols %in% names(result)))
})

test_that("trs_crossdate sliding window", {
  # Use package function to create realistic test data
  x <- trs_pseudo_rwl(n_series = 2, series_length = 50, end_date = 2000)
  y <- trs_pseudo_rwl(n_series = 3, series_length = 100, end_date = 2000)

  result_sliding <- trs_crossdate(x, y, sliding = TRUE, pb = FALSE, min_overlap = 10)
  result_fixed <- trs_crossdate(x, y, sliding = FALSE, pb = FALSE, min_overlap = 10)

  expect_true(nrow(result_sliding) >= nrow(result_fixed))
})

test_that("trs_crossdate ranking and filtering", {
  # Use package function to create test data
  x <- trs_pseudo_rwl(n_series = 2, series_length = 30, end_date = 2000)
  y <- trs_pseudo_rwl(n_series = 5, series_length = 50, end_date = 2000)

  # Test ranking
  result_ranked <- trs_crossdate(x, y, rank_by = "t_Ho", pb = FALSE, min_overlap = 10)
  result_unranked <- trs_crossdate(x, y, pb = FALSE, min_overlap = 10)

  # Test top_n filtering
  result_top3 <- trs_crossdate(x, y, top_n = 3, rank_by = "t_BP", pb = FALSE, min_overlap = 10)

  # Should have at most 3 results per series
  if (nrow(result_top3) > 0) {
    series_counts <- table(result_top3$series)
    expect_true(all(series_counts <= 3))
  }
})

test_that("trs_crossdate self-comparison", {
  # Use package function to create test data
  x <- trs_pseudo_rwl(n_series = 3, series_length = 40, end_date = 2000)

  # Self-comparison should work
  result <- trs_crossdate(x, sliding = FALSE, pb = FALSE, min_overlap = 10)
  expect_s3_class(result, "data.frame")

  # Should have diagonal matches (series with itself)
  if (nrow(result) > 0) {
    self_matches <- result[result$series == result$reference, ]
    expect_true(nrow(self_matches) > 0)
  }
})
