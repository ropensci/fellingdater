testdata <- data.frame(id = c("aaa", "bbb", "ccc"),
                  swr = c(10, 11, 12),
                  waneyedge = c(FALSE, FALSE,TRUE),
                  end = c(123, 456, 1789))

testthat::test_that("Wrong arguments generate errors", {
     testthat::expect_error(
          fd_report(),
          regexp = "argument \"x\""
     )
     testthat::expect_error(
          fd_report(testdata,
          ),
          regexp = "series"
     )
     testthat::expect_error(
          fd_report(testdata,
               "series" = "id"
          ),
          regexp = "last"
     )
     testthat::expect_error(
          fd_report(testdata,
                    "series" = "id",
                    "last"="end"
          ),
          regexp = "n_sapwood"
     )
     testthat::expect_error(
          fd_report(testdata,
                    "series" = "id",
                    "last"="end",
                    "n_sapwood"="swr",
                    "waneyedge" = "waynesedge"
          ),
          regexp = "waneyedge"
     )
})

testthat::test_that("Output is a data.frame", {
     x <- fd_report(testdata,
                    "series" = "id",
                    "last"="end",
                    "n_sapwood"="swr"
     )
     testthat::expect_s3_class(x, "data.frame")
})
