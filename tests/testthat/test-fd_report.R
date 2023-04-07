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
                    "series" = "id",
                    "last"="end"
          ),
          regexp = "n_sapwood"
     )
})

testthat::test_that("last must be numeric", {
     testthat::expect_error(
          fd_report(testdata,
                    "series" = "id"
          ),
          regexp = "last"
     )
     testdata <- data.frame(series = c("aaa", "bbb", "ccc"),
                            n_sapwood = c(10, 11, 12),
                            waneyedge = c(FALSE, FALSE,TRUE),
                            last = c("honderddrieentwintig", 456, 1789))
     testthat::expect_error(
          fd_report(testdata),
          regexp = "last"
     )
})

testthat::test_that("waneyedge must be boolean", {
     testthat::expect_error(
          fd_report(testdata,
                    "series" = "id",
                    "last"="end",
                    "n_sapwood"="swr",
                    "waneyedge" = "waynesedge"
          ),
          regexp = "waneyedge"
     )
     testdata <- data.frame(id = c("aaa", "bbb", "ccc"),
                            swr = c(10, 11, 12),
                            waneyedge = c("ja", "nee", 345),
                            end = c(123, 456, 1789))
     testthat::expect_warning(
          fd_report(testdata,
                    "series" = "id",
                    "last"="end",
                    "n_sapwood"="swr",
          ),
          regexp = "waneyedge"
     )
})

testthat::test_that("credMass must be between 0 and 1", {
     testthat::expect_error(
          fd_report(testdata,
                    "series" = "id",
                    "last" = "end",
                    "n_sapwood" = "swr",
                    "credMass" = "lots"
          ),
          regexp = "credMass"
     )
     testthat::expect_error(
          fd_report(testdata,
                    "series" = "id",
                    "last" = "end",
                    "n_sapwood" = "swr",
                    "credMass" = -145
          ),
          regexp = "credMass"
     )
     testthat::expect_error(
          fd_report(testdata,
                    "series" = "id",
                    "last" = "end",
                    "n_sapwood" = "swr",
                    "credMass" = 1.01
          ),
          regexp = "credMass"
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
