testthat::test_that("Data must be present", {
        testthat::expect_error(fd_report(),
                               regexp = "argument \"x\"")
})

testthat::test_that("Series must be present", {
        testthat::expect_error(fd_report(trs_example2,
                                         series = "bestaat_niet"),
                               regexp = "series")
        testdata <- data.frame(
                series = c(NA, "bbb", "ccc"),
                n_sapwood = c(10, 11, 12),
                waneyedge = c(FALSE, FALSE, TRUE),
                last = c(123, 456, 1789)
        )
        testthat::expect_error(fd_report(testdata),
                               regexp = "series")
})

testthat::test_that("n_sapwood must be numeric", {
        testthat::expect_error(fd_report(trs_example1,
                                         n_sapwood = "bestaat_niet"),
                               regexp = "n_sapwood")
        testdata <- data.frame(
                series = c("aaa", "bbb", "ccc"),
                n_sapwood = c("tien", 11, 12),
                waneyedge = c(FALSE, FALSE, TRUE),
                last = c(123, 456, 1789)
        )
        testthat::expect_error(fd_report(testdata),
                               regexp = "n_sapwood")
})

testthat::test_that("last must be numeric", {
        testthat::expect_error(fd_report(trs_example2,
                                         last = "bestaat_niet"),
                               regexp = "last")
        testdata <- data.frame(
                series = c("aaa", "bbb", "ccc"),
                n_sapwood = c(10, 11, 12),
                waneyedge = c(FALSE, FALSE, TRUE),
                last = c("honderddrieentwintig", 456, 1789)
        )
        testthat::expect_error(fd_report(testdata),
                               regexp = "last")
})

testthat::test_that("waneyedge must be boolean", {
        testthat::expect_error(fd_report(trs_example3,
                                         waneyedge = "waynesedge"),
                               regexp = "waneyedge")
        testdata <- data.frame(
                series = c("aaa", "bbb", "ccc"),
                n_sapwood = c(10, 11, 12),
                waneyedge = c("ja", "nee", 345),
                last = c(123, 456, 1789)
        )
        testthat::expect_error(fd_report(testdata),
                               regexp = "waneyedge")
})

testthat::test_that("credMass must be between 0 and 1", {
        testthat::expect_error(fd_report(trs_example4,
                                         "credMass" = "lots"),
                               regexp = "credMass")
        testthat::expect_error(fd_report(trs_example5,
                                         "credMass" = -145),
                               regexp = "credMass")
        testthat::expect_error(fd_report(trs_example6,
                                         "credMass" = 1.01),
                               regexp = "credMass")
})

testthat::test_that("Output is a data.frame", {
        x <- fd_report(trs_example7)
        testthat::expect_s3_class(x, "data.frame")
})


testthat::test_that("Waneyedge gives exact date", {
        x <- fd_report(trs_example3)
        testthat::expect_equal(x$lower[3],
                               NA_real_)
        testthat::expect_equal(x$upper[3],
                               1008)
        testthat::expect_equal(x$felling_date[3],
                               "in 1008")
})

testthat::test_that("No Waneyedge gives between date", {
        x <- fd_report(trs_example1)
        testthat::expect_equal(x$lower[2],
                               x$last[2])
        testthat::expect_gte(x$upper[1],
                             x$last[1] + x$n_sapwood[1])
        testthat::expect_match(x$felling_date[1],
                               sprintf("^between %d and %d$", x$lower[1], x$upper[1]))
})
