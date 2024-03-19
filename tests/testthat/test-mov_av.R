testthat::test_that("length oputput equals input", {
        num_vec <- rnorm(100)
        x <- mov_av(num_vec, w = 20)
        testthat::expect_equal(length(num_vec), length(x))
})

testthat::test_that("length oputput equals input", {
        num_vec <- rnorm(100)
        x <- mov_av(num_vec, w = 20, edges = "fill")
        testthat::expect_equal(length(num_vec), length(x))
})

testthat::test_that("length oputput equals input", {
        num_vec <- rnorm(100)
        x <- mov_av(num_vec, w = 20, edges = "nofill")
        testthat::expect_equal(length(num_vec), length(x))
})

testthat::test_that("does not work with align = random character", {
        num_vec <- rnorm(100)
        testthat::expect_error(mov_av(
                num_vec, w = 20, align = "in 't midden"
        ))
})

testthat::test_that("does not work with edges = random character", {
        num_vec <- rnorm(100)
        testthat::expect_error(mov_av(
                num_vec, w = 20, edges = "cut")
        )
})
