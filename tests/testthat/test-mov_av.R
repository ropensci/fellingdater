testthat::test_that("length oputput equals input", {
        num_vec <- rnorm(100)
        x <- mov_av(num_vec, w = 20)
        testthat::expect_equal(length(num_vec), length(x))
})


testthat::test_that("does not work with align = random character", {
        num_vec <- rnorm(100)
        testthat::expect_error(length(mov_av(
                num_vec, w = 20, align = "in 't midden"
        )))
})
