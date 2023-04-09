testthat::test_that("n_sapwood is present and a natural number", {
     #testthat::expect_error(
     #     sw_interval(),
     #     regexp = "argument \"n_sapwood\""
     #)
     testthat::expect_error(
          sw_interval(n_sapwood=-5),
          regexp = "n_sapwood"
     )
     testthat::expect_error(
          sw_interval(n_sapwood=22.456),
          regexp = "n_sapwood"
     )
     testthat::expect_error(
          sw_interval(n_sapwood="iets meer dan twee"),
          regexp = "n_sapwood"
     )
})

testthat::test_that("credMass is a number between 0 and 1", {
     testthat::expect_error(
          sw_interval(
               n_sapwood=5,
               credMass=-5
          ),
          regexp = "credMass"
     )
     testthat::expect_error(
          sw_interval(
               n_sapwood=5,
               credMass = "nulkommadink"
          ),
          regexp = "credMass"
     )
     testthat::expect_error(
          sw_interval(
               n_sapwood=5,
               credMass=34.56
          ),
          regexp = "credMass"
     )
})

testthat::test_that("hdi works with lognormal density function", {
     hdi <- sw_interval(
          n_sapwood = 10,
          last = 1234,
          hdi = TRUE,
          credMass = .95,
          sw_data = "Wazny_1990",
          densfun = "lognormal"
     )
     testthat::expect_equal(hdi$lower[1], 1234)
     testthat::expect_equal(hdi$upper[1], 1250)
     testthat::expect_gte(hdi$p[1], 0.95)
})

testthat::test_that("hdi works with normal density function", {
     hdi <- sw_interval(
          n_sapwood = 10,
          last = 1234,
          hdi = TRUE,
          credMass = .95,
          sw_data = "Wazny_1990",
          densfun = "normal"
     )
     testthat::expect_equal(hdi$lower[1], 1234)
     testthat::expect_equal(hdi$upper[1], 1248)
     testthat::expect_gte(hdi$p[1], 0.95)
})
