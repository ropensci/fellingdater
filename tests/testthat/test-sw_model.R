testthat::test_that("credMass is a number between 0 and 1", {
     testthat::expect_error(
          sw_model(
               credMass=-5
          ),
          regexp = "credMass"
     )
     testthat::expect_error(
          sw_model(
               credMass = "nulkommadink"
          ),
          regexp = "credMass"
     )
     testthat::expect_error(
          sw_model(
               credMass=34.56
          ),
          regexp = "credMass"
     )
})

testthat::test_that("sw_data exists", {
     testthat::expect_error(
          sw_model(
               sw_data = "Van_Daele_1978"
          ),
          regexp = "sw_data should be"
     )
})

testthat::test_that("sw_model does not work with invalid density function", {
     testthat::expect_error(
          sw_model(
               densfun = "nuka-cola"
          ),
          regexp = "not a supported distribution"
     )
})

testthat::test_that("sw_model with plot FALSE returns list", {
     model = sw_model(
          densfun = "normal",
          credMass = 0.933,
          plot = FALSE
     )
     testthat::expect_type(
          model,
          "list"
     )
     testthat::expect_equal(
          model$density_function,
          "normal"
     )
     testthat::expect_type(
          model$n,
          "integer"
     )
     testthat::expect_type(
          model$range[0],
          "double"
     )
     testthat::expect_type(
          model$range[1],
          "double"
     )
     testthat::expect_type(
          model$range[2],
          "double"
     )
})

testthat::test_that("d.dens does not work with invalid density function", {
     testthat::expect_error(
          d.count(
               densfun = "nuka-cola",
          ),
          regexp = "not a supported distribution"
     )
})

# Test if the function returns a ggplot object
testthat::test_that("sw_model(plot =TRUE) returns a ggplot object", {
        p <- sw_model("Hollstein_1980",
                        plot = TRUE)
        testthat::expect_true("ggproto" %in% class(p$layers[[1]]$stat))
})
