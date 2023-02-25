# test data for sw_combine() and sw_combine_plot()

## a dataset in which all series have preserved sapwood
dummy0 <- data.frame(
     trs = c("trs_1", "trs_2", "trs_3", "trs_4", "trs_5"),
     end = c(1000, 1009, 1007, 1005, 1010),
     swr = c(5, 10, 15, 16, 8),
     bark = c(FALSE, FALSE, FALSE, FALSE, FALSE)
)

dummy1 <- data.frame(
     series = c("trs_1", "trs_2", "trs_3", "trs_4", "trs_5"),
     last = c(1000, 1009, 1007, 1005, 1010),
     n_sapwood = c(5, 10, 15, 16, 8),
     waneyedge = c(FALSE, FALSE, FALSE, FALSE, FALSE)
)

## a dataset in which one series has an exact felling date (= waney edge preserved)
dummy2 <- data.frame(
     series = c("trs_1", "trs_2", "trs_3", "trs_4", "trs_5"),
     last = c(1000, 1005, 1008, 1000, 1010),
     n_sapwood = c(5, 10, NA, 1, 3),
     waneyedge = c(FALSE, FALSE, FALSE, FALSE, TRUE))

## a dataset with multiple exact felling dates
dummy3 <- data.frame(
     series = c("trs_1", "trs_2", "trs_3", "trs_4", "trs_5"),
     last = c(1000, 1005, 1008, 1000, 1010),
     n_sapwood = c(5, 10, NA, 1, NA),
     waneyedge = c(TRUE, TRUE, TRUE, TRUE, TRUE))

## a combination of series with and without sapwood rings
dummy4 <- data.frame(
     series = c("trs_1", "trs_2", "trs_3", "trs_4", "trs_5"),
     last = c(1000, 1005, 1005, 1020, 1040),
     n_sapwood = c(5, 10, NA, 1, 0),
     waneyedge = c(FALSE, FALSE, FALSE, FALSE, FALSE))

## this dataset contains series without preserved sapwood
dummy5 <- data.frame(
     series = c("trs_1", "trs_2", "trs_3", "trs_4"),
     last = c(1000, 1005, 1000, 1000),
     n_sapwood = as.numeric(c(NA, NA, NA, NA)),
     waneyedge = c(FALSE, FALSE, FALSE, FALSE))

# test data for for sw_sum()

dummy6 <- data.frame(
     series = c("trs_1", "trs_2", "trs_3", "trs_4", "trs_5", "trs_6", "trs_7", "trs_8", "trs_9"),
     last = c(1000, 1009, 1007, 1005, 1010, 1020, 1025, 1050, 1035),
     swr = c(5, 10, 15, 16, 8, 0, 10, 3, 1),
     waneyedge = c(FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE)
)

dummy7 <- data.frame(
     series = c("trs_1", "trs_2", "trs_3", "trs_4", "trs_5", "trs_6", "trs_7", "trs_8", "trs_9"),
     last = c(1000, 1009, 1007, 1007, 1010, 1020, 1025, 1050, 1035),
     swr = c(5, 10, 15, 16, 8, 0, 10, 3, 1),
     waneyedge = c(FALSE, FALSE, TRUE, TRUE, FALSE, FALSE, FALSE, TRUE, FALSE)
)

usethis::use_data(dummy0,
                  dummy1,
                  dummy2,
                  dummy3,
                  dummy4,
                  dummy5,
                  dummy6,
                  dummy7,
                  internal = TRUE, overwrite = TRUE)
