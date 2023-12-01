# test data for sw_combine() and sw_combine_plot()

## a dataset in which all series have preserved sapwood
trs_example0 <- data.frame(
        trs = c("trs_01", "trs_02", "trs_03", "trs_04", "trs_05"),
        end = c(1000, 1009, 1007, 1005, 1010),
        swr = c(5, 10, 15, 16, 8),
        bark = c(FALSE, FALSE, FALSE, FALSE, FALSE)
)

## a dataset in which all series have preserved sapwood
trs_example1 <- data.frame(
     series = c("trs_01", "trs_02", "trs_03", "trs_04", "trs_05"),
     last = c(1000, 1009, 1007, 1005, 1010),
     n_sapwood = c(5, 10, 15, 16, 8),
     waneyedge = c(FALSE, FALSE, FALSE, FALSE, FALSE)
)

## a dataset in which one series has an exact felling date (= waney edge preserved)
trs_example2 <- data.frame(
     series = c("trs_06", "trs_07", "trs_08", "trs_09", "trs_10"),
     last = c(1000, 1005, 1008, 1000, 1010),
     n_sapwood = c(5, 10, NA, 1, 15),
     waneyedge = c(FALSE, FALSE, FALSE, FALSE, TRUE))

## a dataset with multiple exact felling dates
trs_example3 <- data.frame(
     series = c("trs_11", "trs_12", "trs_13", "trs_14", "trs_15"),
     last = c(1000, 1005, 1008, 1000, 1010),
     n_sapwood = c(5, 10, NA, 1, NA),
     waneyedge = c(TRUE, TRUE, TRUE, TRUE, TRUE))

## a combination of series with and without sapwood rings
trs_example4 <- data.frame(
     series = c("trs_16", "trs_17", "trs_18", "trs_19", "trs_20"),
     last = c(1000, 1005, 1005, 1020, 1040),
     n_sapwood = c(5, 10, NA, 1, 0),
     waneyedge = c(FALSE, FALSE, FALSE, FALSE, FALSE))

## this dataset contains series without preserved sapwood
trs_example5 <- data.frame(
     series = c("trs_21", "trs_22", "trs_23", "trs_24"),
     last = c(1000, 1005, 1000, 1000),
     n_sapwood = as.numeric(c(NA, NA, NA, NA)),
     waneyedge = c(FALSE, FALSE, FALSE, FALSE))

# test data for for sw_sum()

trs_example6 <- data.frame(
     series = c("trs_25", "trs_26", "trs_27", "trs_28", "trs_29", "trs_30", "trs_31", "trs_32", "trs_33"),
     last = c(1000, 1009, 1007, 1005, 1010, 1020, 1025, 1050, 1035),
     n_sapwood = c(5, 10, 15, 16, 8, 0, 10, 3, 1),
     waneyedge = c(FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE)
)

trs_example7 <- data.frame(
     series = c("trs_34", "trs_35", "trs_36", "trs_37", "trs_38", "trs_39", "trs_40", "trs_41", "trs_42"),
     last = c(1000, 1009, 1007, 1007, 1010, 1020, 1025, 1050, 1035),
     n_sapwood = c(5, 10, 15, 16, 8, 0, 10, NA, 1),
     waneyedge = c(FALSE, FALSE, TRUE, TRUE, FALSE, FALSE, FALSE, TRUE, FALSE)
)

usethis::use_data(trs_example0,
                  trs_example1,
                  trs_example2,
                  trs_example3,
                  trs_example4,
                  trs_example5,
                  trs_example6,
                  trs_example7,
                  internal = FALSE, overwrite = TRUE)
