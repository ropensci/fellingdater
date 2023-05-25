#' cor_table: calculates correlation values between tree-ring series
#'
#' @param x Name of a `data.frame` (of class ´rwl´) with tree-ring data,
#'   where each column represents a measurement series, and calendar years
#'   are listed as rownames.
#' @param y Name of a `data.frame` (of class ´rwl´) with tree-ring data,
#'   where each column represents a measurement series, and years are listed
#'   as rownames.
#'   x is compared to this series (y = reference).
#'   If `NULL` (default) x is compared to itself (y = x).
#' @param min_overlap A `numeric` specifying the minimal overlap between series.
#' @param remove.duplicates Logical. If `TRUE` identical pairs of series and
#'   references are removed from the output.
#' @param output Should be one of `matrix` or `table` (default).
#' @param values A character vector listing all correlation values that
#'   should be computed. Defaults to all possible values:
#'   c("r_pearson", "t_St", "glk", "glk_p", "t_BP", "t_Ho")
#' @param sort_by Correlation value by which the output is sorted for each
#'   series in x.
#'
#' @return Depends on the value of `output`. If `matrix` a list of correlation
#'   matrices is returned. If  `table` a data.frame is returned with all
#'   correlation values listed by `values.`
#' @export
#' @description This function calculates common correlation values between dated
#'   tree-ring series (`x`) and a set of reference chronologies (`y`).
#'   When no master chronologies are provided, each series in x is compared to all
#'   other series in x.
#'
cor_table <-
     function(x,
              y = NULL,
              min_overlap = 50,
              remove.duplicates = TRUE,
              output = "table", #c("matrix", "table")
              values = c("r_pearson", "t_St", "glk", "glk_p", "t_BP", "t_Ho"),
              sort_by = "t_Ho") #c("glk", "t_St", "t_BP", "t_Ho", "r_pearson"))
     {
          # to avoid notes in CMD check
          pnorm <- cor <- select <- series <- reference <- NULL

          ### checks
          x_ori <- x

          if (is.null(y)) {
               y = x
               noRef = TRUE
          }
          else {
               noRef = FALSE
               y_ori <- y

          }

          if (!("rwl" %in% class(x))) {
               warning("'x' is not class rwl")
          }
          if (!("rwl" %in% class(y))) {
               warning("'x' is not class rwl")
          }

          if (!all(diff(as.numeric(row.names(x))) == 1)) {
               stop(
                    "The tree-ring series 'x' have/has no consecutive years in increasing order as rownames"
               )
          }
          if (!all(diff(as.numeric(row.names(y))) == 1)) {
               stop(
                    "The master series 'y' have/has no consecutive years in increasing order as rownames"
               )
          }
          if (any(
               length(min_overlap) != 1 |
               !is.numeric(min_overlap) |
               min_overlap %% 1 != 0 |
               min_overlap < 3
          )) {
               stop("'overlap' should be a single integer >=3")
          }
          if (min_overlap < 50) {
               warning(
                    "The minimum number of overlap (`min_overlap = `) is lower than 50.
                    This might lead to statistically insignificant matches."
               )
          }

          if (output == "table" & !(sort_by %in% values)) {
               warning(paste0(
                    "Results are sorted by ",
                    values[1],
                    " instead of ",
                    sort_by,
                    "."
               ))
               sort_by <- values[1]

          }

          # descriptives of the full data sets
          if (output == "table") {

               descr <- dplR::rwl.stats(x)[,1:4]
               colnames(descr) <- c("series", "first", "last", "length")

               descr_ref <- dplR::rwl.stats(y)[,1:3]
               colnames(descr_ref) <- c("reference", "ref_first", "ref_last")
          }

          ### reduce x and y to common overlap, based on rownames = years
          interval_x <- rownames(x)
          interval_y <- rownames(y)
          interval <- intersect(interval_x, interval_y)

          if (length(interval) < min_overlap) {
               stop("no common interval between series and master series")
          }

          # drop = FALSE ensures x/y remains a data.frame when only one series is present
          x <- x[rownames(x) %in% interval, , drop = FALSE]
          y <- y[rownames(y) %in% interval, , drop = FALSE]

          # remove empty columns
          x <- x[, sapply(x, function(col) sum(!is.na(col))) > 3, drop = FALSE]
          y <- y[, sapply(y, function(col) sum(!is.na(col))) > 3, drop = FALSE]


          ### overlap
          n <- dim(x)[2]
          m <- dim(y)[2]
          overlap <- matrix(NA_real_, nrow = n, ncol = m)
          rownames(overlap) <- names(x)
          colnames(overlap) <- names(y)

          for (i in 1:n) {
               # substract single column from each matrix column => NA when no overlap!
               OVL <- x[, i] - y
               OVL <- colSums(!is.na(OVL))
               OVL[OVL == 0] <- NA
               overlap[i, ] <- OVL
          }

          ### parallel variation (%PV | GLK)
          GLK_mat <- matrix(NA_real_, nrow = n, ncol = m)
          rownames(GLK_mat) <- names(x)
          colnames(GLK_mat) <- names(y)

          GLK_p <- matrix(NA_real_, nrow = n, ncol = m)
          rownames(GLK_mat) <- names(x)
          colnames(GLK_mat) <- names(y)

          if ("glk" %in%  values) {
               treering_sign_x <- apply(x, 2, diff)
               treering_sign_x <- sign(treering_sign_x)
               treering_sign_y <- apply(y, 2, diff)
               treering_sign_y <- sign(treering_sign_y)
               for (i in 1:n) {
                    treering_GC <-
                         abs(treering_sign_x[, i] - treering_sign_y)
                    GLK_values <-
                         1 - (colSums(treering_GC, na.rm = TRUE) / (2 * (overlap[i, ] - 1)))
                    GLK_mat[i,] <- GLK_values
               }

               if (noRef == TRUE) {
                    diag(GLK_mat) <- 1
               }
               ### probability associated with %PV | GLK
               s_df <- 1 / (2 * sqrt(overlap))
               z_df <- (GLK_mat - .5) / s_df
               z_normcdf <-
                    apply(z_df, 2, function(z) pnorm(z, mean = 0, sd = 1))
               GLK_p <- 2 * (1 - z_normcdf)
               # when dim(x) == 1 +> apply returns vector instead of matrix
               dim(GLK_p) <- c(dim(x)[2], dim(y)[2])
               rownames(GLK_p) <- colnames(x)
               colnames(GLK_p) <- colnames(y)

          }

          ### t-values according to the Hollstein 1980 algorithm
          tHo_mat <- matrix(NA_real_, nrow = n, ncol = m)
          rownames(tHo_mat) <- names(x)
          colnames(tHo_mat) <- names(y)

          if ("t_Ho" %in% values) {
               wuch_x <- apply(x, 2, function(x) {x / c(NA, x[-length(x)])} )
               wuch_x <- 100 * log10(wuch_x)

               wuch_y <- apply(y, 2, function(x) {x / c(NA, x[-length(x)])} )
               wuch_y <- 100 * log10(wuch_y)

               r <-
                    cor(x = wuch_x,
                        y = wuch_y,
                        use = "pairwise.complete.obs",
                        method = "pearson"
                       )

               # if r < 0, r is set to zero
               r[r < 0] <- 0

               tHo_mat <- # overlap - 1 to compensate for reduced overlap after lag(x)
                    round(r * sqrt((overlap - 1) - 2) / sqrt(1 - r ^ 2), 2)

          }

          ### t-values according to the Baillie & Pilcher 1973 algorithm

          tBP_mat <- matrix(NA_real_, nrow = n, ncol = m)
          rownames(tBP_mat) <- names(x)
          colnames(tBP_mat) <- names(y)

          if ("t_BP" %in% values) {
               movav5_x <- apply(x, 2, function(x) {
                    movAv(x, w = 5)
               })
               rownames(movav5_x) <- rownames(x)
               movav5_x <- 100 * x / movav5_x
               movav5_x <- log(movav5_x)
               if (noRef == FALSE) {
                    movav5_y <- apply(y, 2, function(x) {
                         movAv(x, w = 5)
                    })
                    rownames(movav5_y) <- rownames(y)
                    movav5_y <- 100 * y / movav5_y
                    movav5_y <- log(movav5_y)
                    # Pearson r between pairs of series for common overlap
                    r <-
                         cor(x = movav5_x,
                             y = movav5_y,
                             use = "pairwise.complete.obs",
                             method = "pearson"
                             )
               } else {
                    r <-
                         cor(x = movav5_x,
                             y = movav5_x,
                             use = "pairwise.complete.obs",
                             method = "pearson"
                             )

               }
               # if r <0, r is set to zero (Baillie & Pilcher 1973)
               r[r < 0] <- 0

               tBP_mat <- # overlap - 4 to compensate for reduced overlap after movAv near both edges
                    round(r * sqrt((overlap - 4) - 2) / sqrt(1 - r ^ 2), 2)

          }

          ### r-Pearson
          r_pearson <- matrix(NA_real_, nrow = n, ncol = m)

          if ("r_pearson" %in% values | "t_St" %in% values) {
               r_pearson <-
                    cor( x = x,
                         y = y,
                         use = "pairwise.complete.obs",
                         method = "pearson"
                         )
               overlap_r <- is.na(overlap)
               overlap_r <- ifelse(overlap_r == TRUE, NA, 1)
               r_pearson <- r_pearson * overlap_r

          }

          ### t-value (Student's t)
          t_St <- matrix(NA_real_, nrow = n, ncol = m)

          if ("t_St" %in% values) {
               t_St <- (r_pearson * sqrt(overlap - 2)) / sqrt(1 - r_pearson ^ 2)

          }

          ### output
          corr_table <-
               list(
                    overlap = overlap,
                    glk = round(100 * GLK_mat, 1),
                    glk_p = GLK_p,
                    r_pearson = r_pearson,
                    t_St = t_St,
                    t_Ho = tHo_mat,
                    t_BP = tBP_mat
               )

          if (output == "table") {

               for (i in names(corr_table)){

                    if (i == names(corr_table)[1]){
                         corr_table_i <- corr_table[[i]]
                         corr_table_i <- as.data.frame(
                              as.table(corr_table_i),
                              stringsAsFactors = FALSE
                         )
                         names(corr_table_i) <- c("series", "reference", i)
                         corr_df <- corr_table_i

                    } else {
                         corr_table_i <- corr_table[[i]]
                         corr_table_i <- as.data.frame(
                              as.table(corr_table_i),
                              stringsAsFactors = FALSE
                         )
                         names(corr_table_i) <- c("series", "reference", i)

                         corr_df <- merge(corr_df, corr_table_i, by = c("series", "reference"))
                    }
               }

               # add descriptives of tree-ring series and references
               corr_table <- merge(corr_df, descr)
               corr_table <- merge(corr_table, descr_ref)

               col_order <- c(
                    "series",
                    "length",
                    "first",
                    "last",
                    "reference",
                    "ref_first",
                    "ref_last",
                    "overlap",
                    "glk",
                    "glk_p",
                    "r_pearson",
                    "t_St",
                    "t_BP",
                    "t_Ho"
               )
               corr_table <- corr_table[, col_order]

               if (length(setdiff(
                    c(
                         "glk",
                         "glk_p",
                         "t_BP",
                         "t_Ho",
                         "r_pearson",
                         "t_St"
                    ),
                    values
               )) != 0) {
                    cols_to_remove <-
                         setdiff(c(
                              "glk",
                              "glk_p",
                              "t_BP",
                              "t_Ho",
                              "r_pearson",
                              "t_St"
                         ),
                         values)

                    corr_table <-
                         select(corr_table, vars = -cols_to_remove)
               }

               ### remove duplicates (not possible when y != NULL)
               if (remove.duplicates == TRUE && noRef == TRUE) {
                    corr_table <- subset(corr_table, series < reference)
               }

               ### sort by series, then by tHo (or sort_by argument)
               corr_table <- corr_table[order(corr_table[["series"]],
                                              -corr_table[[sort_by]]), ]

          }

          return(corr_table)

     }
