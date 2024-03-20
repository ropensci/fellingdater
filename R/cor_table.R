#' Calculate correlation values between tree-ring series
#'
#' This function computes various correlation values between tree-ring series in
#' a `data.frame` `x` and a set of reference chronologies in a `data.frame` `y`.
#' If `y` is not provided, it compares each series in `x` to all other series in
#' `x.
#'
#' @param x A `data.frame` of class ´rwl' with tree-ring data. Each column
#'   represents a measurement series, and row names correspond to (calendar)
#'   years.
#' @param y A `data.frame` of class 'rwl' with tree-ring data. Each column
#'   represents a measurement series or chronology, and row names correspond to
#'   (calendar) years. If NULL (default), `x` is compared to itself (y = x).
#' @param min_overlap A `numeric` value specifying the minimum overlap required
#'   between series for correlation calculation.
#' @param remove_duplicates A logical value. If `TRUE`, identical pairs of
#'   series and references are removed from the output.
#' @param output The desired output format, either "matrix" or "table"
#'   (default).
#' @param sort_by The correlation value by which the output is sorted for each
#'   series in `x`. One of "r_pearson", "t_St", "glk", "glk_p", "t_BP", "t_Ho".
#'   Default to "t_Ho"
#'
#' @return The function returns a list of correlation matrices if `output` is
#'   set to "matrix." If `output` is set to "table," it returns a `data.frame`
#'   reporting all correlation values.
#' @export
#' @references
#'   - Baillie, M.G.L., Pilcher, J.R. (1973) A simple crossdating program for
#' tree-ring research. _Tree-Ring Bulletin_ **33**, 7–14.
#' <http://hdl.handle.net/10150/260029>
#'
#'   - Buras, A. and Wilmking, M. (2015) Correcting the calculation of
#' Gleichläufigkeit, _Dendrochronologia_ **34**, 29-30.
#' <https://doi.org/10.1016/j.dendro.2015.03.003>
#'
#'   - Eckstein, D. and Bauch, J. (1969) Beitrag zur Rationalisierung eines
#' dendrochronologischen Verfahrens und zur Analyse seiner Aussagesicherheit.
#' _Forstwissenschaftliches Centralblatt_, **88**(1), 230-250.
#'
#'   - Huber, B. (1943) Über die Sicherheit jahrringchronologischer Datierung.
#' _Holz als Roh- und Werkstoff_ **6**, 263-268.
#' <https://doi.org/10.1007/BF02603303>
#'
#'   - Hollstein E. (1980) Mitteleuropäische Eichenchronologie. Trierer
#' dendrochronologische Forschungen zur Archäologie und Kunstgeschichte, Trierer
#' Grabungen und Forschungen **11**, Mainz am Rhein.
#'
#'   - Jansma, E. (1995) RemembeRINGs; The development and application of local
#' and regional tree-ring chronologies of oak for the purposes of archaeological
#' and historical research in the Netherlands, Nederlandse Archeologische
#' Rapporten **19**, Rijksdienst voor het Oudheidkundig Bodemonderzoek,
#' Amersfoort. <https://dspace.library.uu.nl/handle/1874/45149>
#'
#'   - Schweingruber, F. H. (1988) Tree rings: basics and applications of
#' dendrochronology, Kluwer Academic Publishers, Dordrecht, Netherlands, 276 p.
#'
#'   - Visser, R.M. (2020) On the similarity of tree-ring patterns: Assessing
#' the influence of semi-synchronous growth changes on the Gleichläufigkeit for
#' big tree-ring data sets, _Archaeometry_ **63**, 204-215.
#' <https://doi.org/10.1111/arcm.12600>
#'
#' @description This function calculates common correlation values between dated
#'   tree-ring series (`x`) and a set of reference chronologies (`y`). When no
#'   master chronologies are provided, each series in x is compared to all other
#'   series in x.
#'
#'   Only values are reported for pairs of series with a common overlap >=
#'   min_overlap.
#'
#'   The correlation values computed include:
#'   - glk: 'Gleichläufigkeit' or 'percentage of parallel variation'
#'   (Buras & Wilmking 2015; Eckstein & Bauch 1969; Huber 1942; Visser 2020)
#'   - glk_p: significance level associated with the glk-value (Jansma 1995)
#'   - r_pearson: the Pearson's correlation coefficient
#'   - t_St: Student's t-value
#'   - t_BP: t-values according to the Baillie & Pilcher (1973) algorithm
#'   - t_Ho: t-values according to the Hollstein (1980) algorithm
#' @examples
#' # example code
#'
#' Doel1 <- system.file("extdata", "DOEL1.fh", package = "fellingdateR")
#' Doel1_trs <- read_fh(Doel1, verbose = FALSE)
#' # crossdating ring-width series from Doel 1 against each other:
#'
#' cor_results <- cor_table(Doel1_trs, output = "table", min_overlap = 80,
#' sort_by = "t_Ho", remove_duplicates = TRUE)
#' head(cor_results,20)
#'
cor_table <-
  function(x,
           y = NULL,
           min_overlap = 50,
           remove_duplicates = TRUE,
           output = "table",
           sort_by = "t_Ho")
  {
    # to avoid notes in CMD check
    pnorm <- cor <- select <- series <- reference <- NULL

    ### checks
    x_ori <- x

    if (is.null(y)) {
      y <- x
      no_ref <- TRUE
    } else {
      no_ref <- FALSE
      y_ori <- y
    }

    if (!inherits(x, "rwl")) {
      warning("'x' is not class rwl")
    }
    if (!inherits(y, "rwl")) {
      warning("'y' is not class rwl")
    }

    increasing_consecutive_years <- all(diff(as.numeric(row.names(x))) == 1)
    if (!increasing_consecutive_years) {
      stop(
        "rownames 'x' are not consecutive years in increasing order."
      )
    }

    increasing_consecutive_years <- all(diff(as.numeric(row.names(y))) == 1)
    if (!increasing_consecutive_years) {
      stop(
        "rownames 'y' are not consecutive years in increasing order."
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
          This might lead to statistically insignificant correlation values."
      )
    }

    # descriptives of the full data sets
    if (output == "table") {
      descr <- dplR::rwl.stats(x)[, 1:4]
      colnames(descr) <-
        c("series", "first", "last", "length")

      descr_ref <- dplR::rwl.stats(y)[, 1:3]
      colnames(descr_ref) <-
        c("reference", "ref_first", "ref_last")
    }

    ### reduce x and y to common overlap
    interval_x <- rownames(x)
    interval_y <- rownames(y)
    interval <- intersect(interval_x, interval_y)

    if (length(interval) < min_overlap) {
      stop("no common interval between series and master series")
    }

    x <- x[rownames(x) %in% interval, , drop = FALSE]
    y <- y[rownames(y) %in% interval, , drop = FALSE]

    # remove empty columns
    x <-  x[, apply(!is.na(x), 2, sum) > 3, drop = FALSE]
    y <-  y[, apply(!is.na(y), 2, sum) > 3, drop = FALSE]

    ### overlap
    n <- dim(x)[2]
    m <- dim(y)[2]
    overlap <- matrix(NA_real_, nrow = n, ncol = m)
    rownames(overlap) <- names(x)
    colnames(overlap) <- names(y)

    for (i in 1:n) {
      # substract single column from each matrix column => NA when no overlap!
      ovl <- x[, i] - y
      ovl <- colSums(!is.na(ovl))
      ovl[ovl == 0] <- NA
      overlap[i, ] <- ovl
    }
    overlap_min <- overlap
    overlap_min[overlap_min < min_overlap] <- NA
    overlap_min[overlap_min >= min_overlap] <- 1

    ### parallel variation (%PV | GLK)
    glk_mat <- matrix(NA_real_, nrow = n, ncol = m)
    rownames(glk_mat) <- names(x)
    colnames(glk_mat) <- names(y)

    glk_p <- matrix(NA_real_, nrow = n, ncol = m)
    rownames(glk_mat) <- names(x)
    colnames(glk_mat) <- names(y)

    treering_sign_x <- apply(x, 2, diff)
    treering_sign_x <- sign(treering_sign_x)
    treering_sign_y <- apply(y, 2, diff)
    treering_sign_y <- sign(treering_sign_y)
    for (i in 1:n) {
      treering_gc <-
        abs(treering_sign_x[, i] - treering_sign_y)
      glk_values <-
        1 - (colSums(treering_gc, na.rm = TRUE) / (2 * (overlap[i, ] - 1)))
      glk_mat[i, ] <- glk_values
    }

    if (no_ref == TRUE) {
      diag(glk_mat) <- 1
    }
    ### probability associated with %PV | GLK
    s_df <- 1 / (2 * sqrt(overlap))
    z_df <- (glk_mat - .5) / s_df
    z_normcdf <-
      apply(z_df, 2, function(z) {
        pnorm(z, mean = 0, sd = 1)
      })
    glk_p <- 2 * (1 - z_normcdf)
    # when dim(x) == 1 +> apply returns a vector instead of a matrix
    dim(glk_p) <- c(dim(x)[2], dim(y)[2])
    rownames(glk_p) <- colnames(x)
    colnames(glk_p) <- colnames(y)

    ### t-values according to the Hollstein 1980 algorithm
    tho_mat <- matrix(NA_real_, nrow = n, ncol = m)
    rownames(tho_mat) <- names(x)
    colnames(tho_mat) <- names(y)

    wuch_x <- apply(x, 2, function(x) {
      x / c(NA, x[-length(x)])
    })
    wuch_x <- 100 * log10(wuch_x)

    wuch_y <-
      apply(y, 2, function(x) {
        x / c(NA, x[-length(x)])
      })
    wuch_y <- 100 * log10(wuch_y)

    r <-
      cor(
        x = wuch_x,
        y = wuch_y,
        use = "pairwise.complete.obs",
        method = "pearson"
      )

    # if r < 0, r is set to zero
    r[r < 0] <- 0

    tho_mat <-
      # overlap - 1 to compensate for reduced overlap after lag(x)
      round(r * sqrt((overlap - 1) - 2) / sqrt(1 - r^2), 2)


    ### t-values according to the Baillie & Pilcher 1973 algorithm
    tbp_mat <- matrix(NA_real_, nrow = n, ncol = m)
    rownames(tbp_mat) <- names(x)
    colnames(tbp_mat) <- names(y)

    movav5_x <- apply(x, 2, function(x) {
      mov_av(x, w = 5)
    })
    rownames(movav5_x) <- rownames(x)
    movav5_x <- 100 * x / movav5_x
    movav5_x <- log(movav5_x)
    if (no_ref == FALSE) {
      movav5_y <- apply(y, 2, function(x) {
        mov_av(x, w = 5)
      })
      rownames(movav5_y) <- rownames(y)
      movav5_y <- 100 * y / movav5_y
      movav5_y <- log(movav5_y)
      # Pearson r between pairs of series for common overlap
      r <-
        cor(
          x = movav5_x,
          y = movav5_y,
          use = "pairwise.complete.obs",
          method = "pearson"
        )
    } else {
      r <-
        cor(
          x = movav5_x,
          y = movav5_x,
          use = "pairwise.complete.obs",
          method = "pearson"
        )
    }
    # if r < 0, r is set to zero (Baillie & Pilcher 1973)
    r[r < 0] <- 0

    tbp_mat <-
      # overlap - 4 to compensate for reduced overlap after mov_av
      round(r * sqrt((overlap - 4) - 2) / sqrt(1 - r^2), 2)


    ### r-Pearson
    r_pearson <- matrix(NA_real_, nrow = n, ncol = m)

    r_pearson <-
      cor(
        x = x,
        y = y,
        use = "pairwise.complete.obs",
        method = "pearson"
      )
    overlap_r <- is.na(overlap)
    overlap_r <- ifelse(overlap_r == TRUE, NA, 1)
    r_pearson <- r_pearson * overlap_r


    ### t-value (Student's t)
    t_St <- matrix(NA_real_, nrow = n, ncol = m)

    t_St <- (r_pearson * sqrt(overlap - 2)) / sqrt(1 - r_pearson^2)


    ### output
    corr_table <-
      list(
        overlap = overlap,
        glk = round(100 * glk_mat, 1) * overlap_min,
        glk_p = glk_p * overlap_min,
        r_pearson = r_pearson * overlap_min,
        t_St = t_St * overlap_min,
        t_Ho = tho_mat * overlap_min,
        t_BP = tbp_mat * overlap_min
      )

    if (output == "table") {
      for (i in names(corr_table)) {
        if (i == names(corr_table)[1]) {
          corr_table_i <- corr_table[[i]]
          corr_table_i <-
            as.data.frame(as.table(corr_table_i),
              stringsAsFactors = FALSE
            )
          names(corr_table_i) <-
            c("series", "reference", i)
          corr_df <- corr_table_i
        } else {
          corr_table_i <- corr_table[[i]]
          corr_table_i <-
            as.data.frame(as.table(corr_table_i),
              stringsAsFactors = FALSE
            )
          names(corr_table_i) <-
            c("series", "reference", i)

          corr_df <-
            merge(corr_df,
              corr_table_i,
              by = c("series", "reference")
            )
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


      ### remove duplicates (not possible when y != NULL)
      if (remove_duplicates == TRUE && no_ref == TRUE) {
        corr_table <- subset(corr_table, series < reference)
      }

      ### sort by series, then by tHo (or sort_by argument)
      corr_table <-
        corr_table[order(
          corr_table[["series"]],
          -corr_table[[sort_by]]
        ), ]

      corr_table <-
        subset(corr_table, overlap >= min_overlap)
    }
    return(corr_table)
  }
