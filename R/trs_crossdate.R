#' Comprehensive sliding window crossdating analysis using multiple methods
#'
#' This function performs crossdating analysis using sliding window technique with
#' multiple statistical methods: Parallel Variation (GLK/SGC), Baillie & Pilcher
#' t-values, Hollstein t-values, and standard Pearson correlation t-values. It tests series
#' at different temporal lags to find a potential date positions.
#'
#' @param x A data frame of test tree-ring series in `rwl` format (years as rownames,
#'   series as columns). All columns must be numeric.
#' @param y A data frame of reference tree-ring series in `rwl` format. If `NULL`,
#'   uses `x` for self-comparison. Default is `NULL`.
#' @param min_overlap Integer. Minimum number of overlapping years required between
#'   series pairs to compute statistics. Must be >= 3. Default is 30.
#' @param sliding Logical. If `TRUE`, performs sliding window analysis testing
#'   multiple temporal lags. If `FALSE`, tests only at lag 0 (contemporary positioning).
#'   Default is `TRUE`.
#' @param top_n Integer or NULL. If specified, returns only the top n matches per
#'   test series (after optional sorting by `rank_by`). Default is `NULL`.
#' @param rank_by Character or NULL. Statistic to rank by when sorting results per
#'   test series. If `NULL`, no additional sorting is applied beyond default ordering.
#'   Options are "r_pearson", "t_St", "t_BP", "t_Ho", "sgc", "ssgc", "glk".
#'   Default is `NULL`.
#' @param pb Logical. If `TRUE` (default), displays a progress bar during computation.
#'   Set to `FALSE` to suppress progress bar output, useful when calling the function
#'   programmatically or in non-interactive sessions.
#'
#' @return A data frame with crossdating results containing the following columns:
#' \describe{
#'   \item{series}{Name of the test series from x}
#'   \item{length}{Length of the test series}
#'   \item{first}{Date of the first ring at the particular lag position}
#'   \item{last}{Date of the last ring at the particular lag position}
#'   \item{reference}{Name of the reference series}
#'   \item{ref_first}{Date of the first ring in the reference series}
#'   \item{ref_last}{Date of the last ring in the reference series}
#'   \item{overlap}{Number of overlapping years between series and reference}
#'   \item{r_pearson}{Pearson correlation coefficient}
#'   \item{t_St}{Student's t-statistic}
#'   \item{t_BP}{Baillie & Pilcher t-statistic}
#'   \item{t_Ho}{Hollstein t-statistic}
#'   \item{sgc}{Synchronous Growth Change (SGC) statistic}
#'   \item{ssgc}{Semi-SGC statistic}
#'   \item{sgc_p}{SGC p-value}
#'   \item{glk}{Gleichläufigkeit (GLK) statistic}
#'   \item{glk_p}{GLK p-value}
#' }
#'
#' @details
#' This function implements sliding window crossdating, a fundamental technique in
#' dendrochronology for dating wood samples of unknown age. The process involves:
#'
#' \enumerate{
#'   \item Determining the range of possible lags based on series overlap
#'   \item For each lag, shifting the test series temporally
#'   \item Computing all crossdating statistics
#'   \item Returning results for each tested position
#' }
#'
#' The sliding window approach allows identification of the temporal position where
#' test and reference series show maximum similarity, which indicates the most
#' likely dating for the test series.
#'
#' **Statistical Methods:**
#' \itemize{
#'   \item t_BP: t-values according to the Baillie & Pilcher (1973) algorithm
#'   \item t_Ho: t-values according to the Hollstein (1980) algorithm
#'   \item sgc, sgc_p: synchronous growth change and associated significance level (Visser 2020)
#'   \item ssgc: semi-synchronous growth change (Visser 2020)
#'   \item glk, glk_p: 'Gleichläufigkeit' or 'percentage of parallel variation'
#'   (Buras & Wilmking 2015; Eckstein & Bauch 1969; Huber 1942), and
#'   significance level (p-value) associated with the glk-value (Jansma 1995)
#'   \item r_pearson: the Pearson's correlation coefficient
#'   \item t_St: Student's t-value
#' }
#'
#' **Sorting and Filtering:**
#' \itemize{
#'   \item If `rank_by = NULL`: Results are returned with default ordering (series, reference, first)
#'   \item If `rank_by` is specified: Results are sorted per test series by the chosen statistic (high to low)
#'   \item If `top_n` is specified: Only the top n matches per test series are returned (after optional sorting)
#' }
#'
#' @examples
#' \dontrun{
#' # Create sample data
#' rwl_test <- trs_pseudo_rwl(n_series = 2, series_length = 80, end_date = 1950)
#' rwl_ref <- trs_pseudo_rwl(n_series = 3, series_length = 150, end_date = 2000)
#'
#' # Full sliding window analysis with default ordering
#' result <- trs_crossdate(rwl_test, rwl_ref, min_overlap = 30)
#'
#' # Sort by Hollstein t-values
#' result_sorted <- trs_crossdate(rwl_test, rwl_ref, min_overlap = 30, rank_by = "t_Ho")
#'
#' # Contemporary positioning only
#' contemp <- trs_crossdate(rwl_test, rwl_ref, sliding = FALSE)
#'
#' # Self-comparison for quality control
#' self_check <- trs_crossdate(rwl_ref, sliding = FALSE)
#'
#' # Get top 3 matches per series ranked by Hollstein t-values
#' top_matches <- trs_crossdate(rwl_test, rwl_ref, top_n = 3, rank_by = "t_Ho")
#'
#' # Get top 5 matches with default ordering (no sorting by statistic)
#' top_default <- trs_crossdate(rwl_test, rwl_ref, top_n = 5)
#' }
#'
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
#' @seealso \code{\link{trs_pv}}, \code{\link{trs_tbp}}, \code{\link{trs_tho}}
#'
#' @importFrom dplR rwl.stats
#' @importFrom utils txtProgressBar setTxtProgressBar head
#'
#' @export
trs_crossdate <- function(x, y = NULL, min_overlap = 30, sliding = TRUE,
                          top_n = NULL, rank_by = NULL, pb = TRUE) {
  # Input validation
  stopifnot(is.data.frame(x))
  if (is.null(y)) y <- x
  stopifnot(is.data.frame(y))
  if (nrow(x) == 0 || ncol(x) == 0) stop("'x' cannot be empty")
  if (!all(sapply(x, is.numeric))) stop("All columns in 'x' must be numeric")

  x <- trs_trim(x)
  y <- trs_trim(y)

  # Ensure row names are interpretable as years and in consecutive order
  check_consecutive(x)
  check_consecutive(y)

  if (min_overlap < 3 || min_overlap %% 1 != 0) {
    stop("'min_overlap' should be a single integer >= 3")
  }

  if (!is.logical(sliding)) {
    stop("'sliding' must be a logical value (TRUE/FALSE)")
  }

  if (!is.null(top_n) && (top_n < 1 || top_n != as.integer(top_n))) {
    stop("'top_n' must be a positive integer")
  }
  # Validate rank_by parameter
  valid_rank_by <- c("r_pearson", "t_St", "t_BP", "t_Ho", "sgc", "ssgc", "glk")
  if (!is.null(rank_by) && !rank_by %in% valid_rank_by) {
    stop("'rank_by' must be NULL or one of: ", paste(valid_rank_by, collapse = ", "))
  }

  x_years <- as.integer(rownames(x))
  y_years <- as.integer(rownames(y))

  # Get descriptives for reference
  y_stats <- dplR::rwl.stats(y)[, 1:3]

  # Pre-transform full series for efficiency
  transformed_data <- list()
  transformed_data$tbp_x <- trs_tbp_transform(x)
  transformed_data$tbp_y <- trs_tbp_transform(y)
  transformed_data$tho_x <- trs_tho_transform(x)
  transformed_data$tho_y <- trs_tho_transform(y)

  # Determine lags to test
  if (sliding) {
    y_range <- range(y_years)
    # Define all lags where x can be positioned with min_overlap over y
    min_lag <- y_range[1] - max(x_years) + min_overlap - 1
    max_lag <- y_range[2] - min(x_years) - min_overlap + 1
    lags_to_use <- min_lag:max_lag
  } else {
    lags_to_use <- 0
  }

  # Pre-calculate series statistics once (outside the loop)
  x_series_stats <- lapply(colnames(x), function(series_name) {
    series_years <- x_years[!is.na(x[, series_name])]
    list(
      name = series_name,
      length = length(series_years),
      first_year = min(series_years),
      last_year = max(series_years)
    )
  })
  names(x_series_stats) <- colnames(x)

  # Pre-allocate vectors for all result columns
  # Estimate maximum possible results for pre-allocation
  n_series <- ncol(x)
  n_refs <- ncol(y)
  max_combinations_per_lag <- n_series * n_refs
  estimated_total <- length(lags_to_use) * max_combinations_per_lag

  # Pre-allocate vectors with estimated size
  series_vec <- character(estimated_total)
  length_vec <- integer(estimated_total)
  first_vec <- integer(estimated_total)
  last_vec <- integer(estimated_total)
  reference_vec <- character(estimated_total)
  ref_first_vec <- integer(estimated_total)
  ref_last_vec <- integer(estimated_total)
  overlap_vec <- integer(estimated_total)
  r_pearson_vec <- numeric(estimated_total)
  t_St_vec <- numeric(estimated_total)
  t_BP_vec <- numeric(estimated_total)
  t_Ho_vec <- numeric(estimated_total)
  sgc_vec <- numeric(estimated_total)
  ssgc_vec <- numeric(estimated_total)
  sgc_p_vec <- numeric(estimated_total)
  glk_vec <- numeric(estimated_total)
  glk_p_vec <- numeric(estimated_total)

  # Counter for actual results
  result_count <- 0

  if (pb) {
    # Initiate progress bar
    pbar <- utils::txtProgressBar(
      min = 0,
      max = length(lags_to_use),
      style = 3,
      char = "="
    )
    progress <- 0
    on.exit(close(pbar), add = TRUE)
  }

  # Process each lag
  for (lag in lags_to_use) {
    # Shift x years
    x_shifted_years <- x_years + lag
    common_years <- intersect(x_shifted_years, y_years)

    if (length(common_years) < min_overlap) {
      # Update progress bar even if no processing
      progress <- progress + 1
      utils::setTxtProgressBar(pb, progress)
      next
    }

    # Get indices for subsetting
    x_idx <- match(common_years - lag, rownames(x))
    y_idx <- match(common_years, rownames(y))

    # Subset original data
    x_sub <- x[x_idx, , drop = FALSE]
    y_sub <- y[y_idx, , drop = FALSE]
    rownames(x_sub) <- as.character(common_years)
    rownames(y_sub) <- as.character(common_years)

    # Compute Parallel Variation (PV) statistics
    pv_result <- suppressWarnings(
      trs_pv(x_sub, y_sub,
        min_overlap = min_overlap,
        prob = TRUE, as_df = FALSE
      )
    )

    # Compute Baillie & Pilcher t-values
    # Subset pre-transformed data
    tbp_x_sub <- transformed_data$tbp_x[x_idx, , drop = FALSE]
    tbp_y_sub <- transformed_data$tbp_y[y_idx, , drop = FALSE]
    rownames(tbp_x_sub) <- as.character(common_years)
    rownames(tbp_y_sub) <- as.character(common_years)

    tbp_result <- trs_tbp(tbp_x_sub, tbp_y_sub,
      min_overlap = min_overlap,
      as_df = FALSE, transform = FALSE
    )

    # Compute Hollstein t-values
    # Subset pre-transformed data
    tho_x_sub <- transformed_data$tho_x[x_idx, , drop = FALSE]
    tho_y_sub <- transformed_data$tho_y[y_idx, , drop = FALSE]
    rownames(tho_x_sub) <- as.character(common_years)
    rownames(tho_y_sub) <- as.character(common_years)

    tho_result <- trs_tho(tho_x_sub, tho_y_sub,
      min_overlap = min_overlap,
      as_df = FALSE, transform = FALSE
    )

    # Compute Pearson correlation and associated t-values
    cor_result <- trs_tSt(x_sub, y_sub, min_overlap = min_overlap, as_df = FALSE)

    # Find valid combinations (vectorized approach)
    valid_combinations <- which(!is.na(pv_result$overlap) & pv_result$overlap >= min_overlap, arr.ind = TRUE)

    if (nrow(valid_combinations) > 0) {
      for (k in seq_len(nrow(valid_combinations))) {
        i <- valid_combinations[k, 1]
        j <- valid_combinations[k, 2]

        series_name <- colnames(x_sub)[i]
        ref_name <- colnames(y_sub)[j]

        # Use pre-calculated stats
        series_stats <- x_series_stats[[series_name]]
        series_first <- series_stats$first_year + lag
        series_last <- series_stats$last_year + lag

        # Get reference positions
        ref_first <- y_stats[y_stats$series == ref_name, "first"]
        ref_last <- y_stats[y_stats$series == ref_name, "last"]

        # Increment counter and store values
        result_count <- result_count + 1

        # Grow vectors if needed (should be rare with good estimation)
        if (result_count > length(series_vec)) {
          new_size <- length(series_vec) * 2
          length(series_vec) <- new_size
          length(length_vec) <- new_size
          length(first_vec) <- new_size
          length(last_vec) <- new_size
          length(reference_vec) <- new_size
          length(ref_first_vec) <- new_size
          length(ref_last_vec) <- new_size
          length(overlap_vec) <- new_size
          length(r_pearson_vec) <- new_size
          length(t_St_vec) <- new_size
          length(t_BP_vec) <- new_size
          length(t_Ho_vec) <- new_size
          length(sgc_vec) <- new_size
          length(ssgc_vec) <- new_size
          length(sgc_p_vec) <- new_size
          length(glk_vec) <- new_size
          length(glk_p_vec) <- new_size
        }

        # Store values at current position
        series_vec[result_count] <- series_name
        length_vec[result_count] <- series_stats$length
        first_vec[result_count] <- series_first
        last_vec[result_count] <- series_last
        reference_vec[result_count] <- ref_name
        ref_first_vec[result_count] <- ref_first
        ref_last_vec[result_count] <- ref_last
        overlap_vec[result_count] <- pv_result$overlap[i, j]
        r_pearson_vec[result_count] <- cor_result$r_pearson[i, j]
        t_St_vec[result_count] <- cor_result$t_St[i, j]
        t_BP_vec[result_count] <- tbp_result$t_BP[i, j]
        t_Ho_vec[result_count] <- tho_result$t_Ho[i, j]
        sgc_vec[result_count] <- pv_result$sgc[i, j]
        ssgc_vec[result_count] <- pv_result$ssgc[i, j]
        sgc_p_vec[result_count] <- pv_result$sgc_p[i, j]
        glk_vec[result_count] <- pv_result$glk[i, j]
        glk_p_vec[result_count] <- pv_result$glk_p[i, j]
      }
    }

    if (pb) {
      # Update progress bar
      progress <- progress + 1
      utils::setTxtProgressBar(pbar, progress)
    }
  }

  if (pb) {
    # Close progress bar
    close(pbar)
    cat("\nProcessing crossdate results...\n")
  }

  # Create final data.frame once at the end
  if (result_count > 0) {
    # Trim vectors to actual size
    series_vec <- series_vec[1:result_count]
    length_vec <- length_vec[1:result_count]
    first_vec <- first_vec[1:result_count]
    last_vec <- last_vec[1:result_count]
    reference_vec <- reference_vec[1:result_count]
    ref_first_vec <- ref_first_vec[1:result_count]
    ref_last_vec <- ref_last_vec[1:result_count]
    overlap_vec <- overlap_vec[1:result_count]
    r_pearson_vec <- r_pearson_vec[1:result_count]
    t_St_vec <- t_St_vec[1:result_count]
    t_BP_vec <- t_BP_vec[1:result_count]
    t_Ho_vec <- t_Ho_vec[1:result_count]
    sgc_vec <- sgc_vec[1:result_count]
    ssgc_vec <- ssgc_vec[1:result_count]
    sgc_p_vec <- sgc_p_vec[1:result_count]
    glk_vec <- glk_vec[1:result_count]
    glk_p_vec <- glk_p_vec[1:result_count]

    results_df <- data.frame(
      series = series_vec,
      length = length_vec,
      first = first_vec,
      last = last_vec,
      reference = reference_vec,
      ref_first = ref_first_vec,
      ref_last = ref_last_vec,
      overlap = overlap_vec,
      r_pearson = r_pearson_vec,
      t_St = t_St_vec,
      t_BP = t_BP_vec,
      t_Ho = t_Ho_vec,
      sgc = sgc_vec,
      ssgc = ssgc_vec,
      sgc_p = sgc_p_vec,
      glk = glk_vec,
      glk_p = glk_p_vec,
      stringsAsFactors = FALSE
    )

    # Apply default sorting (by series, reference, first)
    ord_idx <- with(results_df, order(series, reference, first))
    results_df <- results_df[ord_idx, ]
    rownames(results_df) <- NULL

    # Apply optional sorting by rank_by and filtering by top_n (only when rank_by is specified)
    if (!is.null(rank_by)) {
      final_results <- do.call(rbind, lapply(split(results_df, results_df$series), function(group) {
        # Handle NA values in ranking column
        valid_rows <- !is.na(group[[rank_by]])
        if (sum(valid_rows) == 0) {
          # If no valid values for ranking, return empty group if top_n filtering needed
          if (!is.null(top_n)) {
            return(group[0, , drop = FALSE]) # Return empty if top_n filtering needed
          } else {
            return(group) # Return unsorted group
          }
        }

        # Sort by rank_by (high to low)
        group_valid <- group[valid_rows, , drop = FALSE]
        group_invalid <- group[!valid_rows, , drop = FALSE]

        ord <- order(-group_valid[[rank_by]])
        group <- rbind(group_valid[ord, , drop = FALSE], group_invalid)

        # Apply top_n filtering if specified
        if (!is.null(top_n)) {
          group <- utils::head(group, n = top_n)
        }

        return(group)
      }))
      rownames(final_results) <- NULL
      return(final_results)
    }
  } else {
    results_df <- data.frame()
  }

  return(results_df)
}
