# fellingdater 1.2.0 (2025-06-30)

NEW FEATURES
A new set of functions for tree-ring dating, analysis, and visualization (trs_*)

- Data generation and preparation:
trs_pseudo_rwl(): Generate synthetic tree-ring datasets for testing and method development
trs_select(): Select specific series from tree-ring data
trs_remove(): Remove series from tree-ring datasets
trs_trim(): Remove leading and trailing NA's in tree-ring datasets or single series

- Crossdating analysis:
trs_crossdate(): Comprehensive crossdating analysis with multiple statistical measures
trs_date_end(): Set end dates for tree-ring series based on crossdating results
trs_tho(): Calculate t-values according to the Hollstein (1980) transformation
trs_tbp(): Calculate t-values according to the Baillie & Pilcher (1973) transformation
trs_pv(): Calculate percentage of parallel variation including SGC, SSGC, and Gleichläufigkeit, with significance levels
trs_tSt(): Calculate correlation measures (Pearson r and t-values)

- Visualization:
trs_plot_dated(): Display crossdating results with highlighting of parallel variation
trs_plot_rwl(): Plot individual tree-ring series with customizable colors and styling

Enhanced workflow integration

Tree-ring analysis functions now seamlessly integrate with existing felling date estimation (sw_* functions)
Complete dendroarchaeological workflow from data processing through crossdating to felling date estimation
Support for both synthetic data generation and real dendrochronological datasets

NEW DATASET

- Weitz_2025: New sapwood model for the Brussels region (Belgium) based on Weitz et al. (2025)


# fellingdater 1.1.0 (2025-06-05)

-   Fixes to ensure that a user-defined sapwood dataset can be used with all relevant functions (fd_report(), sw_combine() and sw_sum().
-   Extra checks on the input (e.g. check_input(), check_duplicate_labels() and check_sapwood_data_user().
    These additional functions have been added to 'helper-functions.R' and integrated into fd_report(), sw_combine() and sw_sum().
-   The example data sets for testing sw_combine() and sw_sum() and fd_report() have been renamed from trs_example(1 to 7) to sw_example(1 to 7).

# fellingdater 1.0.3 (2025-05-24)

-   fix to fd_report() to allow user-defined sw_data.


# fellingdater 1.0.2 (2024-05-22)

### PUBLICATION

-   Paper accepted for publication in 'The Journal of Open Source Software' (JOSS).
-   doi: <https://doi.org/10.21105/joss.06716>
-   CITATION and README updated accordingly

# fellingdater 1.0.1 (2024-05-13)

-   Paper.md updated for 'The Journal of Open Source Software' (JOSS).
-   New vignette (article): "The fellingdater workflow"

# fellingdater 1.0.0 (2024-04-16)

-   Major release of `fellingdater` to reflect the fact that the package is now fully functional and has successfully passed the rOpenSci review process.
-   review history: <https://github.com/ropensci/software-review/issues/618>

# fellingdater 0.0.0.9004 (2024-03-21)

### REVIEW

-   new version of the package after review
-   review history: <https://github.com/ropensci/software-review/issues/618>
-   `fellingdateR`has been renamed to `fellingdater`

# fellingdater 0.0.0.9003 (2023-11-20)

### NEW FEATURES

-   Version ready for submission to rOpenSci for software review
-   First draft op paper for 'The Journal of Open Source Software' in /paper/paper.md
