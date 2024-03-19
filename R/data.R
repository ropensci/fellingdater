#' Brathen 1982 sapwood data set.
#'
#' Sapwood data set for Western Sweden published by Bräthen in 1982.
#'
#' @name Brathen_1982
#' @docType data
#' @format A tibble of 2 variables:
#' \describe{
#'   \item{n_sapwood}{number of sapwood rings}
#'   \item{count}{number of times n_sapwood was observed}
#'   }
#' @source Bräthen A. 1982. A tree-ring chronology from the western part of Sweden. Sapwood and a dating problem, in: Hackens T., Mejdahl V. (Eds.), Second Nordic Conference on the Application of Scientific Methods in Archaeology, PACT 7(1). pp. 27–35.
#' @details
#'     sample size = 69 observations
#' @examples
#' sw_data_info("Brathen_1982")
#'
#' sw_model("Brathen_1982", plot = TRUE)
#'
"Brathen_1982"

#' Hollstein 1980 sapwood data set.
#'
#' Sapwood data set for South and Central Germany published by Hollstein in 1980.
#'
#' @name Hollstein_1980
#' @keywords Hollstein_1980
#' @docType data
#' @format A tibble of 2 variables:
#' \describe{
#'   \item{n_sapwood}{number of sapwood rings}
#'   \item{count}{number of times n_sapwood was observed}
#'   }
#' @source Hollstein E. 1980. Mitteleuropäische Eichenchronologie: Trierer dendrochronologische Forschungen zur Archäologie und Kunstgeschichte, Trierer Grabungen und Forschungen. Verlag Phillipp von Zabern, Mainz am Rhein.
#' @details
#'     sample size = 490 observations
#' @examples
#' sw_data_info("Hollstein_1980")
#'
#' sw_model("Hollstein_1980", plot = TRUE)
#'
"Hollstein_1980"


#' Miles 1997 sapwood data set.
#'
#' Sapwood data set for the Northern Midland counties (U.K.) - Cheshire, Staffordshire, West Midlands, Northamptonshire, Cambridgeshire, and everything to the north - published by Miles in 1997.
#'
#' @name Miles_1997_NM
#' @docType data
#' @format A tibble of 2 variables:
#' \describe{
#'   \item{n_sapwood}{number of sapwood rings}
#'   \item{count}{number of times n_sapwood was observed}
#'   }
#' @details
#'     sample size = 295 observations
#' @source Miles D. 1997. The interpretation, presentation and use of tree-ring dates. Vernacular architecture 28, 40–56. <https://doi.org/10.1179/030554797786050563>
#' @examples
#' sw_data_info("Miles_1997_NM")
#'
#' sw_model("Miles_1997_NM", plot = TRUE)
#'
"Miles_1997_NM"


#' Miles 1997 sapwood data set.
#'
#' Sapwood data set for the Southern counties (U.K.), up to and including Gloucestershire, Warwickshire, Bedfordshire, Suffolk and Norfolk, published by Miles in 1997.
#'
#' @name Miles_1997_SC
#' @docType data
#' @format A tibble of 2 variables:
#' \describe{
#'   \item{n_sapwood}{number of sapwood rings}
#'   \item{count}{number of times n_sapwood was observed}
#'   }
#' @details
#'     sample size = 406 observations
#' @source Miles D. 1997. The interpretation, presentation and use of tree-ring dates. Vernacular architecture 28, 40–56. <https://doi.org/10.1179/030554797786050563>
#' @examples
#' sw_data_info("Miles_1997_SC")
#'
#' sw_model("Miles_1997_SC", plot = TRUE)
#'
"Miles_1997_SC"


#' Miles 1997 sapwood data set.
#'
#' Sapwood data set for Wales and border counties (U.K.), Shropshire, Hereford and Worcesterthe, published by Miles in 1997.
#'
#' @name Miles_1997_WBC
#' @docType data
#' @format A tibble of 2 variables:
#' \describe{
#'   \item{n_sapwood}{number of sapwood rings}
#'   \item{count}{number of times n_sapwood was observed}
#'   }
#' @details
#'     sample size = 219 observations
#' @source Miles D. 1997. The interpretation, presentation and use of tree-ring dates. Vernacular architecture 28, 40–56. <https://doi.org/10.1179/030554797786050563>
#' @examples
#' sw_data_info("Miles_1997_WBC")
#'
#' sw_model("Miles_1997_WBC", plot = TRUE)
#'
"Miles_1997_WBC"


#' Pilcher 1987 sapwood data set.
#'
#' Sapwood data set for northern France , published by Pilcher in 1987.
#'
#' @name Pilcher_1987
#' @docType data
#' @format A tibble of 2 variables:
#' \describe{
#'   \item{n_sapwood}{number of sapwood rings}
#'   \item{count}{number of times n_sapwood was observed}
#'   }
#' @details
#'     sample size = 219 observations
#' @source Pilcher J.R. 1987. A 700 year dating chronology for northern France. Applications of tree-ring studies. Current research in dendrochronology and related subjects. BAR International Series 333, 127–139.
#' @examples
#' sw_data_info("Pilcher_1987")
#'
#' sw_model("Pilcher_1987", plot = TRUE)
#'
"Pilcher_1987"


#' Sohar et al. 2012 sapwood data set.
#'
#' Sapwood data set for Eastern Estonia, Latvia, Lithuania, published by Sohar et al. in 2012.
#'
#' @name Sohar_2012_ELL_c
#' @docType data
#' @format A tibble of 2 variables:
#' \describe{
#'   \item{n_sapwood}{number of sapwood rings}
#'   \item{count}{number of times n_sapwood was observed}
#'   }
#' @details
#'     sample size = 562 observations
#'     !!! sapwood determined by _color_
#' @source Sohar K., Vitas A. & Läänelaid A. 2012. Sapwood estimates of pedunculate oak (*Quercus robur* L.) in eastern Baltic, Dendrochronologia 30.1, 49–56. <https://doi.org/10.1016/j.dendro.2011.08.001>
#' @examples
#' sw_data_info("Sohar_2012_ELL_c")
#'
#' sw_model("Sohar_2012_ELL_c", plot = TRUE)
#'
"Sohar_2012_ELL_c"


#' Sohar et al. 2012 sapwood data set.
#'
#' Sapwood data set for Eastern Estonia, Latvia, Lithuania, published by Sohar et al. in 2012.
#'
#' @name Sohar_2012_ELL_t
#' @docType data
#' @format A tibble of 2 variables:
#' \describe{
#'   \item{n_sapwood}{number of sapwood rings}
#'   \item{count}{number of times n_sapwood was observed}
#'   }
#' @details
#'     sample size = 533 observations
#'     !!! sapwood determined by presence of _tyloses_
#' @source Sohar K., Vitas A. & Läänelaid A. 2012. Sapwood estimates of pedunculate oak (*Quercus robur* L.) in eastern Baltic, Dendrochronologia 30.1, 49–56. <https://doi.org/10.1016/j.dendro.2011.08.001>
#' @examples
#' sw_data_info("Sohar_2012_ELL_t")
#'
#' sw_model("Sohar_2012_ELL_t", plot = TRUE)
#'
"Sohar_2012_ELL_t"


#' Sohar et al. 2012 sapwood data set.
#'
#' Sapwood data set for Southern Finland and western Estonia, published by Sohar et al. in 2012.
#'
#' @name Sohar_2012_FWE_c
#' @docType data
#' @format A tibble of 2 variables:
#' \describe{
#'   \item{n_sapwood}{number of sapwood rings}
#'   \item{count}{number of times n_sapwood was observed}
#'   }
#' @details
#'     sample size = 98 observations
#'     !!! sapwood determined by _color_
#' @source Sohar K., Vitas A. & Läänelaid A. 2012. Sapwood estimates of pedunculate oak (*Quercus robur* L.) in eastern Baltic, Dendrochronologia 30.1, 49–56. <https://doi.org/10.1016/j.dendro.2011.08.001>
#' @examples
#' sw_data_info("Sohar_2012_FWE_c")
#'
#' sw_model("Sohar_2012_FWE_c", plot = TRUE)
#'
"Sohar_2012_FWE_c"


#' Sohar et al. 2012 sapwood data set.
#'
#' Sapwood data set for Southern Finland and western Estonia, published by Sohar et al. in 2012.
#'
#' @name Sohar_2012_FWE_t
#' @docType data
#' @format A tibble of 2 variables:
#' \describe{
#'   \item{n_sapwood}{number of sapwood rings}
#'   \item{count}{number of times n_sapwood was observed}
#'   }
#' @details
#'     sample size = 97 observations
#'     !!! sapwood determined by the presence of _tyloses_
#' @source Sohar K., Vitas A. & Läänelaid A. 2012. Sapwood estimates of pedunculate oak (*Quercus robur* L.) in eastern Baltic, Dendrochronologia 30.1, 49–56. <https://doi.org/10.1016/j.dendro.2011.08.001>
#' @examples
#' sw_data_info("Sohar_2012_FWE_t")
#'
#' sw_model("Sohar_2012_FWE_t", plot = TRUE)
#'
"Sohar_2012_FWE_t"


#' Wazny 1990 sapwood data set.
#'
#' Sapwood data set for Poland, published by Wazny in 1990.
#'
#' @name Wazny_1990
#' @docType data
#' @format A tibble of 2 variables:
#' \describe{
#'   \item{n_sapwood}{number of sapwood rings}
#'   \item{count}{number of times n_sapwood was observed}
#'   }
#' @details
#'     sample size = 206 observations
#' @source Wazny T. 1990. Aufbau und Anwendung der Dendrochronologie für Eichenholz in Polen (PhD dissertation). Hamburg University, Hamburg.
#' @examples
#' sw_data_info("Wazny_1990")
#'
#' sw_model("Wazny_1990", plot = TRUE)
#'
"Wazny_1990"


#' van Daalen (unpublished) sapwood data set.
#'
#' Sapwood data set for historical timbers found in the Netherlands that were
#'   imported from Norway.
#'   Unpublished data by S. van Daalen (version 19 Dec 2022, Van Daalen Dendrochronologie -
#'   <www.dendro.nl>).
#'
#' @name vanDaalen_Norway
#' @docType data
#' @format A tibble of 2 variables:
#' \describe{
#'   \item{n_sapwood}{number of sapwood rings}
#'   \item{count}{number of times n_sapwood was observed}
#'   }
#' @details
#'     sample size = 104 observations
#' @source S. van Daalen, unpublished dataset (version: 19 Dec 2022).
#' @examples
#' sw_data_info("vanDaalen_Norway")
#'
#' sw_model("vanDaalen_Norway", plot = TRUE)
#'
"vanDaalen_Norway"

#' van Daalen (unpublished) sapwood data set.
#'
#' Sapwood data set for historical timbers found in the Netherlands en Belgium,
#'   with a local provenance.
#'   Unpublished data by S. van Daalen (version 19 Dec 2022, Van Daalen Dendrochronologie -
#'   <www.dendro.nl>).
#'
#' @name vanDaalen_NLBE
#' @docType data
#' @format A tibble of 2 variables:
#' \describe{
#'   \item{n_sapwood}{number of sapwood rings}
#'   \item{count}{number of times n_sapwood was observed}
#'   }
#' @details
#'     sample size = 644 observations
#' @source S. van Daalen, unpublished dataset (version: 19 Dec 2022).
#' @examples
#' sw_data_info("vanDaalen_NLBE")
#'
#' sw_model("vanDaalen_NLBE", plot = TRUE)
#'
"vanDaalen_NLBE"

#' Example dataset 0
#'
#' A dataset in which all series have preserved sapwood.
#'   Unconventional variable names.
#'
#' @format A tibble of 4 variables:
#' \describe{
#'   \item{trs}{unique ID of the tree-ring series}
#'   \item{end}{calendar year assigned to the last measured ring}
#'   \item{swr}{number of observed sapwood rings}
#'   \item{bark}{waney edge present TRUE/FALSE}
#'   }
#' @examples
#' trs_example0
#'

"trs_example0"

#' Example dataset 1
#'
#' A dataset in which all series have preserved sapwood rings.
#'
#' @format A tibble of 4 variables:
#' \describe{
#'   \item{series}{unique ID of the tree-ring series}
#'   \item{last}{calendar year assigned to the last measured ring}
#'   \item{n_sapwood}{number of observed sapwood rings}
#'   \item{waneyedge}{waney edge present TRUE/FALSE}
#'   }
#' @examples
#' trs_example1
#'
"trs_example1"

#' Example dataset 2
#'
#' A dataset in which one series has an exact felling date (= waney edge present).
#'
#' @format A tibble of 4 variables:
#' \describe{
#'   \item{series}{unique ID of the tree-ring series}
#'   \item{last}{calendar year assigned to the last measured ring}
#'   \item{n_sapwood}{number of observed sapwood rings}
#'   \item{waneyedge}{waney edge present TRUE/FALSE}
#'   }
#' @examples
#' trs_example2
#'
"trs_example2"

#' Example dataset 3
#'
#' A dataset with multiple exact felling dates.
#'
#' @format A tibble of 4 variables:
#' \describe{
#'   \item{series}{unique ID of the tree-ring series}
#'   \item{last}{calendar year assigned to the last measured ring}
#'   \item{n_sapwood}{number of observed sapwood rings}
#'   \item{waneyedge}{waney edge present TRUE/FALSE}
#'   }
#' @examples
#' trs_example3
#'
"trs_example3"

#' Example dataset 4
#'
#' A combination of series with and without sapwood rings.
#'
#' @format A tibble of 4 variables:
#' \describe{
#'   \item{series}{unique ID of the tree-ring series}
#'   \item{last}{calendar year assigned to the last measured ring}
#'   \item{n_sapwood}{number of observed sapwood rings}
#'   \item{waneyedge}{waney edge present TRUE/FALSE}
#'   }
#' @examples
#' trs_example4
#'
"trs_example4"

#' Example dataset 5
#'
#' None of the series in this dataset have preserved sapwood rings.
#'
#' @format A tibble of 4 variables:
#' \describe{
#'   \item{series}{unique ID of the tree-ring series}
#'   \item{last}{calendar year assigned to the last measured ring}
#'   \item{n_sapwood}{number of observed sapwood rings}
#'   \item{waneyedge}{waney edge present TRUE/FALSE}
#'   }
#' @examples
#' trs_example5
#'
"trs_example5"

#' Example dataset 6
#'
#' A test dataset for sw_sum().
#'
#' @format A tibble of 4 variables:
#' \describe{
#'   \item{series}{unique ID of the tree-ring series}
#'   \item{last}{calendar year assigned to the last measured ring}
#'   \item{n_sapwood}{number of observed sapwood rings}
#'   \item{waneyedge}{waney edge present TRUE/FALSE}
#'   }
#' @examples
#' trs_example6
#'
"trs_example6"

#' Example dataset 7
#'
#' A test dataset for sw_sum().
#'
#' @format A tibble of 4 variables:
#' \describe{
#'   \item{series}{unique ID of the tree-ring series}
#'   \item{last}{calendar year assigned to the last measured ring}
#'   \item{n_sapwood}{number of observed sapwood rings}
#'   \item{waneyedge}{waney edge present TRUE/FALSE}
#'   }
#' @examples
#' trs_example7
#'
"trs_example7"
