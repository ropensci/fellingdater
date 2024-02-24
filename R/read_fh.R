#' Read a Heidelberg format (.fh) tree-ring file
#'
#' @description This function reads a Heidelberg format file (.fh) containing
#' ring width data and returns the HEADER fields as attributes of the
#' `data.frame`´with the measurement data.
#'
#' @param fname A `character` vector specifying the name of the .fh file to
#'   read.
#' @param BC_correction A `logical`. If `TRUE` the correction moves BC-dates one
#'   year forward.
#' @param verbose A `logical`. If `FALSE`, messages are suppressed during the
#'   reading process.
#' @param header A `logical`. If `TRUE` the HEADER fields are returned as a
#'   `data.frame`; if `FALSE`, the measurement data is returned.
#'
#' @references This function is an extension of `read.fh()` from the **dplR
#'   package** (<https://github.com/opendendro/dplR>), developed and maintained
#'   by Prof. dr. Andy Bunn (Bunn 2008, Bunn 2010, Bunn et al. 2022) on
#'   <https://opendendro.org/>.
#'
#' @details This function reads .fh file with ring width data in either block
#'   (decadal format) or column format (e.g., with comment flags) as used by
#'   TSAP program. The function is also capable of reading chronologies or
#'   half-chronos in decadal format. The `read_fh` function is case-insensitive.
#'   Information found in the HEADER fields is listed as attributes of the
#'   `data.frame`´with the measurement data. .
#'
#'   The header fields harvested from the .fh file include:
#'
#'   "Project", "FirstMeasurementDate", "Location", "Town", "Street", "Client",
#'   "Longitude", "Latitude", "DateOfSampling", "FirstMeasurementDate",
#'   "SapWoodRings", "Comment", "MissingRingsAfter", "InvalidRingsAfter",
#'   "MissingringsBefore", "DeltaMissingringsBefore", "ChronoMemberKeycodes",
#'   "PersId"
#'
#' @return If `header` is TRUE, a data.frame is returned with HEADER fields as
#'   attributes. If `header` is FALSE, a `data.frame` of class `rwl` with
#'   ring-width measurements in columns is returned, with (calendar) years as
#'   row names.`
#'
#' @author The original `read.fh()` function is part of the **dplR package**
#'   (<https://github.com/opendendro/dplR>) and was developed by Christian Zang,
#'   with new features and patches contributed by Mikko Korpela and Ronald
#'   Visser. This `read_fh()` function expands the functionalities of the
#'   original [dplR::read.fh()].
#'
#' @export
#' @examples
#' Doel1 <- system.file("extdata", "DOEL1.fh", package = "fellingdateR")
#' Doel1_trs <- read_fh(Doel1, verbose = FALSE)
#' head(Doel1_trs, 10)
#'
#' Doel1_header <- read_fh(Doel1, verbose = FALSE, header = TRUE)
#' Doel1_header
#'
read_fh <- function(fname,
                    BC_correction = FALSE,
                    verbose = TRUE,
                    header = FALSE) {
        # NEW: verbose = TRUE, header = FALSE
        inp <- readLines(fname, ok = TRUE, warn = FALSE)
        # NEW: removes empty lines in .fh file
        inp <- inp[length(inp) != 0]
        ## Get start and end positions of headers and data blocks
        header.begin <- grep("^HEADER:$", inp)
        # NEW: Quadro => chrono
        # NEW: Double => half chrono
        header.end <-
                grep("^DATA:(Tree|Single|HalfChrono|Double|Quadro)$",
                     inp)
        n <- length(header.end)
        if (n == 0) {
                # NEW: warning now includes Double and Quadro
                stop('file has no data in "Tree", "Single" of "(half)Chrono" formats')
        }
        ## For each data block in one of the supported formats, find the
        ## corresponding header block
        header.taken <- logical(length(header.begin))
        for (i in seq_len(n)) {
                n.preceding <- sum(header.begin < header.end[i] - 1)
                if (n.preceding == 0 || header.taken[n.preceding]) {
                        stop("invalid file: HEADER and DATA don't match")
                } else {
                        header.taken[n.preceding] <- TRUE
                }
        }
        if (!all(header.taken)) {
                warning("more HEADER blocks than DATA blocks in supported formats")
        }
        ## For each data block in one of the supported formats, find the
        ## following header block (or end of file)
        data.end <- numeric(n)
        for (i in seq_len(n - 1)) {
                tmp <- header.begin[header.begin > header.end[i]]
                data.end[i] <- tmp[1]
        }
        tmp <- header.begin[header.begin > header.end[n]]
        if (length(tmp) > 0) {
                data.end[n] <- tmp[1]
        } else {
                data.end[n] <- length(inp) + 1
        }
        ## Forget headers that are not used by the data blocks
        header.begin <- header.begin[header.taken]

        ## Get essential metadata from headers
        keycodes <- character(n)
        lengths <- numeric(n) # commit Ronald Visser
        start.years <- numeric(n)
        end.years <- numeric(n)
        multipliers <- rep(1, n)
        divisors <- rep(100, n)
        site.code <- rep(NA_character_, n)
        tree.vec <- rep(NA_real_, n)
        core.vec <- rep(NA_real_, n)
        radius.vec <- rep(NA_real_, n)
        stemdisk.vec <- rep(NA_real_, n)
        pith.offset <- rep(NA_real_, n)
        ## NEW: extra header fields added
        pith_offset_delta <- rep(NA_real_, n)
        data_type <- character(n)
        species <- rep(NA_character_, n)
        sapwoodrings <- numeric(n)
        sapwoodrings_chr <- rep(NA_character_, n)
        unmeasured_rings <- numeric(n)
        invalid_rings <- numeric(n)
        status <- rep(NA_character_, n)
        waneyedge <- rep(NA_character_, n)
        bark <- rep(NA_character_, n)
        pith <- rep(NA_character_, n)
        comments <- rep(NA_character_, n)
        project <- rep(NA_character_, n)
        location <- rep(NA_character_, n)
        town <- rep(NA_character_, n)
        town_zip <- rep(NA_character_, n)
        street <- rep(NA_character_, n)
        personal_id <- rep(NA_character_, n)
        sampling_date <- rep(NA_character_, n)
        measuring_date <- rep(NA_character_, n)
        client_id <- rep(NA_character_, n)
        longitude <- rep(NA_character_, n)
        latitude <- rep(NA_character_, n)
        chrono_members <- rep(NA_character_, n)

        for (i in seq_len(n)) {
                # make case-insensitive by replacing 'fixed=TRUE' by 'ignore.case=TRUE'###
                ## NEW: get data_type
                data_type[i] <- inp[(header.end[i])]
                data_type[i] <- sub(
                        "DATA:",
                        "",
                        ignore.case = TRUE,
                        x = grep(
                                "^DATA:",
                                data_type,
                                value = TRUE,
                                ignore.case = TRUE
                        )
                )

                this.header <- inp[(header.begin[i] + 1):(header.end[i] - 1)]
                ## get keycode (= series id)
                this.keycode <- sub(
                        "KEYCODE=",
                        "",
                        ignore.case = TRUE,
                        x = grep(
                                "^KEYCODE=",
                                this.header,
                                value = TRUE,
                                ignore.case = TRUE
                        )
                )
                if (length(this.keycode) != 1) {
                        string2 <- gettext('number of "KeyCode" lines is not 1',
                                           domain = "R-dplR")
                        stop(
                                gettextf(
                                        "in series %s: ",
                                        as.character(i),
                                        domain = "R-dplR"
                                ),
                                string2,
                                domain = NA
                        )
                } else {
                        keycodes[i] <- this.keycode
                }
                # get length
                this.length <- sub(
                        "LENGTH=",
                        "",
                        ignore.case = TRUE,
                        x = grep(
                                "^LENGTH=",
                                this.header,
                                value = TRUE,
                                ignore.case = TRUE
                        )
                )
                if (length(this.length) != 1) {
                        string2 <- gettext('number of "Length" lines is not 1',
                                           domain = "R-dplR")
                        stop(
                                gettextf("in series %s: ", keycodes[i], domain = "R-dplR"),
                                string2,
                                domain = NA
                        )
                } else {
                        lengths[i] <- as.numeric(this.length)
                }
                ## get end year
                this.end.year <- sub(
                        "DATEEND=",
                        "",
                        ignore.case = TRUE,
                        x = grep(
                                "^DATEEND=",
                                this.header,
                                value = TRUE,
                                ignore.case = TRUE
                        )
                )
                if (length(this.end.year) != 1) {
                        string2 <- gettext('number of "DateEnd" lines is not 1',
                                           domain = "R-dplR")
                        stop(
                                gettextf("in series %s: ", keycodes[i], domain = "R-dplR"),
                                string2,
                                domain = NA
                        )
                } else {
                        end.years[i] <- as.numeric(this.end.year)
                }
                ## get start year
                this.start.year <- sub(
                        "DATEBEGIN=",
                        "",
                        ignore.case = TRUE,
                        x = grep(
                                "^DATEBEGIN=",
                                this.header,
                                value = TRUE,
                                ignore.case = TRUE
                        )
                )
                if (length(this.start.year) != 1) {
                        if (length(this.end.year) == 1) {
                                start.years[i] <- end.years[i] - lengths[i] + 1
                        } else {
                                string2 <- gettext('number of "DateBegin" lines is not 1',
                                                   domain = "R-dplR")
                                stop(
                                        gettextf(
                                                "in series %s: ",
                                                keycodes[i],
                                                domain = "R-dplR"
                                        ),
                                        string2,
                                        domain = NA
                                )
                        }
                } else {
                        start.years[i] <- as.numeric(this.start.year)
                        # check for BC - AD dates and add not existing year 0
                        if (start.years[i] < 0 &&
                            end.years[i] > 0 && BC_correction == TRUE) {
                                start.years[i] <- start.years[i] + 1
                        }
                }
                ## correct BC dates to +1, because rwl-format uses non-existing year 0 and this is not necessary in the FH-format
                if (start.years[i] < 0 &&
                    end.years[i] < 0 && BC_correction == TRUE) {
                        start.years[i] <- start.years[i] + 1
                        end.years[i] <- end.years[i] + 1
                }
                ## get unit (by default, divide by 100)
                this.unit <- sub(
                        "UNIT=",
                        "",
                        ignore.case = TRUE,
                        # new
                        x = grep(
                                "^UNIT=",
                                this.header,
                                value = TRUE,
                                ignore.case = TRUE
                        )
                )
                if (length(this.unit) == 1) {
                        this.unit <- sub("mm", "", this.unit, ignore.case = TRUE) # new
                        div.loc <- regexpr("/", this.unit, fixed = TRUE)
                        if (div.loc > 0) {
                                multipliers[i] <- as.numeric(substr(this.unit, 1, div.loc - 1))
                                divisors[i] <-
                                        as.numeric(substr(
                                                this.unit,
                                                div.loc + 1,
                                                nchar(this.unit)
                                        ))
                        } else {
                                multipliers[i] <- as.numeric(this.unit)
                                divisors[i] <- 1
                        }
                        if (is.na(multipliers[i]) || is.na(divisors[i])) {
                                string2 <- gettext('cannot interpret "Unit" line',
                                                   domain = "R-dplR")
                                stop(
                                        gettextf(
                                                "in series %s: ",
                                                keycodes[i],
                                                domain = "R-dplR"
                                        ),
                                        string2,
                                        domain = NA
                                )
                        }
                } else if (length(this.unit) > 1) {
                        string2 <- gettext('number of "Unit" lines is > 1',
                                           domain = "R-dplR")
                        stop(
                                gettextf("in series %s: ", keycodes[i], domain = "R-dplR"),
                                string2,
                                domain = NA
                        )
                }
                ## get site code
                this.site <- sub(
                        "SITECODE=",
                        "",
                        ignore.case = TRUE,
                        x = grep(
                                "^SITECODE=",
                                this.header,
                                value = TRUE,
                                ignore.case = TRUE
                        )
                )
                if (length(this.site) == 1) {
                        site.code[i] <- this.site
                }
                ## get tree number
                this.tree <- sub(
                        "TreeNo=",
                        "",
                        ignore.case = TRUE,
                        x = grep(
                                "^TreeNo=",
                                this.header,
                                value = TRUE,
                                ignore.case = TRUE
                        )
                )
                if (length(this.tree) == 1) {
                        tmp <- suppressWarnings(as.numeric(this.tree))
                        if (identical(tmp, round(tmp))) {
                                tree.vec[i] <- tmp
                        }
                }
                ## get core number
                this.core <- sub(
                        "CoreNo=",
                        "",
                        ignore.case = TRUE,
                        x = grep(
                                "^CoreNo=",
                                this.header,
                                value = TRUE,
                                ignore.case = TRUE
                        )
                )
                if (length(this.core) == 1) {
                        tmp <- suppressWarnings(as.numeric(this.core))
                        if (identical(tmp, round(tmp))) {
                                core.vec[i] <- tmp
                        }
                }
                ## get radius number
                this.radius <- sub(
                        "RadiusNo=",
                        "",
                        fixed = TRUE,
                        x = grep("^RadiusNo=", this.header, value =
                                         TRUE)
                )
                if (length(this.radius) == 1) {
                        tmp <- suppressWarnings(as.numeric(this.radius))
                        if (identical(tmp, round(tmp))) {
                                radius.vec[i] <- tmp
                        }
                }
                ## get stem disk number
                this.stemdisk <-
                        sub(
                                "StemDiskNo=",
                                "",
                                ignore.case = TRUE,
                                x = grep(
                                        "^StemDiskNo=",
                                        this.header,
                                        value = TRUE,
                                        ignore.case = TRUE
                                )
                        )
                if (length(this.stemdisk) == 1) {
                        tmp <- suppressWarnings(as.numeric(this.stemdisk))
                        if (identical(tmp, round(tmp))) {
                                stemdisk.vec[i] <- tmp
                        }
                }
                ## get pith offset (missing rings before start of series)
                this.missing <-
                        sub(
                                "MissingRingsBefore=",
                                "",
                                ignore.case = TRUE,
                                x = grep(
                                        "^MissingRingsBefore=",
                                        this.header,
                                        value = TRUE,
                                        ignore.case = TRUE
                                )
                        )
                if (length(this.missing) == 1) {
                        tmp <- suppressWarnings(as.numeric(this.missing))
                        if (identical(tmp, round(tmp)) &&
                            tmp >= 0 && !is.na(tmp)) {
                                #new !is.na() when text input
                                pith.offset[i] <- tmp + 1
                        }
                }
                ## NEW: get pith offset uncertainty (delta missing rings before start of series)
                this.missing.delta <-
                        sub(
                                "DELTAMISSINGRINGSBEFORE=",
                                "",
                                ignore.case = TRUE,
                                x = grep(
                                        "^DELTAMISSINGRINGSBEFORE=",
                                        this.header,
                                        value = TRUE,
                                        ignore.case = TRUE
                                )
                        )
                if (length(this.missing.delta) == 1) {
                        tmp <- suppressWarnings(as.numeric(this.missing.delta))
                        if (identical(tmp, round(tmp)) &&
                            tmp >= 0 && !is.na(tmp)) {
                                #new !is.na() when text input
                                pith_offset_delta[i] <- tmp
                        }
                }

                ## NEW: get Species code
                this.species <- sub(
                        "SPECIES=",
                        "",
                        ignore.case = TRUE,
                        x = grep(
                                "^SPECIES=",
                                this.header,
                                value = TRUE,
                                ignore.case = TRUE
                        )
                )
                if (length(this.species) == 1) {
                        species[i] <- this.species
                }

                ## NEW: get status (Dated/Undated/relDated)
                this.status <- sub(
                        "DATED=",
                        "",
                        ignore.case = TRUE,
                        x = grep(
                                "^DATED=",
                                this.header,
                                value = TRUE,
                                ignore.case = TRUE
                        )
                )
                if (length(this.status) == 1) {
                        status[i] <- this.status
                }
                ## NEW: detect presence of pith
                this.pith <- sub(
                        "PITH=",
                        "",
                        ignore.case = TRUE,
                        x = grep(
                                "^PITH=",
                                this.header,
                                value = TRUE,
                                ignore.case = TRUE
                        )
                )
                if (length(this.pith) == 1) {
                        pith[i] <- this.pith
                }

                ## NEW: get number of recorded SapWoodRings
                this.swr <- sub(
                        "SAPWOODRINGS=",
                        "",
                        ignore.case = TRUE,
                        x = grep(
                                "^SAPWOODRINGS=",
                                this.header,
                                value = TRUE,
                                ignore.case = TRUE
                        )
                )
                ### when no sapwoodrings are present NA instead of zero
                if (identical(this.swr, character(0))) {
                        sapwoodrings[i] <- NA
                }
                ### when field SWR has a numeric value
                if (length(this.swr) == 1) {
                        tmp <- suppressWarnings(as.numeric(this.swr))
                        if (identical(tmp, round(tmp))) {
                                sapwoodrings[i] <- tmp
                        }
                }
                ### when field SWR has a non-numeric value
                if (length(this.swr) == 1) {
                        if (is.na(suppressWarnings(as.numeric(this.swr)))) {
                                tmp <- suppressWarnings(as.character(this.swr))
                                sapwoodrings_chr[i] <- tmp
                        }
                }

                ## NEW: get presence of waney edge
                this.waneyedge <- sub(
                        "WALDKANTE=",
                        "",
                        ignore.case = TRUE,
                        x = grep(
                                "^WALDKANTE=",
                                this.header,
                                value = TRUE,
                                ignore.case = TRUE
                        )
                )
                if (length(this.waneyedge) == 1) {
                        waneyedge[i] <- this.waneyedge
                }

                ## NEW: get presence of bark from header
                this.bark <- sub(
                        "BARK=",
                        "",
                        ignore.case = TRUE,
                        x = grep(
                                "^BARK=",
                                this.header,
                                value = TRUE,
                                ignore.case = TRUE
                        )
                )
                if (length(this.bark) == 1) {
                        bark[i] <- this.bark
                }

                ## NEW: get comments
                this.comments <- sub(
                        "COMMENT=",
                        "",
                        ignore.case = TRUE,
                        x = grep(
                                "^COMMENT=",
                                this.header,
                                value = TRUE,
                                ignore.case = TRUE
                        )
                )
                if (length(this.comments) == 1) {
                        comments[i] <- this.comments
                }

                ## NEW: get unmeasured, but observed rings (MissingRingsAfter)
                this.missing <-
                        sub(
                                "MISSINGRINGSAFTER=",
                                "",
                                ignore.case = TRUE,
                                x = grep(
                                        "^MISSINGRINGSAFTER=",
                                        this.header,
                                        value = TRUE,
                                        ignore.case = TRUE
                                )
                        )

                ### when no missing rings are present NA instead of zero
                if (identical(this.missing, character(0))) {
                        unmeasured_rings[i] <- NA
                }
                if (length(this.missing) == 1) {
                        tmp <- suppressWarnings(as.numeric(this.missing))
                        if (identical(tmp, round(tmp))) {
                                unmeasured_rings[i] <- tmp
                        }
                }

                ## NEW: get unreliable measurements at end - e.g. deformed rings (InvalidRingsAfter)
                ### sometimes this field is used instead of MissingRingsAfter
                this.invalid <-
                        sub(
                                "INVALIDRINGSAFTER=",
                                "",
                                ignore.case = TRUE,
                                x = grep(
                                        "^INVALIDRINGSAFTER=",
                                        this.header,
                                        value = TRUE,
                                        ignore.case = TRUE
                                )
                        )

                ### when no invalid rings are present NA instead of zero
                if (identical(this.invalid, character(0))) {
                        invalid_rings[i] <- NA
                }
                if (length(this.invalid) == 1) {
                        tmp <- suppressWarnings(as.numeric(this.invalid))
                        if (identical(tmp, round(tmp))) {
                                invalid_rings[i] <- tmp
                        }
                }

                ## NEW: get project name
                this.project <- sub(
                        "PROJECT=",
                        "",
                        ignore.case = TRUE,
                        x = grep(
                                "^Project=",
                                this.header,
                                value = TRUE,
                                ignore.case = TRUE
                        )
                )
                if (length(this.project) == 1) {
                        project[i] <- this.project
                }

                ## NEW: get location
                this.location <- sub(
                        "LOCATION=",
                        "",
                        ignore.case = TRUE,
                        x = grep(
                                "^LOCATION=",
                                this.header,
                                value = TRUE,
                                ignore.case = TRUE
                        )
                )
                if (length(this.location) == 1) {
                        location[i] <- this.location
                }

                ## NEW: get town name
                this.town <- sub(
                        "TOWN=",
                        "",
                        ignore.case = TRUE,
                        x = grep(
                                "^TOWN=",
                                this.header,
                                value = TRUE,
                                ignore.case = TRUE
                        )
                )
                if (length(this.town) == 1) {
                        town[i] <- this.town
                }
                ## NEW: get town zipcode
                this.zip <- sub(
                        "TOWNZIPCODE=",
                        "",
                        ignore.case = TRUE,
                        x = grep(
                                "^TOWNZIPCODE=",
                                this.header,
                                value = TRUE,
                                ignore.case = TRUE
                        )
                )
                if (length(this.zip) == 1) {
                        town_zip[i] <- this.zip
                }

                ## NEW: get street name
                this.street <- sub(
                        "STREET=",
                        "",
                        ignore.case = TRUE,
                        x = grep(
                                "^STREET=",
                                this.header,
                                value = TRUE,
                                ignore.case = TRUE
                        )
                )
                if (length(this.street) == 1) {
                        street[i] <- this.street
                }

                ## NEW: get personal id/author
                this.author <- sub(
                        "PERSID=",
                        "",
                        ignore.case = TRUE,
                        x = grep(
                                "^PERSID=",
                                this.header,
                                value = TRUE,
                                ignore.case = TRUE
                        )
                )
                if (length(this.author) == 1) {
                        personal_id[i] <- this.author
                }

                ## NEW: get sampling date
                this.date.sampling <-
                        sub(
                                "DATEOFSAMPLING=",
                                "",
                                ignore.case = TRUE,
                                x = grep(
                                        "^DATEOFSAMPLING=",
                                        this.header,
                                        value = TRUE,
                                        ignore.case = TRUE
                                )
                        )
                if (length(this.date.sampling) == 1) {
                        sampling_date[i] <- this.date.sampling
                }

                ## NEW: get first measurement date
                this.date.measuring <-
                        sub(
                                "FIRSTMEASUREMENTDATE=",
                                "",
                                ignore.case = TRUE,
                                x = grep(
                                        "^FIRSTMEASUREMENTDATE=",
                                        this.header,
                                        value = TRUE,
                                        ignore.case = TRUE
                                )
                        )
                if (length(this.date.measuring) == 1) {
                        measuring_date[i] <- this.date.measuring
                }

                ## NEW: get client id
                this.client <- sub(
                        "CLIENT=",
                        "",
                        ignore.case = TRUE,
                        x = grep(
                                "^CLIENT=",
                                this.header,
                                value = TRUE,
                                ignore.case = TRUE
                        )
                )
                if (length(this.client) == 1) {
                        client_id[i] <- this.client
                }

                ## NEW: get chrono member keycodes
                this.member <-
                        sub(
                                "CHRONOMEMBERKEYCODES=",
                                "",
                                ignore.case = TRUE,
                                x = grep(
                                        "^CHRONOMEMBERKEYCODES=",
                                        this.header,
                                        value = TRUE,
                                        ignore.case = TRUE
                                )
                        )
                if (length(this.member) == 1) {
                        chrono_members[i] <- this.member
                }

                ## NEW: get latitude
                this.latitude <- sub(
                        "LATITUDE=",
                        "",
                        ignore.case = TRUE,
                        x = grep(
                                "^LATITUDE=",
                                this.header,
                                value = TRUE,
                                ignore.case = TRUE
                        )
                )
                if (length(this.latitude) == 1) {
                        latitude[i] <- this.latitude
                }
                # NEW: get longitude
                this.longitude <-
                        sub(
                                "LONGITUDE=",
                                "",
                                ignore.case = TRUE,
                                x = grep(
                                        "^LONGITUDE=",
                                        this.header,
                                        value = TRUE,
                                        ignore.case = TRUE
                                )
                        )
                if (length(this.longitude) == 1) {
                        longitude[i] <- this.longitude
                }
        }

        ## calculate time span for data.frame
        min.year <- min(start.years)
        r.off <- min.year - 1
        max.year <- max(end.years)
        span <- min.year:max.year
        dendro.matrix <- matrix(NA, ncol = n, nrow = length(span))
        colnames(dendro.matrix) <- keycodes
        rownames(dendro.matrix) <- span

        ## get rid of comments (if any)
        strip.comment <- function(x) {
                strsplit(x, ";")[[1]][1]
        }
        # loop through data blocks
        for (i in seq_len(n)) {
                portion.start <- header.end[i] + 1
                portion.end <- data.end[i] - 1
                n.expected <- end.years[i] - start.years[i] + 1
                if (portion.end < portion.start) {
                        stop(
                                gettextf("in series %s: ", keycodes[i], domain = "R-dplR"),
                                gettextf(
                                        "too few values (expected %d, got %d)",
                                        n.expected,
                                        0,
                                        domain = "R-dplR"
                                ),
                                domain = NA
                        )
                }
                portion <- inp[portion.start:portion.end]

                ## data is in column format (with comments)
                if (nchar(portion[1]) < 60 ||
                    grepl(";", portion[1], fixed = TRUE)) {
                        data <- as.numeric(vapply(portion, strip.comment, "foo"))

                        ## data is in block format
                } else if (data_type[i] == "Single" ||
                           data_type[i] == "Tree") {
                        data <- numeric(length(portion) * 10)
                        for (j in seq_along(portion)) {
                                row.fwf <- substring(
                                        portion[j],
                                        seq(
                                                from = 1,
                                                by = 6,
                                                length = 10
                                        ),
                                        seq(
                                                from = 6,
                                                by = 6,
                                                length = 10
                                        )
                                )
                                row.numeric <- as.numeric(row.fwf)
                                data[(j * 10 - 9):(j * 10)] <- row.numeric
                        }
                        ## Remove trailing zeros
                        zeros <- which(data == 0)
                        if (length(zeros) > 0) {
                                nonzeros <- setdiff(zeros[1]:length(data), zeros)
                                if (length(nonzeros) > 0) {
                                        zeros <- zeros[zeros > max(nonzeros)]
                                        if (length(zeros) > 0) {
                                                data <- data[-zeros]
                                        }
                                } else {
                                        data <- data[-zeros]
                                }
                        }
                }

                ## NEW: data in block format (Quadro) => chronology
                else if (data_type[i] == "Quadro") {
                        data <- numeric(length(portion) * 4)
                        for (j in seq_along(portion)) {
                                row.fwf <- substring(
                                        portion[j],
                                        seq(
                                                from = 1,
                                                by = 6,
                                                length = 16
                                        ),
                                        seq(
                                                from = 6,
                                                by = 6,
                                                length = 16
                                        )
                                )
                                row.numeric <- as.numeric(row.fwf)
                                row.numeric <- row.numeric[c(1, 5, 9, 13)]
                                data[(j * 4 - 3):(j * 4)] <- row.numeric
                        }
                        ## Remove trailing zeros
                        zeros <- which(data == 0)
                        if (length(zeros) > 0) {
                                nonzeros <- setdiff(zeros[1]:length(data), zeros)
                                if (length(nonzeros) > 0) {
                                        zeros <- zeros[zeros > max(nonzeros)]
                                        if (length(zeros) > 0) {
                                                data <- data[-zeros]
                                        }
                                }
                                else {
                                        data <- data[-zeros]
                                }
                        }
                }

                ## NEW: data in block format (Double) => half-chrono
                else if (data_type[i] == "Double" ||
                         data_type[i] == "HalfChrono") {
                        data <- numeric(length(portion) * 5)
                        for (j in seq_along(portion)) {
                                row.fwf <- substring(
                                        portion[j],
                                        seq(
                                                from = 1,
                                                by = 6,
                                                length = 10
                                        ),
                                        seq(
                                                from = 6,
                                                by = 6,
                                                length = 10
                                        )
                                )
                                row.numeric <- as.numeric(row.fwf)
                                row.numeric <- row.numeric[c(1, 3, 5, 7, 9)]
                                data[(j * 5 - 4):(j * 5)] <- row.numeric
                        }
                        ## Remove trailing zeros
                        zeros <- which(data == 0)
                        if (length(zeros) > 0) {
                                nonzeros <- setdiff(zeros[1]:length(data), zeros)
                                if (length(nonzeros) > 0) {
                                        zeros <- zeros[zeros > max(nonzeros)]
                                        if (length(zeros) > 0) {
                                                data <- data[-zeros]
                                        }
                                }
                                else {
                                        data <- data[-zeros]
                                }
                        }
                }

                data <- data * multipliers[i] / divisors[i]
                n.true <- length(data)
                if (n.true == n.expected) {
                        ## write data into matrix
                        dendro.matrix[(start.years[i] - r.off):(end.years[i] - r.off), i] <-
                                data
                } else if (n.true < n.expected) {
                        stop(
                                gettextf("in series %s: ", keycodes[i], domain = "R-dplR"),
                                gettextf(
                                        "too few values (expected %d, got %d)",
                                        n.expected,
                                        n.true,
                                        domain = "R-dplR"
                                ),
                                domain = NA
                        )
                } else if (all(is.na(data[(n.expected + 1):n.true]))) {
                        dendro.matrix[(start.years[i] - r.off):(end.years[i] - r.off), i] <-
                                data[seq_len(n.expected)]
                } else {
                        stop(
                                gettextf("in series %s: ", keycodes[i], domain = "R-dplR"),
                                gettextf(
                                        "too many values (expected %d, got %d)",
                                        n.expected,
                                        n.true,
                                        domain = "R-dplR"
                                ),
                                domain = NA
                        )
                }
        }
        ## NEW: verbose == TRUE
        if (verbose) {
                cat(sprintf(
                        ngettext(
                                n,
                                "There is %d series\n",
                                "There are %d series\n",
                                domain = "R-dplR"
                        ),
                        n
                ))
                start.years.char <-
                        format(start.years,
                               scientific = FALSE,
                               trim = TRUE)
                end.years.char <-
                        format(end.years,
                               scientific = FALSE,
                               trim = TRUE)
                seq.series.char <-
                        format(seq_len(n),
                               scientific = FALSE,
                               trim = TRUE)
                ## NEW: column titles + 'data_type' added to message
                cat(
                        paste0(
                                format("no.", width = 5),
                                "\t",
                                format("series", width = 10),
                                "\t",
                                format("type", width = 5),
                                "\t",
                                format(
                                        "first",
                                        width = 5,
                                        justify = "right"
                                ),
                                "\t",
                                format("last", width = 5, justify = "right"),
                                "\t",
                                format("prec", width = 5),
                                "\n"
                        )
                )
                cat(
                        paste0(
                                format(seq.series.char, width = 5),
                                "\t",
                                format(keycodes, width = 10),
                                "\t",
                                format(data_type, width = 5),
                                "\t",
                                ## NEW!
                                format(
                                        start.years.char,
                                        width = 5,
                                        justify = "right"
                                ),
                                "\t",
                                format(
                                        end.years.char,
                                        width = 5,
                                        justify = "right"
                                ),
                                "\t",
                                format(
                                        multipliers / divisors,
                                        scientific = FALSE,
                                        drop0trailing = TRUE
                                ),
                                "\n"
                        ),
                        sep = ""
                )
        }

        rwl <- as.data.frame(dendro.matrix) # return data.frame
        ## Create data.frame for site, tree, core, radius, stem disk IDs
        all.have.treeID <- !any(is.na(tree.vec))
        na.core <- is.na(core.vec)
        all.have.coreID <- !any(na.core)
        ## Try to find implicit core IDs (tree ID occurs once)
        if (all.have.treeID && !all.have.coreID) {
                foo <- table(tree.vec)
                measured.once <- as.numeric(names(foo)[foo == 1])
                core.vec[na.core & tree.vec %in% measured.once] <- 1
                all.have.coreID <- !any(is.na(core.vec))
        }
        ## Only include "ids" data.frame if all tree and core IDs are known
        if (all.have.treeID && all.have.coreID) {
                unique.sites <- unique(site.code)
                n.unique <- length(unique.sites)
                if (n.unique > 1) {
                        site.vec <- match(site.code, unique.sites)
                        tree.vec2 <- complex(n, NA_real_, NA_real_)
                        total.dupl <- 0
                        for (i in seq_len(n.unique)) {
                                idx <- which(site.vec == i)
                                ut <- unique(tree.vec[idx])
                                for (this.tree in ut) {
                                        idx2 <- idx[tree.vec[idx] == this.tree]
                                        if (this.tree %in% tree.vec2) {
                                                tree.vec2[idx2] <- 1i * (total.dupl + 1)
                                                total.dupl <- total.dupl + 1
                                        } else {
                                                tree.vec2[idx2] <- this.tree
                                        }
                                }
                        }
                        if (total.dupl > 0) {
                                dont.change <- Im(tree.vec2) == 0
                                existing <- unique(Re(tree.vec2[dont.change]))
                                max.existing <- max(existing)
                                if (max.existing < 1) {
                                        free.ids <- 1:total.dupl
                                } else {
                                        free.ids <- which(!(
                                                1:max.existing %in% existing
                                        ))
                                        free.ids <-
                                                c(
                                                        free.ids,
                                                        seq(
                                                                from = max.existing + 1,
                                                                by = 1,
                                                                length.out = max(
                                                                        0,
                                                                        total.dupl - length(free.ids)
                                                                )
                                                        )
                                                )
                                }
                                tree.vec2[!dont.change] <-
                                        free.ids[Im(tree.vec2[!dont.change])]
                        }
                        tree.vec2 <- Re(tree.vec2)
                        adf <-
                                data.frame(
                                        tree = tree.vec2,
                                        core = core.vec,
                                        site = site.vec,
                                        row.names = keycodes
                                )
                } else {
                        adf <- data.frame(
                                tree = tree.vec,
                                core = core.vec,
                                row.names = keycodes
                        )
                }
                if (any(!is.na(radius.vec))) {
                        adf <- cbind(adf, radius = radius.vec)
                }
                if (any(!is.na(stemdisk.vec))) {
                        adf <- cbind(adf, stemDisk = stemdisk.vec)
                }
                attr(rwl, "ids") <- adf
                cat(
                        gettext(
                                'Tree and core IDs were found. See attribute "ids".\n',
                                domain = "R-dplR"
                        )
                )
        }
        ## Include pith offset data.frame if some pith offsets are known
        na.po <- is.na(pith.offset)
        if (any(!na.po)) {
                attr(rwl, "po") <-
                        data.frame(series = keycodes, pith.offset = pith.offset)
                if (any(na.po) && isTRUE(verbose)) {
                        cat(
                                gettext(
                                        'Pith offsets were found (some missing values). See attribute "po".\n',
                                        domain = "R-dplR"
                                )
                        )
                } else if (isTRUE(verbose)) {
                        cat(
                                gettext(
                                        'Pith offsets were found (no missing values). See attribute "po".\n',
                                        domain = "R-dplR"
                                )
                        )
                }
        }

        ## NEW: add header fields as attributes
        attr(rwl, "series") <- keycodes
        attr(rwl, "data_type") <- data_type
        attr(rwl, "chrono_members") <- chrono_members
        attr(rwl, "species") <- species
        attr(rwl, "first") <- start.years
        attr(rwl, "last") <- end.years
        attr(rwl, "length") <- lengths
        attr(rwl, "n_sapwood") <- sapwoodrings
        attr(rwl, "n_sapwood_chr") <- sapwoodrings_chr
        attr(rwl, "unmeasured_rings") <- unmeasured_rings
        attr(rwl, "invalid_rings") <- invalid_rings
        attr(rwl, "status") <- status
        attr(rwl, "waneyedge") <- waneyedge
        attr(rwl, "bark") <- bark
        attr(rwl, "pith") <- pith
        attr(rwl, "pith_offset") <- pith.offset
        attr(rwl, "pith_offset_delta") <- pith_offset_delta
        attr(rwl, "comments") <- comments
        attr(rwl, "project") <- project
        attr(rwl, "location") <- location
        attr(rwl, "town") <- town
        attr(rwl, "zip") <- town_zip
        attr(rwl, "street") <- street
        attr(rwl, "sampling_date") <- sampling_date
        attr(rwl, "measuring_date") <- measuring_date
        attr(rwl, "personal_id") <- personal_id
        attr(rwl, "client_id") <- client_id
        attr(rwl, "longitude") <- longitude
        attr(rwl, "latitude") <- latitude

        ## add class
        class(rwl) <- c("rwl", "data.frame")

        if (header) {
                get_header(rwl)
        } else {
                return(rwl)
        }
}
