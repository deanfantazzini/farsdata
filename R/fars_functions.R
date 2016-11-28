#' Function to read the fars csv file data
#'
#' This function checks whether the given filename is correct and then reads the fars csv file data
#'
#' @param filename is a string specifying the address where to read the fars cdv (zipped) data
#' @return data are the loaded fars data in dataframe format
#'
#' @details
#' This function first checks whether the filename is correct: if this is not correct the execution of the function is
#' stopped. Then the csv data is loaded using the read_csv function from the readr package (any messages are suppressed)
#' and then transformed into a dataframe using the tbl_df function from the dplyr package. The dataframe has the following columns:
#' \describe{
#' \item{STATE}{State number}
#' \item{ST_CASE}{State/Case Number}
#' \item{VE_TOTAL}{Number of Vehicles in Crash}
#' \item{VE_FORMS}{Vehicle Forms Submitted}
#' \item{PVH_INVL}{Number of Parked/Working Vehicles in the Crash}
#' \item{PEDS}{Number of Persons Not in Motor Vehicles}
#' \item{PERNOTMVIT}{Number of Persons Not in Motor Vehicles in Transport}
#' \item{PERMVIT}{Number of Persons in Motor Vehicles In-Transport}
#' \item{PERSONS}{Number of Forms Submitted for Persons in Motor Vehicles}
#' \item{COUNTY}{County location}
#' \item{CITY}{City location}
#' \item{DAY}{Day of the Month of the Crash}
#' \item{MONTH}{Month of Crash}
#' \item{YEAR}{Year of Crash}
#' \item{DAY_WEEK}{Day of Week of crash}
#' \item{HOUR}{Hour of Crash}
#' \item{MINUTE}{Minute of Crash}
#' \item{NHS}{Did this crash occurr on a trafficway that is part of the National Highway System?}
#' \item{ROAD_FNC}{Functional classification of the trafficway on which the crash occurred}
#' \item{ROUTE}{Route signing of the trafficway on which the crash occurred}
#' \item{TWAY_ID}{This data element records the trafficway on which the crash occurred (1982-Later)}
#' \item{TWAY_ID2}{Trafficway on which the crash occurred (2004-Later)}
#' \item{MILEPT}{Milepoint nearest to the location where the crash occurred}
#' \item{LATITUDE}{Location of the crash using Global Position coordinates: latitude}
#' \item{LONGITUD}{Location of the crash using Global Position coordinates:: longitude}
#' \item{SP_JUR}{Does the location on the trafficway where the crash occurred qualify as a Special Jurisdiction?}
#' \item{HARM_EV}{First injury or damage producing event of the crash}
#' \item{MAN_COLL}{Orientation of two motor vehicles in-transport when they are involved in the "First Harmful Event" of a collision crash}
#' \item{RELJCT1}{Crash's location with respect to presence in an interchange area}
#' \item{RELJCT2}{Crash's location with respect to presence in or proximity to components typically in junction or interchange areas}
#' \item{TYP_INT}{Type of Intersection}
#' \item{WRK_ZONE}{Crash  within the boundaries of a work zone}
#' \item{REL_ROAD}{Location of the crash as it relates to its position within or outside the trafficway}
#' \item{LGT_COND}{Light Condition}
#' \item{WEATHER1}{Atmospheric Conditions (2007-Later)}
#' \item{WEATHER2}{Atmospheric Conditions (2007-Later)}
#' \item{WEATHER}{Atmospheric Conditions (1975-2006)}
#' \item{SCH_BUS}{School Bus Related to crash}
#' \item{RAIL}{Did the crash occurr in or near a rail grade crossing}
#' \item{NOT_HOUR}{Hour that emergency medical service was notified}
#' \item{NOT_MIN}{Minutes after the hour that emergency medical service was notified}
#' \item{ARR_HOUR}{Hour of Arrival at Scene}
#' \item{ARR_MIN}{Minute of Arrival at Scene}
#' \item{HOSP_HR}{Hour of EMS Arrival at Hospital}
#' \item{HOSP_MN}{Minute of EMS Arrival at Hospital}
#' \item{CF1}{factors related to the crash expressed by the investigating officer}
#' \item{CF2}{factors related to the crash expressed by the investigating officer}
#' \item{CF3}{factors related to the crash expressed by the investigating officer}
#' \item{FATALS}{number of fatally injured persons}
#' \item{DRUNK_DR}{number of drunk drivers involved in the fatal crash}
#' }
#'
#' @export
#' @importFrom readr read_csv
#' @importFrom dplyr tbl_df
#'
#' @examples
#'  data_file<-system.file("extdata","accident_2013.csv.bz2", package = "farsdata")
#'  setwd(dirname(data_file))
#'  file="accident_2013.csv.bz2"
#'  dat<-fars_read(file)
#'
#'  \dontrun{
#'  file="accident_2011.csv.bz2"
#'  dat<-fars_read(file) # error
#'  }

fars_read <- function(filename) {
        if(!file.exists(filename))
                stop("file '", filename, "' does not exist")
        data <- suppressMessages({
                readr::read_csv(filename, progress = FALSE)
        })
        dplyr::tbl_df(data)
}


#' Create the filename for the fars csv file data
#'
#' This function creates the filename for the fars csv (zipped) file data
#'
#' @param year is a numeric scalar specifying which yearly data
#' (regarding fatal injuries suffered in motor vehicle traffic crashes) have to be loaded
#' @return a string specifying the address where to read the fars data
#' @details
#' This function  creates the filename for the specific yearly fars csv (zipped) file data.
#' There are 3 possible years: 2013, 2014, 2015.
#'
#' @export
#'
#' @examples
#'  file=make_filename(2013)
#'  file
make_filename <- function(year) {
        year <- as.integer(year)
        sprintf("accident_%d.csv.bz2", year)
}


#' Function to read the fars csv file data over several years extracting the months of the crashes
#'
#' This function checks whether the given years are correct,
#' then reads the fars csv file data and extracts the months of the crashes.
#' If the years are invalid it sends a warning
#'
#' @param years is a numeric vector specifying which yearly data
#' (regarding fatal injuries suffered in motor vehicle traffic crashes)
#' have to be loaded
#' @return a list containing the loaded fars data in dataframe format for each year
#' @details
#' This function checks whether the given years are correct,
#' then reads the fars csv file data and extracts the months of the crashes.
#' If the years are invalid it sends
#' a warning. Remember that only 3 years are available: 2013,2014,2015
#'
#' @export
#' @importFrom dplyr select
#' @importFrom dplyr mutate
#'
#' @examples
#'
#'  data_file<-system.file("extdata","accident_2013.csv.bz2", package = "farsdata")
#'  setwd(dirname(data_file))
#'  dat=fars_read_years(c(2013,2014))
#'  dat
#'
fars_read_years <- function(years) {
        MONTH<-NULL
        lapply(years, function(year) {
                file <- make_filename(year)
                tryCatch({
                        dat <- fars_read(file)
                        dplyr::mutate(dat, year = year) %>%
                                dplyr::select(MONTH, year)
                }, error = function(e) {
                        warning("invalid year: ", year)
                        return(NULL)
                })
        })
}

#' Function to read the fars csv file data over several years,
#' summaryzing the crashes for each month of the given years
#'
#' This function checks whether the given years are correct,
#' then reads the fars csv file data, extracts the months of the crashes,
#' compute the number of crashes for each month, and then prepare a dataframe
#' where the rows are the months, while the columns are the year
#'
#' @param years is a numeric vector specifying which yearly data
#' (regarding fatal injuries suffered in motor vehicle traffic crashes)
#' have to be loaded
#' @return a dataframe containing the number of crashes for each month and each year
#' @details
#' This function loadsthe fars data and summarize then by preparing a dataframe
#' containing the number of crashes for each month (rows) for each given year (columns)
#'
#' @export
#' @importFrom dplyr bind_rows
#' @importFrom dplyr group_by
#' @importFrom dplyr summarize
#' @importFrom tidyr spread
#' @importFrom tidyr %>%
#'
#' @examples
#'
#'  data_file<-system.file("extdata","accident_2013.csv.bz2", package = "farsdata")
#'  setwd(dirname(data_file))
#'  dat=fars_summarize_years(c(2013,2014))
#'  dat
#'
fars_summarize_years <- function(years) {
  year<-NULL
  MONTH<-NULL
  n<-NULL
  dat_list <- fars_read_years(years)
  dplyr::bind_rows(dat_list) %>%
    dplyr::group_by(year, MONTH) %>%
    dplyr::summarize(n = n()) %>%
    tidyr::spread(year, n)
}

#' Function to plot the crashes' locations on a US state map
#'
#' This function loads the fars data, checks that whether US state number is valid,
#' checks whether there are crashes to plot, checks whether the latitude and longitudes are
#' correct, and finally plot the locations of the accidents
#' using the US state map where they took place.
#'
#' @param year is a numeric scalar specifying which yearly data
#' (regarding fatal injuries suffered in motor vehicle traffic crashes)
#' have to be loaded
#' @param state.num is an numerical scalar indicating which US state has to be considered
#' @return NULL. A plot of the US state with the locations of the accidents is printed but not returned.
#' @details
#' This function loads the fars data, checks that whether US state number is valid (otherwise it stops),
#' checks whether there are crashes to plot (otherwise it sends the message
#' "no accidents to plot"), checks whether the latitude and longitudes are
#' correct (otherwise it inserts na), and finally plots the locations of the accidents
#' using the US state map where they took place.
#'
#' @export
#' @importFrom dplyr filter
#' @importFrom maps map
#' @importFrom graphics points
#'
#' @examples
#'
#'  data_file<-system.file("extdata","accident_2013.csv.bz2", package = "farsdata")
#'  setwd(dirname(data_file))
#'  library(maps)
#'  fars_map_state(48, 2014)
#'
fars_map_state <- function(state.num, year) {
        STATE<-NULL
        filename <- make_filename(year)
        data <- fars_read(filename)
        state.num <- as.integer(state.num)

        if(!(state.num %in% unique(data$STATE)))
                stop("invalid STATE number: ", state.num)
        data.sub <- dplyr::filter(data, STATE == state.num)
        if(nrow(data.sub) == 0L) {
                message("no accidents to plot")
                return(invisible(NULL))
        }
        is.na(data.sub$LONGITUD) <- data.sub$LONGITUD > 900
        is.na(data.sub$LATITUDE) <- data.sub$LATITUDE > 90
        with(data.sub, {
                maps::map("state", ylim = range(LATITUDE, na.rm = TRUE),
                          xlim = range(LONGITUD, na.rm = TRUE))
                graphics::points(LONGITUD, LATITUDE, pch = 46)
        })
}
