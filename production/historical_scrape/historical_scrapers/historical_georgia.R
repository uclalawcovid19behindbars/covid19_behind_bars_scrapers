source("./R/generic_scraper.R")

georgia_pull <- function(x, file, date = NULL){
    # Get API endpoint from permacc 
    # Example of file url from 2021-01-27: "lju7-43hn/20210127215032mp_"
    stringr::str_c(
        "https://wr.perma-archives.org/public/", 
        file, 
        "/https://services5.arcgis.com/mBtYHKRd2hqJxboF/arcgis/rest/services/", 
        "COVID19StatewideV2/FeatureServer/0/query?f=json&where=1%3D1&", 
        "returnGeometry=false&spatialRel=esriSpatialRelIntersects&outFields=*&",
        "resultOffset=0&resultRecordCount=1000&resultType=standard&cacheHint=true") %>%
        jsonlite::read_json(simplifyVector = TRUE)
}

#' Scraper class for historical Georgia COVID data
#' Uses permacc saves to reconstruct the API endpoint 
#' 
#' @name georgia_scraper
#' @description Georgia data comes from an api which needs minimal cleaning.
#' \describe{
#'   \item{Facility_Name}{The faciilty name.}
#'   \item{Latitude}{Lat.}
#'   \item{Longitude}{Long.}
#'   \item{Address}{Address of the facility.}
#'   \item{City}{City location of facility.}
#'   \item{State}{State location of facility (should always be Georgia).}
#'   \item{Zip}{Zip code of facility.}
#'   \item{Staff Confirmed}{Cumulative staff confirmed.}
#'   \item{Staff Deaths}{Cumulative staff deaths.}
#'   \item{Staff Recovered}{Cumulative staff recovered.}
#'   \item{Inmate Confirmed}{Cumulative resident confirmed.}
#'   \item{Inmate Deaths}{Cumulative resident deaths.}
#'   \item{Inmate Recovered}{Cumulative resident recovered.}
#'   \item{Private Prison}{Y/N is this a private prison.}
#'   \item{Confirmed Display}{Display format for vizualizations.}
#'   \item{Death Dispaly}{Display format for vizualizations.}
#'   \item{Recovered Display}{Display format for vizualizations.}
#'   \item{FID}{Georgia internal facility id.}
#' }

historical_georgia_scraper <- R6Class(
    "historical_georgia_scraper",
    inherit = generic_scraper,
    public = list(
        log = NULL,
        initialize = function(
            log,
            url = "http://www.dcor.state.ga.us/content/CVD_Dashboard",
            # Uses same ID as main Georgia scraper! 
            id = "georgia",
            type = "json",
            state = "GA",
            jurisdiction = "state",
            # pull the JSON data directly from the API
            pull_func = georgia_pull,
            # restructuring the data means pulling out the data portion of the json
            restruct_func = function(x, date = NULL) as_tibble(x$features$attributes),
            # Rename the columns to appropriate database names
            extract_func = function(x, date = NULL){
                x %>% 
                    select(
                        Name = Facility_Name, 
                        Staff.Confirmed = Staff_Confirmed, 
                        Staff.Recovered = Staff_Recovered, 
                        Staff.Deaths = Staff_Deaths, 
                        Residents.Confirmed = Inmate_Confirmed, 
                        Residents.Recovered = Inmate_Recovered, 
                        Residents.Deaths = Inmate_Deaths, 
                        )}){
            super$initialize(
                url = url, id = id, pull_func = pull_func, type = type,
                restruct_func = restruct_func, extract_func = extract_func,
                log = log, state = state, jurisdiction = jurisdiction)
        }
    )
)

if(sys.nframe() == 0){
    historical_georgia <- historical_georgia_scraper$new(log=TRUE)
    historical_georgia$reset_date("DATE")
    historical_georgia$raw_data
    historical_georgia$pull_raw(file, .dated_pull = TRUE)
    historical_georgia$raw_data
    historical_georgia$save_raw()
    historical_georgia$restruct_raw()
    historical_georgia$restruct_data
    historical_georgia$extract_from_raw()
    historical_georgia$extract_data
    historical_georgia$validate_extract()
    historical_georgia$save_extract()
}
