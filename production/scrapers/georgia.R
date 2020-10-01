source("./R/generic_scraper.R")

#' Scraper class for general Georgia COVID data
#' 
#' @name georgia_scraper
#' @description This will be a description of Georgia data and what the scraper
#' does
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

georgia_scraper <- R6Class(
    "georgia_scraper",
    inherit = generic_scraper,
    public = list(
        log = NULL,
        initialize = function(
            log,
            url = stringr::str_c(
                "https://services5.arcgis.com/mBtYHKRd2hqJxboF/arcgis/rest/services/",
                "COVID19Statewide/FeatureServer/0/query?f=json&where=1%3D1&",
                "returnGeometry=false&spatialRel=esriSpatialRelIntersects&outFields=*&",
                "resultOffset=0&resultRecordCount=1000&resultType=standard",
                "&cacheHint=true"),
            id = "georgia",
            type = "json",
            state = "GA",
            # pull the JSON data directly from the API
            pull_func = function(...) jsonlite::read_json(..., simplifyVector = TRUE),
            # restructuring the data means pulling out the data portion of the json
            restruct_func = function(x) as_tibble(x$features$attributes),
            # Rename the columns to appropriate database names
            extract_func = function(x){
                x %>% 
                    select(
                        Name = Facility_Name, Staff.Confirmed = Staff_Confirmed, 
                        Staff.Recovered = Staff_Recovered, Staff.Deaths = Staff_Deaths, 
                        Residents.Confirmed = Inmate_Confirmed, 
                        Residents.Recovered = Inmate_Recovered, 
                        Residents.Deaths = Inmate_Deaths)}){
            super$initialize(
                url = url, id = id, pull_func = pull_func, type = type,
                restruct_func = restruct_func, extract_func = extract_func,
                log = log, state = state)
        }
    )
)

if(sys.nframe() == 0){
    georgia <- georgia_scraper$new(log=FALSE)
    georgia$raw_data
    georgia$pull_raw()
    georgia$raw_data
    georgia$restruct_raw()
    georgia$restruct_data
    georgia$extract_from_raw()
    georgia$extract_data
}

