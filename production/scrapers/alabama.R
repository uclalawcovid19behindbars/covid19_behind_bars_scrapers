source("./R/generic_scraper.R")
source("./R/utilities.R")

alabama_pull <- function(x){
    # direct api pull
    "https://services7.arcgis.com/jF2q3LPxL7PETdYk/arcgis/rest/" %>%
        str_c(
            "services/esriMapData/FeatureServer/0/query?f=json&",
            "where=State%20%3D%20%27AL%27&returnGeometry=false&",
            "spatialRel=esriSpatialRelIntersects&outFields=*&",
            "orderByFields=ShortName%20asc&outSR=102100&resultOffset=0&",
            "resultRecordCount=100&resultType=standard&cacheHint=true")
        
    "https://services7.arcgis.com/jF2q3LPxL7PETdYk/arcgis/rest/services/esriMapData/FeatureServer/0/query?f=json&where=State%20%3D%20%27AL%27&returnGeometry=false&spatialRel=esriSpatialRelIntersects&outFields=*&orderByFields=ShortName%20asc&outSR=102100&resultOffset=0&resultRecordCount=100&resultType=standard&cacheHint=true" %>% 
        jsonlite::read_json(simplifyVector = TRUE)
}

alabama_restruct <- function(x){
    as_tibble(x$features$attributes)
}

alabama_extract <- function(x){
    x %>%
        select(
            Name = Facility,
            Residents.Tested = Inmate_Tested,
            Residents.Confirmed = Inmate_Positive,
            Residents.Pending = Inmate_Pending,
            Residents.Active = Inmate_Active_Cases, 
            Residents.Deaths = Inmate_Death,
            Residents.Recovered = Inmate_Recovered,
            Staff.Confirmed = Employee_Positive,
            Staff.Deaths = Employee_Death,
            Staff.Recovered = Employee_Recovered, 
            Staff.Active = Employee_Active_Cases, 
            Staff.Initiated = Employee_Vaccinations, 
            Residents.Initiated = Inmate_Vaccinations
        )
}

#' Scraper class for general Alabama COVID data
#' 
#' @name alabama_scraper
#' @description Alabama scraper comes directly from an api. Minimal cleaning
#' required. Note that Alabama does not report the number of tests administered
#' but rather the number of residents tested.
#' \describe{
#'   \item{Facility}{The facility name.}
#'   \item{Type}{The type of facility i.e. private, work-release, major, etc}
#'   \item{Street}{Address}
#'   \item{City}{City}
#'   \item{State}{State}
#'   \item{Zip}{Zip}
#'   \item{ShortName}{Facility alternative name}
#'   \item{Inmate_Tested}{The number of inmates tested rather than total tests administered.}
#'   \item{Inmate_Positive}{Cummulative Inmates Tested}
#'   \item{Inmate_Pending}{Inmate_Pending}
#'   \item{Inmate_Recovered}{Cummulative Inmates Recovered}
#'   \item{Inmate_Death}{Cumulative Deaths}
#'   \item{Employee_Positive}{Employees Positive}
#'   \item{Employee_Recovered}{Employees Recovered}
#'   \item{Employee_Death}{Employees Death}
#' }

alabama_scraper <- R6Class(
    "alabama_scraper",
    inherit = generic_scraper,
    public = list(
        log = NULL,
        initialize = function(
            log,
            url = "http://www.doc.alabama.gov/covid19news",
            id = "alabama",
            type = "json",
            state = "AL",
            jurisdiction = "state",
            # pull the JSON data directly from the API
            pull_func = alabama_pull,
            # restructuring the data means pulling out the data portion of the json
            restruct_func = alabama_restruct,
            # Rename the columns to appropriate database names
            extract_func = alabama_extract){
            super$initialize(
                url = url, id = id, pull_func = pull_func, type = type,
                restruct_func = restruct_func, extract_func = extract_func,
                log = log, state = state, jurisdiction = jurisdiction)
        }
    )
)

if(sys.nframe() == 0){
    alabama <- alabama_scraper$new(log=TRUE)
    alabama$raw_data
    alabama$pull_raw()
    alabama$raw_data
    alabama$save_raw()
    alabama$restruct_raw()
    alabama$restruct_data
    alabama$extract_from_raw()
    alabama$extract_data
    alabama$validate_extract()
    alabama$save_extract()
}

