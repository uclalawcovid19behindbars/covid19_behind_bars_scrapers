source("./R/generic_scraper.R")
source("./R/utilities.R")

texas_statewide_pull <- function(x){
    str_c(
        "https://maps.tdem.texas.gov/koop/googlesheets/tdcjCovidCounts/",
        "TDCJ!A1:W/FeatureServer/0/query?f=json&where=(",
        "(Unit%20%3D%20%27Deceased%27)%20OR%20",
        "(Unit%20%3D%20%27Mass%20Testing%27)%20OR%20",
        "(Unit%20%3D%20%27No%20Longer%20in%20Custody%27)%20OR%20",
        "(Unit%20%3D%20%27Non%20Unit%27)%20OR%20",
        "(Unit%20%3D%20%27Total%27)%20OR%20",
        "(Unit%20%3D%20%27Unknown%27))",
        "%20AND%20(Unit%3D%27Total%27)&",
        "returnGeometry=false&spatialRel=esriSpatialRelIntersects&outFields=*"
        ) %>%
        jsonlite::read_json(simplifyVector = TRUE)
        
}

texas_statewide_restruct <- function(x){
    as_tibble(x$features$attributes)
}

texas_statewide_extract <- function(x){
    x %>%
        mutate(Unit = "State-Wide") %>%
        mutate(Residents.Deaths = Offender_Deceased__Presumed_COVID + 
                   Offender_Deceased_Confirmed_COVID) %>%
        select(
            Name = Unit,
            Residents.Deaths,
            Staff.Tested = Employee_Total_Tests,
            Staff.Deaths = Employee_Deceased__COVID
        )
}

#' Scraper class for general texas_statewide COVID data
#' 
#' @name texas_statewide_scraper
#' @description Data pulled from texas_statewide API with minimal cleaning required.
#' Description of variables can be found here
#' https://www.tdcj.texas.gov/covid-19/definitions.html
#' \describe{
#'   \item{Unit}{}
#'   \item{Unique_ID}{}
#'   \item{Address}{}
#'   \item{Offender_Total_Tests}{}
#'   \item{Offender_Total_Positive_Cases}{}
#'   \item{Offender_Active_Cases}{}
#'   \item{Offender_Recovered}{}
#'   \item{Offender_Deceased__Presumed_COVID}{}
#'   \item{Offender_Deceased__Pending}{}
#'   \item{Offender_Deceased_Confirmed_COVID}{}
#'   \item{Employee_Total_Tests}{}
#'   \item{Employee_Total_Positive_Cases}{}
#'   \item{Employee_Active_Cases}{}
#'   \item{Employee_Recovered}{}
#'   \item{Employee_Deceased__COVID}{}
#'   \item{Employee_Deceased__Pending}{}
#'   \item{Employee_DeceasedNot_COVID}{}
#'   \item{Medical_Restriction}{}
#'   \item{Medical_Isolation}{}
#'   \item{Units_on_Precautionary_Lockdown}{}
#'   \item{Last_Updated}{}
#' }

texas_statewide_scraper <- R6Class(
    "texas_statewide_scraper",
    inherit = generic_scraper,
    public = list(
        log = NULL,
        initialize = function(
            log,
            url = "https://www.tdcj.texas.gov/covid-19/index.html",
            id = "texas_statewide",
            type = "json",
            state = "TX",
            jurisdiction = "state",
            check_date = NULL,
            # pull the JSON data directly from the API
            pull_func = texas_statewide_pull,
            # restructuring the data means pulling out the data portion of the json
            restruct_func = texas_statewide_restruct,
            # Rename the columns to appropriate database names
            extract_func = texas_statewide_extract){
            super$initialize(
                url = url, id = id, pull_func = pull_func, type = type,
                restruct_func = restruct_func, extract_func = extract_func,
                log = log, state = state, jurisdiction = jurisdiction,
                check_date = check_date)
        }
    )
)

if(sys.nframe() == 0){
    texas_statewide <- texas_statewide_scraper$new(log=FALSE)
    texas_statewide$run_check_date()
    texas_statewide$raw_data
    texas_statewide$pull_raw()
    texas_statewide$raw_data
    texas_statewide$save_raw()
    texas_statewide$restruct_raw()
    texas_statewide$restruct_data
    texas_statewide$extract_from_raw()
    texas_statewide$extract_data
    texas_statewide$validate_extract()
    texas_statewide$save_extract()
}

