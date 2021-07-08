source("./R/generic_scraper.R")
source("./R/utilities.R")

texas_check_date <- function(x, date = Sys.Date()){
    NULL
}

texas_pull <- function(x){
    str_c(
        "https://maps.tdem.texas.gov/koop/googlesheets/tdcjCovidCounts/TDCJ",
        "!A1:W/FeatureServer/0/query?f=json&where=",
        "(Unit%20%3C%3E%20%27Deceased%27)%20AND%20",
        "(Unit%20%3C%3E%20%27Mass%20Testing%27)%20AND%20",
        "(Unit%20%3C%3E%20%27No%20Longer%20in%20Custody%27)%20AND%20",
        "(Unit%20%3C%3E%20%27Non%20Unit%27)%20AND%20",
        "(Unit%20%3C%3E%20%27Total%27)%20AND%20",
        "(Unit%20%3C%3E%20%27Unknown%27)&returnGeometry=false",
        "&spatialRel=esriSpatialRelIntersects&outFields=*&",
        "orderByFields=Unit%20asc&outSR=102100&resultOffset=0&",
        "resultRecordCount=110") %>%
        jsonlite::read_json(simplifyVector = TRUE)
}

texas_restruct <- function(x){
    as_tibble(x$features$attributes)
}

texas_extract <- function(x){
    x %>%
        mutate(Residents.Deaths = Offender_Deceased__Presumed_COVID + 
                   Offender_Deceased_Confirmed_COVID) %>%
        mutate(Residents.Quarantine = 
                   Medical_Restriction + Medical_Isolation) %>%
        select(
            Name = Unit,
            Residents.Tadmin = Offender_Total_Tests,
            Residents.Confirmed = Offender_Total_Positive_Cases,
            Residents.Quarantine,
            Residents.Recovered = Offender_Recovered,
            Residents.Active = Offender_Active_Cases,
            Staff.Confirmed = Employee_Total_Positive_Cases,
            Staff.Recovered = Employee_Recovered
        )
}

#' Scraper class for general Texas COVID data
#' 
#' @name texas_scraper
#' @description Data pulled from Texas API with minimal cleaning required.
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

texas_scraper <- R6Class(
    "texas_scraper",
    inherit = generic_scraper,
    public = list(
        log = NULL,
        initialize = function(
            log,
            url = "https://www.tdcj.texas.gov/covid-19/presumed.html",
            id = "texas",
            type = "json",
            state = "TX",
            jurisdiction = "state",
            check_date = NULL,
            # pull the JSON data directly from the API
            pull_func = texas_pull,
            # restructuring the data means pulling out the data portion of the json
            restruct_func = texas_restruct,
            # Rename the columns to appropriate database names
            extract_func = texas_extract){
            super$initialize(
                url = url, id = id, pull_func = pull_func, type = type,
                restruct_func = restruct_func, extract_func = extract_func,
                log = log, state = state, jurisdiction = jurisdiction,
                check_date = check_date)
        }
    )
)

if(sys.nframe() == 0){
    texas <- texas_scraper$new(log=FALSE)
    texas$run_check_date()
    texas$raw_data
    texas$pull_raw()
    texas$raw_data
    texas$save_raw()
    texas$restruct_raw()
    texas$restruct_data
    texas$extract_from_raw()
    texas$extract_data
    texas$validate_extract()
    texas$save_extract()
}

