source("./R/generic_scraper.R")
source("./R/utilities.R")

historical_ohio_vac_pull <- function(x, date = NULL){
    get_latest_manual("Ohio_Vaccine")
}

historical_ohio_vac_restruct <- function(x, date = NULL){
    x %>%
        rename(
            Residents.Vadmin = "Inmates",
            Staff.Vadmin = "Staff") 
}

historical_ohio_vac_extract <- function(x, date){
    x %>%
        mutate(Name = "STATEWIDE") %>% 
        filter(Date == date) %>% 
        select(-Date)
}

#' Scraper class for ice COVID data
#' 
#' @name historical_ice_scraper
#' @description This scraper pulls html data from the ice page which reports on
#' the variables listed below. Unlike all other scrapers their are total column
#' values that we want to keep which do not corresponds to a state but rather
#' ICE as a whole. In addition we have found that facility names frequently
#' change and require updates to the facility spellings sheet. Dates for this
#' scraper should correspond to all dates which are present in the github sheet
#' columns where the data is being pulled up to when the main scraper started
#' pulling ice data 2021-01-12.
#' \describe{
#'   \item{Facility}{The facility name}
#'   \item{Confirmed cases currently under isolation}{Residents with active inf}
#'   \item{Detainee deaths}{Resident deaths}
#'   \item{Total confirmed COVID-19}{Residents cconfirmed cumulative}
#'   \item{Detained Population}{Current Resident Population}
#'   \item{Population Tested}{Tested}
#' }

historical_ohio_vac_scraper <- R6Class(
    "historical_ohio_vac_scraper",
    inherit = generic_scraper,
    public = list(
        log = NULL,
        initialize = function(
            log,
            url = "Ohio Department of Rehabilitation and Correction",
            id = "historical_ohio_vac",
            type = "manual",
            state = "OH",
            jurisdiction = "state",
            pull_func = historical_ohio_vac_pull,
            restruct_func = historical_ohio_vac_restruct,
            extract_func = historical_ohio_vac_extract){
            super$initialize(
                url = url, id = id, pull_func = pull_func, type = type,
                restruct_func = restruct_func, extract_func = extract_func,
                log = log, state = state, jurisdiction  = jurisdiction)
        }
    )
)

if(sys.nframe() == 0){
    historical_ohio_vac <- historical_ohio_vac_scraper$new(log=TRUE)
    historical_ohio_vac$reset_date("SET_DATE_HERE")
    historical_ohio_vac$raw_data
    historical_ohio_vac$pull_raw(date = scraper$date, .dated_pull = TRUE)
    historical_ohio_vac$raw_data
    historical_ohio_vac$save_raw()
    historical_ohio_vac$restruct_raw(date = scraper$date)
    historical_ohio_vac$restruct_data
    historical_ohio_vac$extract_from_raw(date = scraper$date)
    historical_ohio_vac$extract_data
    historical_ohio_vac$validate_extract()
    historical_ohio_vac$save_extract()
}