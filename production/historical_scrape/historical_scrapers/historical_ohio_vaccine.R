source("./R/generic_scraper.R")
source("./R/utilities.R")

historical_ohio_vac_pull <- function(x, date = NULL, file = NULL){
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

#' Scraper class for Ohio vaccine data
#' 
#' @name historical_ohio_vac_scraper
#' @description This scraper pulls data from a manual csv file. We're receiving 
#' vaccine data from the ODRC once/week via email. We're getting statewide totals
#' for residents and staff. 
#' \describe{
#'   \item{Inmates}{Number of doses administered to incarcerated residents}
#'   \item{Staff}{Number of doses administered to staff}
#' }

historical_ohio_vac_scraper <- R6Class(
    "historical_ohio_vac_scraper",
    inherit = generic_scraper,
    public = list(
        log = NULL,
        initialize = function(
            log,
            url = "Ohio Department of Rehabilitation and Correction",
            id = "historical_ohio_vaccine",
            type = "manual",
            state = "OH",
            jurisdiction = "state",
            check_date = NULL,
            pull_func = historical_ohio_vac_pull,
            restruct_func = historical_ohio_vac_restruct,
            extract_func = historical_ohio_vac_extract){
            super$initialize(
                url = url, id = id, pull_func = pull_func, type = type,
                restruct_func = restruct_func, extract_func = extract_func,
                log = log, state = state, jurisdiction  = jurisdiction,
                check_date = check_date)
        }
    )
)

if(sys.nframe() == 0){
    historical_ohio_vaccine <- historical_ohio_vac_scraper$new(log=TRUE)
    historical_ohio_vaccine$reset_date("SET_DATE_HERE")
    historical_ohio_vaccine$raw_data
    historical_ohio_vaccine$pull_raw(date = scraper$date, .dated_pull = TRUE)
    historical_ohio_vaccine$raw_data
    historical_ohio_vaccine$save_raw()
    historical_ohio_vaccine$restruct_raw(date = scraper$date)
    historical_ohio_vaccine$restruct_data
    historical_ohio_vaccine$extract_from_raw(date = scraper$date)
    historical_ohio_vaccine$extract_data
    historical_ohio_vaccine$validate_extract()
    historical_ohio_vaccine$save_extract()
}