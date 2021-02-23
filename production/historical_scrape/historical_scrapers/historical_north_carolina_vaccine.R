source("./R/generic_scraper.R")
source("./R/utilities.R")

historical_nc_vac_pull <- function(x, date = NULL, file = NULL){
    get_latest_manual("North Carolina_Vaccine")
}

historical_nc_vac_restruct <- function(x, date = NULL){
    x %>%
        rename(
            Residents.Initiated = "Offenders",
            Staff.Initiated = "Staff") 
}

historical_nc_vac_extract <- function(x, date){
    x %>%
        mutate(Name = "STATEWIDE") %>% 
        filter(Date == date) %>% 
        select(-Date)
}

#' Scraper class for North Carolina vaccine data
#' 
#' @name historical_nc_vac_scraper
#' @description This scraper pulls data from a manual csv file. We're receiving 
#' vaccine data from the Nevada DOC once/week via email. We're getting statewide 
#' totals for residents and staff. 
#' \describe{
#'   \item{Offenders}{Number of incarcerated residents who received first doses}
#'   \item{Staff}{Number of staff who received first doses}
#' }

historical_nc_vac_scraper <- R6Class(
    "historical_nc_vac_scraper",
    inherit = generic_scraper,
    public = list(
        log = NULL,
        initialize = function(
            log,
            url = "North Carolina Department of Public Safety",
            id = "historical_north_carolina_vaccine",
            type = "manual",
            state = "NC",
            jurisdiction = "state",
            pull_func = historical_nc_vac_pull,
            restruct_func = historical_nc_vac_restruct,
            extract_func = historical_nc_vac_extract){
            super$initialize(
                url = url, id = id, pull_func = pull_func, type = type,
                restruct_func = restruct_func, extract_func = extract_func,
                log = log, state = state, jurisdiction  = jurisdiction)
        }
    )
)

if(sys.nframe() == 0){
    historical_nc_vaccine <- historical_nc_vac_scraper$new(log=TRUE)
    historical_nc_vaccine$reset_date("SET_DATE_HERE")
    historical_nc_vaccine$raw_data
    historical_nc_vaccine$pull_raw(date = scraper$date, .dated_pull = TRUE)
    historical_nc_vaccine$raw_data
    historical_nc_vaccine$save_raw()
    historical_nc_vaccine$restruct_raw(date = scraper$date)
    historical_nc_vaccine$restruct_data
    historical_nc_vaccine$extract_from_raw(date = scraper$date)
    historical_nc_vaccine$extract_data
    historical_nc_vaccine$validate_extract()
    historical_nc_vaccine$save_extract()
}
