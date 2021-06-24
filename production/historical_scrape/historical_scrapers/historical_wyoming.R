source("./R/generic_scraper.R")
source("./R/utilities.R")

historical_wyoming_pull <- function(x, date = NULL, file = NULL){
    get_latest_manual("Wyoming_Historical")
}

historical_wyoming_restruct <- function(x, date = NULL){
    x %>% 
        select(Date, Name, Residents.Active, Residents.Deaths)
}

historical_wyoming_extract <- function(x, date){
    x %>%
        filter(Date == date) %>% 
        {suppressWarnings(mutate_at(., vars(starts_with("Res")), as.numeric))} %>%
        {suppressWarnings(mutate_at(., vars(starts_with("Staff")), as.numeric))} %>%
        filter(!is.na(Name)) %>% 
        select(-Date)
}

#' Scraper class for Wyoming data
#' 
#' @name historical_wyoming_scraper
#' @description This scraper pulls data from a manual csv file. Wyoming updates data
#' infrequently via a pdf press release. 
#' \describe{
#'   \item{Positive positive cases for inmates}{Active cases among residents}
#'   \item{Number of inmate deaths to date}{Cumulative COVID-19 deaths}
#' }

historical_wyoming_scraper <- R6Class(
    "historical_wyoming_scraper",
    inherit = generic_scraper,
    public = list(
        log = NULL,
        initialize = function(
            log,
            url = "http://corrections.wyo.gov/",
            id = "historical_wyoming",
            type = "manual",
            state = "WY",
            jurisdiction = "state",
            check_date = NULL,
            pull_func = historical_wyoming_pull,
            restruct_func = historical_wyoming_restruct,
            extract_func = historical_wyoming_extract){
            super$initialize(
                url = url, id = id, pull_func = pull_func, type = type,
                restruct_func = restruct_func, extract_func = extract_func,
                log = log, state = state, jurisdiction  = jurisdiction,
                check_date = check_date)
        }
    )
)

if(sys.nframe() == 0){
    historical_wyoming <- historical_wyoming_scraper$new(log=TRUE)
    historical_wyoming$reset_date("SET_DATE_HERE")
    historical_wyoming$raw_data
    historical_wyoming$pull_raw(date = scraper$date, .dated_pull = TRUE)
    historical_wyoming$raw_data
    historical_wyoming$save_raw()
    historical_wyoming$restruct_raw(date = scraper$date)
    historical_wyoming$restruct_data
    historical_wyoming$extract_from_raw(date = scraper$date)
    historical_wyoming$extract_data
    historical_wyoming$validate_extract()
    historical_wyoming$save_extract()
}