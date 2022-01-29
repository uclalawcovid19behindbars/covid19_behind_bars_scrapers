source("./R/generic_scraper.R")
source("./R/utilities.R")

indiana_psychiatric_date_check <- function(x, date = Sys.Date()){
    base_html <- xml2::read_html(x)
    
    base_html %>%
        rvest::html_nodes("p") %>%
        rvest::html_text() %>% 
        {.[str_detect(., "(?i)last updated")]} %>% 
        lubridate::mdy() %>%
        error_on_date(date)
}

indiana_psychiatric_pull <- function(x){
    xml2::read_html(x)
}

indiana_psychiatric_restruct <- function(x){
    stop_defunct_scraper("https://www.in.gov/fssa/dmha/state-psychiatric-hospitals/indiana-state-psychiatric-hospitals-covid-19-statistics/")
}

indiana_psychiatric_extract <- function(x){
    NULL
}

#' Scraper class for general indiana_psychiatric COVID data
#' 
#' @name indiana_psychiatric_scraper
#' @description There are multiple html tables on this page however we are
#' interested in the table titled State Operated Psychiatric Hospitals. Row
#' names here indicate state psychiatric facilities and we want the columns
#' below
#' \describe{
#'   \item{Facility name}{The facility name.}
#'   \item{Patients testing positive}{Residents.Confirmed}
#'   \item{Staff testing positive}{Staff.Confirmed}
#' }

indiana_psychiatric_scraper <- R6Class(
    "indiana_psychiatric_scraper",
    inherit = generic_scraper,
    public = list(
        log = NULL,
        initialize = function(
            log,
            url = "https://www.in.gov/fssa/dmha/state-psychiatric-hospitals/indiana-state-psychiatric-hospitals-covid-19-statistics/",
            id = "indiana_psychiatric",
            type = "html",
            state = "IN",
            jurisdiction = "psychiatric",
            check_date = indiana_psychiatric_date_check,
            pull_func = indiana_psychiatric_pull,
            restruct_func = indiana_psychiatric_restruct,
            extract_func = indiana_psychiatric_extract){
            super$initialize(
                url = url, id = id, pull_func = pull_func, type = type,
                restruct_func = restruct_func, extract_func = extract_func,
                log = log, state = state, jurisdiction = jurisdiction,
                check_date = check_date)
        }
    )
)

if(sys.nframe() == 0){
    indiana_psychiatric <- indiana_psychiatric_scraper$new(log=TRUE)
    indiana_psychiatric$run_check_date()
    indiana_psychiatric$perma_save()
    indiana_psychiatric$raw_data
    indiana_psychiatric$pull_raw()
    indiana_psychiatric$raw_data
    indiana_psychiatric$save_raw()
    indiana_psychiatric$restruct_raw()
    indiana_psychiatric$restruct_data
    indiana_psychiatric$extract_from_raw()
    indiana_psychiatric$extract_data
    indiana_psychiatric$validate_extract()
    indiana_psychiatric$save_extract()
}

