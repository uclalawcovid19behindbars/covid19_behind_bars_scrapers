source("./R/generic_scraper.R")
source("./R/utilities.R")

wyoming_pull <- function(x){
    get_latest_manual("Wyoming")
}

wyoming_restruct <- function(x){
    x %>%
        select(
            Name, Staff.Recovered, Residents.Recovered, Residents.Active)
}

wyoming_extract <- function(x){
    x %>%
        mutate_at(vars(starts_with("Res")), as.numeric) %>%
        mutate_at(vars(starts_with("Staff")), as.numeric) %>%
        filter(!is.na(Name))
}

#' Scraper class for general wyoming COVID data
#' 
#' @name wyoming_scraper
#' @description This will be a description of wyoming data and what the scraper
#' does
#' \describe{
#'   \item{Facility_Name}{The faciilty name.}
#' }

wyoming_scraper <- R6Class(
    "wyoming_scraper",
    inherit = generic_scraper,
    public = list(
        log = NULL,
        initialize = function(
            log,
            url = "http://corrections.wyo.gov/",
            id = "wyoming",
            type = "manual",
            state = "WY",
            jurisdiction = "state",
            # pull the JSON data directly from the API
            pull_func = wyoming_pull,
            # restructuring the data means pulling out the data portion of the json
            restruct_func = wyoming_restruct,
            # Rename the columns to appropriate database names
            extract_func = wyoming_extract){
            super$initialize(
                url = url, id = id, pull_func = pull_func, type = type,
                restruct_func = restruct_func, extract_func = extract_func,
                log = log, state = state, jurisdiction = jurisdiction)
        }
    )
)

if(sys.nframe() == 0){
    wyoming <- wyoming_scraper$new(log=TRUE)
    wyoming$raw_data
    wyoming$pull_raw()
    wyoming$raw_data
    wyoming$save_raw()
    wyoming$restruct_raw()
    wyoming$restruct_data
    wyoming$extract_from_raw()
    wyoming$extract_data
    wyoming$validate_extract()
    wyoming$save_extract()
}

