source("./R/generic_scraper.R")
source("./R/utilities.R")

kansas_statewide_pull <- function(x){
    get_latest_manual("kansas_statewide")
}

kansas_statewide_restruct <- function(x){
    x
}

kansas_statewide_extract <- function(x){
    x %>%
        mutate_at(vars(starts_with("Res")), as.numeric) %>%
        mutate_at(vars(starts_with("Staff")), as.numeric) %>%
        filter(!is.na(Name))
}

#' Scraper class for general kansas_statewide COVID data
#' 
#' @name kansas_statewide_scraper
#' @description This will be a description of kansas_statewide data and what the scraper
#' does
#' \describe{
#'   \item{Facility_Name}{The faciilty name.}
#' }

kansas_statewide_scraper <- R6Class(
    "kansas_statewide_scraper",
    inherit = generic_scraper,
    public = list(
        log = NULL,
        initialize = function(
            log,
            url = "https://www.kansas_statewide.gov/corrections/sites/kansas_statewide.gov.corrections/files/inline-files/MDOC%20COVID19WebDashboard11-9-2020.pdf",
            id = "kansas_statewide",
            type = "manual",
            state = "KS",
            jurisdiction = "state",
            # pull the JSON data directly from the API
            pull_func = kansas_statewide_pull,
            # restructuring the data means pulling out the data portion of the json
            restruct_func = kansas_statewide_restruct,
            # Rename the columns to appropriate database names
            extract_func = kansas_statewide_extract){
            super$initialize(
                url = url, id = id, pull_func = pull_func, type = type,
                restruct_func = restruct_func, extract_func = extract_func,
                log = log, state = state, jurisdiction = jurisdiction)
        }
    )
)

if(sys.nframe() == 0){
    kansas_statewide <- kansas_statewide_scraper$new(log=TRUE)
    kansas_statewide$raw_data
    kansas_statewide$pull_raw()
    kansas_statewide$raw_data
    kansas_statewide$save_raw()
    kansas_statewide$restruct_raw()
    kansas_statewide$restruct_data
    kansas_statewide$extract_from_raw()
    kansas_statewide$extract_data
    kansas_statewide$validate_extract()
    kansas_statewide$save_extract()
}

