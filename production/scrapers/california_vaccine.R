source("./R/generic_scraper.R")
source("./R/utilities.R")

california_vaccine_pull <- function(x){
    get_latest_manual("California_Vaccine")
}

california_vaccine_restruct <- function(x){
    x %>%
        select(
            Name, starts_with("Res"), starts_with("Staff"))
}

california_vaccine_extract <- function(x){
    x %>%
        mutate_at(vars(starts_with("Res")), as.numeric) %>%
        mutate_at(vars(starts_with("Staff")), as.numeric) %>%
        filter(!is.na(Name))
}

#' Scraper class for general california_vaccine COVID data
#' 
#' @name california_vaccine_scraper
#' @description As of 11/20/20 california_vaccine stopped reporting cumulative cases and
#' only reports active resident cases. Note that we only update california_vaccine data if
#' it is less than 7 days old. If WY hasnt produced a new data sheet after 7
#' days then no data should be recorded by the scraper and this scraper should
#' fail.
#' \describe{
#'   \item{Facility_Name}{The faciilty name.}
#' }

california_vaccine_scraper <- R6Class(
    "california_vaccine_scraper",
    inherit = generic_scraper,
    public = list(
        log = NULL,
        initialize = function(
            log,
            url = "https://www.cdcr.ca.gov/covid19/",
            id = "california_vaccine",
            type = "manual",
            state = "CA",
            jurisdiction = "state",
            # pull the JSON data directly from the API
            pull_func = california_vaccine_pull,
            # restructuring the data means pulling out the data portion of the json
            restruct_func = california_vaccine_restruct,
            # Rename the columns to appropriate database names
            extract_func = california_vaccine_extract){
            super$initialize(
                url = url, id = id, pull_func = pull_func, type = type,
                restruct_func = restruct_func, extract_func = extract_func,
                log = log, state = state, jurisdiction = jurisdiction)
        }
    )
)

if(sys.nframe() == 0){
    california_vaccine <- california_vaccine_scraper$new(log=TRUE)
    california_vaccine$raw_data
    california_vaccine$pull_raw()
    california_vaccine$raw_data
    california_vaccine$save_raw()
    california_vaccine$restruct_raw()
    california_vaccine$restruct_data
    california_vaccine$extract_from_raw()
    california_vaccine$extract_data
    california_vaccine$validate_extract()
    california_vaccine$save_extract()
}

