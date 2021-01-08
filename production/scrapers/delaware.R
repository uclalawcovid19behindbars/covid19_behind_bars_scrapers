source("./R/generic_scraper.R")
source("./R/utilities.R")

delaware_pull <- function(x){
    get_latest_manual("Delaware")
}

delaware_restruct <- function(x){
    x %>%
        select(
            Name, Staff.Confirmed, Residents.Confirmed, Residents.Active, 
            Staff.Deaths, Staff.Recovered, Residents.Recovered, 
            Residents.Deaths = Resident.Deaths)
}

delaware_extract <- function(x){
    x %>%
        mutate_at(vars(starts_with("Res")), as.numeric) %>%
        mutate_at(vars(starts_with("Staff")), as.numeric) %>%
        filter(!is.na(Name))
}

#' Scraper class for general delaware COVID data
#' 
#' @name delaware_scraper
#' @description This will be a description of delaware data and what the scraper
#' does
#' \describe{
#'   \item{Facility_Name}{The faciilty name.}
#' }

delaware_scraper <- R6Class(
    "delaware_scraper",
    inherit = generic_scraper,
    public = list(
        log = NULL,
        initialize = function(
            log,
            url = "https://doc.delaware.gov/assets/documents/Confirmed_COVID_Cases.pdf",
            id = "delaware",
            type = "manual",
            state = "DE",
            jurisdiction = "state",
            # pull the JSON data directly from the API
            pull_func = delaware_pull,
            # restructuring the data means pulling out the data portion of the json
            restruct_func = delaware_restruct,
            # Rename the columns to appropriate database names
            extract_func = delaware_extract){
            super$initialize(
                url = url, id = id, pull_func = pull_func, type = type,
                restruct_func = restruct_func, extract_func = extract_func,
                log = log, state = state, jurisdiction = jurisdiction)
        }
    )
)

if(sys.nframe() == 0){
    delaware <- delaware_scraper$new(log=TRUE)
    delaware$raw_data
    delaware$pull_raw()
    delaware$raw_data
    delaware$save_raw()
    delaware$restruct_raw()
    delaware$restruct_data
    delaware$extract_from_raw()
    delaware$extract_data
    delaware$validate_extract()
    delaware$save_extract()
}

