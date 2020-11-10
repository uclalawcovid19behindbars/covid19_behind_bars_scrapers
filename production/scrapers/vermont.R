source("./R/generic_scraper.R")
source("./R/utilities.R")

vermont_pull <- function(x){
    get_latest_manual("Vermont")
}

vermont_restruct <- function(x){
    x %>%
        select(
            Name, Staff.Confirmed, Residents.Confirmed, Staff.Deaths, 
            Staff.Recovered, Residents.Recovered,
            Residents.Tested, Residents.Negative, Residents.Pending, 
            Residents.Population = Resident.Population, 
            Staff.Quarantine, Residents.Quarantine,
            Residents.Deaths = Resident.Deaths)
}

vermont_extract <- function(x){
    x %>%
        mutate_at(vars(starts_with("Res")), as.numeric) %>%
        mutate_at(vars(starts_with("Staff")), as.numeric) %>%
        filter(!is.na(Name))
}

#' Scraper class for general vermont COVID data
#' 
#' @name vermont_scraper
#' @description This will be a description of vermont data and what the scraper
#' does
#' \describe{
#'   \item{Facility_Name}{The faciilty name.}
#' }

vermont_scraper <- R6Class(
    "vermont_scraper",
    inherit = generic_scraper,
    public = list(
        log = NULL,
        initialize = function(
            log,
            url = "https://doc.vermont.gov/covid-19-information-page",
            id = "vermont",
            type = "csv",
            state = "VT",
            # pull the JSON data directly from the API
            pull_func = vermont_pull,
            # restructuring the data means pulling out the data portion of the json
            restruct_func = vermont_restruct,
            # Rename the columns to appropriate database names
            extract_func = vermont_extract){
            super$initialize(
                url = url, id = id, pull_func = pull_func, type = type,
                restruct_func = restruct_func, extract_func = extract_func,
                log = log, state = state)
        }
    )
)

if(sys.nframe() == 0){
    vermont <- vermont_scraper$new(log=TRUE)
    vermont$raw_data
    vermont$pull_raw()
    vermont$raw_data
    vermont$save_raw()
    vermont$restruct_raw()
    vermont$restruct_data
    vermont$extract_from_raw()
    vermont$extract_data
    vermont$validate_extract()
    vermont$save_extract()
}

