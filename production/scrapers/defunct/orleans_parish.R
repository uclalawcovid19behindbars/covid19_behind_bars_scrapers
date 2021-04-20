source("./R/generic_scraper.R")
source("./R/utilities.R")

orleans_parish_pull <- function(x){
    stop_defunct_scraper(x)
    #get_latest_manual("Orleans Parish Jails")
}

orleans_parish_restruct <- function(x){
    x %>%
        select(
            Name, Staff.Confirmed, Residents.Confirmed, Staff.Deaths, 
            Residents.Deaths = Resident.Deaths, Staff.Recovered,
            Residents.Recovered, Residents.Tested, Residents.Negative,
            Residents.Pending, Resident.Population, Staff.Quarantine, 
            Residents.Quarantine, Staff.Tested)
}

orleans_parish_extract <- function(x){
    x %>%
        mutate_at(vars(starts_with("Res")), as.numeric) %>%
        mutate_at(vars(starts_with("Staff")), as.numeric) %>%
        filter(!is.na(Name))
}

#' Scraper class for general orleans_parish COVID data
#' 
#' @name orleans_parish_scraper
#' @description This will be a description of orleans_parish data and what the scraper
#' does
#' \describe{
#'   \item{Facility_Name}{The faciilty name.}
#' }

orleans_parish_scraper <- R6Class(
    "orleans_parish_scraper",
    inherit = generic_scraper,
    public = list(
        log = NULL,
        initialize = function(
            log,
            url = "http://opcso.org/index.php",
            id = "orleans_parish",
            type = "manual",
            state = "LA",
            jurisdiction = "county",
            # pull the JSON data directly from the API
            pull_func = orleans_parish_pull,
            # restructuring the data means pulling out the data portion of the json
            restruct_func = orleans_parish_restruct,
            # Rename the columns to appropriate database names
            extract_func = orleans_parish_extract){
            super$initialize(
                url = url, id = id, pull_func = pull_func, type = type,
                restruct_func = restruct_func, extract_func = extract_func,
                log = log, state = state, jurisdiction = jurisdiction)
        }
    )
)

if(sys.nframe() == 0){
    orleans_parish <- orleans_parish_scraper$new(log=TRUE)
    orleans_parish$raw_data
    orleans_parish$pull_raw()
    orleans_parish$raw_data
    orleans_parish$save_raw()
    orleans_parish$restruct_raw()
    orleans_parish$restruct_data
    orleans_parish$extract_from_raw()
    orleans_parish$extract_data
    orleans_parish$validate_extract()
    orleans_parish$save_extract()
}

