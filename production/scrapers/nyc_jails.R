source("./R/generic_scraper.R")
source("./R/utilities.R")

nyc_jails_pull <- function(x){
    get_latest_manual("New York City Jails")
}

nyc_jails_restruct <- function(x){
    x %>%
        select(
            Resident.Deaths, Staff.Confirmed, Residents.Confirmed,
            Staff.Deaths, Resident.Population, Residents.Quarantine, Name)
}

nyc_jails_extract <- function(x){
    x %>%
        rename(
            Residents.Deaths = Resident.Deaths, 
            Residents.Population = Resident.Population) %>%
        mutate_at(vars(starts_with("Res")), as.numeric) %>%
        mutate_at(vars(starts_with("Staff")), as.numeric) %>%
        filter(!is.na(Name))
}

#' Scraper class for general nyc_jails COVID data
#' 
#' @name nyc_jails_scraper
#' @description This will be a description of nyc_jails data and what the scraper
#' does
#' \describe{
#'   \item{Facility_Name}{The faciilty name.}
#' }

nyc_jails_scraper <- R6Class(
    "nyc_jails_scraper",
    inherit = generic_scraper,
    public = list(
        log = NULL,
        initialize = function(
            log,
            url = "https://www1.nyc.gov/site/boc/covid-19.page",
            id = "nyc_jails",
            type = "csv",
            state = "NY",
            # pull the JSON data directly from the API
            pull_func = nyc_jails_pull,
            # restructuring the data means pulling out the data portion of the json
            restruct_func = nyc_jails_restruct,
            # Rename the columns to appropriate database names
            extract_func = nyc_jails_extract){
            super$initialize(
                url = url, id = id, pull_func = pull_func, type = type,
                restruct_func = restruct_func, extract_func = extract_func,
                log = log, state = state)
        }
    )
)

if(sys.nframe() == 0){
    nyc_jails <- nyc_jails_scraper$new(log=TRUE)
    nyc_jails$raw_data
    nyc_jails$pull_raw()
    nyc_jails$raw_data
    nyc_jails$save_raw()
    nyc_jails$restruct_raw()
    nyc_jails$restruct_data
    nyc_jails$extract_from_raw()
    nyc_jails$extract_data
    nyc_jails$validate_extract()
    nyc_jails$save_extract()
}

