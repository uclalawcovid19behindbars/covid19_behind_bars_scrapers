source("./R/generic_scraper.R")
source("./R/utilities.R")

new_mexico_pull <- function(x){
    get_latest_manual("New Mexico")
}

new_mexico_restruct <- function(x){
    x %>%
        select(
            Name,
            Residents.Confirmed,
            Residents.Active,
            Residents.Deaths = Resident.Deaths, 
            Residents.Recovered,
            Residents.Tadmin
            )
}

new_mexico_extract <- function(x){
    x %>%
        mutate_at(vars(starts_with("Res")), as.numeric) %>%
        mutate_at(vars(starts_with("Staff")), as.numeric) %>%
        filter(!is.na(Name))
}

#' Scraper class for general new_mexico COVID data
#' 
#' @name new_mexico_scraper
#' @description This will be a description of new_mexico data and what the scraper
#' does
#' \describe{
#'   \item{Facility_Name}{The faciilty name.}
#' }

new_mexico_scraper <- R6Class(
    "new_mexico_scraper",
    inherit = generic_scraper,
    public = list(
        log = NULL,
        initialize = function(
            log,
            url = "https://cd.nm.gov/covid-19-updates/",
            id = "new_mexico",
            type = "manual",
            state = "NM",
            jurisdiction = "state",
            # pull the JSON data directly from the API
            pull_func = new_mexico_pull,
            # restructuring the data means pulling out the data portion of the json
            restruct_func = new_mexico_restruct,
            # Rename the columns to appropriate database names
            extract_func = new_mexico_extract){
            super$initialize(
                url = url, id = id, pull_func = pull_func, type = type,
                restruct_func = restruct_func, extract_func = extract_func,
                log = log, state = state, jurisdiction = jurisdiction)
        }
    )
)

if(sys.nframe() == 0){
    new_mexico <- new_mexico_scraper$new(log=TRUE)
    new_mexico$raw_data
    new_mexico$pull_raw()
    new_mexico$raw_data
    new_mexico$save_raw()
    new_mexico$restruct_raw()
    new_mexico$restruct_data
    new_mexico$extract_from_raw()
    new_mexico$extract_data
    new_mexico$validate_extract()
    new_mexico$save_extract()
}

