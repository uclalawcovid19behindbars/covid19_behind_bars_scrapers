source("./R/generic_scraper.R")
source("./R/utilities.R")

rhode_island_pull <- function(x){
    get_latest_manual("Rhode Island")
}

rhode_island_restruct <- function(x){
    x %>%
        select(
            Name, Staff.Confirmed, Residents.Confirmed, Staff.Deaths, 
            Residents.Deaths = Resident.Deaths,
            Staff.Recovered, Residents.Recovered)
}

rhode_island_extract <- function(x){
    x %>%
        mutate_at(vars(starts_with("Res")), as.numeric) %>%
        mutate_at(vars(starts_with("Staff")), as.numeric) %>%
        filter(!is.na(Name))
}

#' Scraper class for general rhode_island COVID data
#' 
#' @name rhode_island_scraper
#' @description This will be a description of rhode_island data and what the scraper
#' does
#' \describe{
#'   \item{Facility_Name}{The faciilty name.}
#' }

rhode_island_scraper <- R6Class(
    "rhode_island_scraper",
    inherit = generic_scraper,
    public = list(
        log = NULL,
        initialize = function(
            log,
            url = "http://www.doc.ri.gov/index.php",
            id = "rhode_island",
            type = "manual",
            state = "RI",
            jurisdiction = "state",
            # pull the JSON data directly from the API
            pull_func = rhode_island_pull,
            # restructuring the data means pulling out the data portion of the json
            restruct_func = rhode_island_restruct,
            # Rename the columns to appropriate database names
            extract_func = rhode_island_extract){
            super$initialize(
                url = url, id = id, pull_func = pull_func, type = type,
                restruct_func = restruct_func, extract_func = extract_func,
                log = log, state = state, jurisdiction = jurisdiction)
        }
    )
)

if(sys.nframe() == 0){
    rhode_island <- rhode_island_scraper$new(log=TRUE)
    rhode_island$raw_data
    rhode_island$pull_raw()
    rhode_island$raw_data
    rhode_island$save_raw()
    rhode_island$restruct_raw()
    rhode_island$restruct_data
    rhode_island$extract_from_raw()
    rhode_island$extract_data
    rhode_island$validate_extract()
    rhode_island$save_extract()
}

