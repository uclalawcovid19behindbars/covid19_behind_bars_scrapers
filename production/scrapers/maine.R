source("./R/generic_scraper.R")
source("./R/utilities.R")

maine_pull <- function(x){
    get_latest_manual("Maine")
}

maine_restruct <- function(x){
    x %>%
        select(
            Name, Staff.Confirmed, Residents.Confirmed, Staff.Deaths, 
            Staff.Recovered, Residents.Recovered, 
            Residents.Tested, Residents.Negative, Residents.Pending, 
            Residents.Population = Resident.Population, 
            Residents.Deaths = Resident.Deaths)
}

maine_extract <- function(x){
    x %>%
        mutate_at(vars(starts_with("Res")), as.numeric) %>%
        mutate_at(vars(starts_with("Staff")), as.numeric) %>%
        filter(!is.na(Name))
}

#' Scraper class for general maine COVID data
#' 
#' @name maine_scraper
#' @description This will be a description of maine data and what the scraper
#' does
#' \describe{
#'   \item{Facility_Name}{The faciilty name.}
#' }

maine_scraper <- R6Class(
    "maine_scraper",
    inherit = generic_scraper,
    public = list(
        log = NULL,
        initialize = function(
            log,
            url = "https://www.maine.gov/corrections/sites/maine.gov.corrections/files/inline-files/MDOC%20COVID19WebDashboard11-9-2020.pdf",
            id = "maine",
            type = "csv",
            state = "ME",
            # pull the JSON data directly from the API
            pull_func = maine_pull,
            # restructuring the data means pulling out the data portion of the json
            restruct_func = maine_restruct,
            # Rename the columns to appropriate database names
            extract_func = maine_extract){
            super$initialize(
                url = url, id = id, pull_func = pull_func, type = type,
                restruct_func = restruct_func, extract_func = extract_func,
                log = log, state = state)
        }
    )
)

if(sys.nframe() == 0){
    maine <- maine_scraper$new(log=TRUE)
    maine$raw_data
    maine$pull_raw()
    maine$raw_data
    maine$save_raw()
    maine$restruct_raw()
    maine$restruct_data
    maine$extract_from_raw()
    maine$extract_data
    maine$validate_extract()
    maine$save_extract()
}

