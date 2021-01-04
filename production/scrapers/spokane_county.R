source("./R/generic_scraper.R")
source("./R/utilities.R")

spokane_county_pull <- function(x){
    stop_defunct_scraper(x)
    #xml2::read_html(x)
}

spokane_county_restruct <- function(x){
    NULL
}

spokane_county_extract <- function(x){
    NULL
}

#' Scraper class for general spokane_county COVID data
#' 
#' @name spokane_county_scraper
#' @description This will be a description of spokane_county data and what the scraper
#' does
#' \describe{
#'   \item{Facility_Name}{The facility name.}
#' }

spokane_county_scraper <- R6Class(
    "spokane_county_scraper",
    inherit = generic_scraper,
    public = list(
        log = NULL,
        initialize = function(
            log,
            url = "https://www.spokanecounty.org/351/Detention-Services",
            id = "spokane_county",
            type = "html",
            state = "WA",
            jurisdiction = "county",
            # pull the JSON data directly from the API
            pull_func = spokane_county_pull,
            # restructuring the data means pulling out the data portion of the json
            restruct_func = spokane_county_restruct,
            # Rename the columns to appropriate database names
            extract_func = spokane_county_extract){
            super$initialize(
                url = url, id = id, pull_func = pull_func, type = type,
                restruct_func = restruct_func, extract_func = extract_func,
                log = log, state = state, jurisdiction = jurisdiction)
        }
    )
)

if(sys.nframe() == 0){
    spokane_county <- spokane_county_scraper$new(log=TRUE)
    spokane_county$raw_data
    spokane_county$pull_raw()
    spokane_county$raw_data
    spokane_county$save_raw()
    spokane_county$restruct_raw()
    spokane_county$restruct_data
    spokane_county$extract_from_raw()
    spokane_county$extract_data
    spokane_county$validate_extract()
    spokane_county$save_extract()
}

