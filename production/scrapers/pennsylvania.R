source("./R/generic_scraper.R")
source("./R/utilities.R")

pennsylvania_pull <- function(x){
    NULL
    
}

pennsylvania_restruct <- function(x){
    NULL
}

pennsylvania_extract <- function(x){
        NULL
}

#' Scraper class for general pennsylvania COVID data
#' 
#' @name pennsylvania_scraper
#' @description This will be a description of pennsylvania data and what the scraper
#' does
#' \describe{
#'   \item{Facility_Name}{The faciilty name.}
#' }

pennsylvania_scraper <- R6Class(
    "pennsylvania_scraper",
    inherit = generic_scraper,
    public = list(
        log = NULL,
        initialize = function(
            log,
            url = "http://url/goes/here",
            id = "pennsylvania",
            type = "pdf",
            state = "PA",
            # pull the JSON data directly from the API
            pull_func = pennsylvania_pull,
            # restructuring the data means pulling out the data portion of the json
            restruct_func = pennsylvania_restruct,
            # Rename the columns to appropriate database names
            extract_func = pennsylvania_extract){
            super$initialize(
                url = url, id = id, pull_func = pull_func, type = type,
                restruct_func = restruct_func, extract_func = extract_func,
                log = log, state = state)
        }
    )
)

if(sys.nframe() == 0){
    pennsylvania <- pennsylvania_scraper$new(log=TRUE)
    pennsylvania$raw_data
    pennsylvania$pull_raw()
    pennsylvania$raw_data
    pennsylvania$save_raw()
    pennsylvania$restruct_raw()
    pennsylvania$restruct_data
    pennsylvania$extract_from_raw()
    pennsylvania$extract_data
    pennsylvania$validate_extract()
    pennsylvania$save_extract()
}

