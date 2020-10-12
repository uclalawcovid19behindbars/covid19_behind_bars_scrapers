source("./R/generic_scraper.R")
source("./R/utilities.R")

replace_pull <- function(x){
    NULL
    
}

replace_restruct <- function(x){
    NULL
}

replace_extract <- function(x){
        NULL
}

#' Scraper class for general replace COVID data
#' 
#' @name replace_scraper
#' @description This will be a description of replace data and what the scraper
#' does
#' \describe{
#'   \item{Facility_Name}{The faciilty name.}
#' }

replace_scraper <- R6Class(
    "replace_scraper",
    inherit = generic_scraper,
    public = list(
        log = NULL,
        initialize = function(
            log,
            url = "http://url/goes/here",
            id = "rplace",
            type = "pdf",
            state = "",
            # pull the JSON data directly from the API
            pull_func = replace_pull,
            # restructuring the data means pulling out the data portion of the json
            restruct_func = replace_restruct,
            # Rename the columns to appropriate database names
            extract_func = replace_extract){
            super$initialize(
                url = url, id = id, pull_func = pull_func, type = type,
                restruct_func = restruct_func, extract_func = extract_func,
                log = log, state = state)
        }
    )
)

if(sys.nframe() == 0){
    replace <- replace_scraper$new(log=TRUE)
    replace$raw_data
    replace$pull_raw()
    replace$raw_data
    replace$save_raw()
    replace$restruct_raw()
    replace$restruct_data
    replace$extract_from_raw()
    replace$extract_data
    replace$validate_extract()
    replace$save_extract()
}

