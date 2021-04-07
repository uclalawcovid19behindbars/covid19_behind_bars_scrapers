source("./R/generic_scraper.R")
source("./R/utilities.R")

arkansas_psychiatric_pull <- function(x){
    get_src_by_attr(x, "a", attr = "href", attr_regex = "(?i)congregate") %>%
        first()
}

arkansas_psychiatric_restruct <- function(x){
    stop_defunct_scraper("https://www.healthy.arkansas.gov/programs-services/topics/covid-19-reports")
}

arkansas_psychiatric_extract <- function(x){
    x
}

#' Scraper class for Arkansas Psychiatric COVID data
#' 
#' @name arkansas_psychiatric_scraper
#' @description AR has data for many congregate settings is compiled by DHHS.
#' Here we need to filter down to just hospital psychiatric facilities. Note this
#' is a low priority scraper as the facilities posted are inconsistent.
#' \describe{
#'   \item{Facility name}{Name}
#' }

arkansas_psychiatric_scraper <- R6Class(
    "arkansas_psychiatric_scraper",
    inherit = generic_scraper,
    public = list(
        log = NULL,
        initialize = function(
            log,
            url = "https://www.healthy.arkansas.gov/programs-services/topics/covid-19-reports",
            id = "arkansas_psychiatric",
            type = "pdf",
            state = "AR",
            jurisdiction = "psychiatric",
            # pull the JSON data directly from the API
            pull_func = arkansas_psychiatric_pull,
            # restructuring the data means pulling out the data portion of the json
            restruct_func = arkansas_psychiatric_restruct,
            # Rename the columns to appropriate database names
            extract_func = arkansas_psychiatric_extract){
            super$initialize(
                url = url, id = id, pull_func = pull_func, type = type,
                restruct_func = restruct_func, extract_func = extract_func,
                log = log, state = state, jurisdiction = jurisdiction)
        }
    )
)

if(sys.nframe() == 0){
    arkansas_psychiatric <- arkansas_psychiatric_scraper$new(log=TRUE)
    arkansas_psychiatric$raw_data
    arkansas_psychiatric$pull_raw()
    arkansas_psychiatric$raw_data
    arkansas_psychiatric$save_raw()
    arkansas_psychiatric$restruct_raw()
    arkansas_psychiatric$restruct_data
    arkansas_psychiatric$extract_from_raw()
    arkansas_psychiatric$extract_data
    arkansas_psychiatric$validate_extract()
    arkansas_psychiatric$save_extract()
}

