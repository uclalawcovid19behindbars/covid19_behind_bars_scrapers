source("./R/generic_scraper.R")
source("./R/utilities.R")

ohio_psychiatric_pull <- function(x){
    get_src_by_attr(x, "a", attr = "href", attr_regex = "(?i)MHASCOVID")
}

ohio_psychiatric_restruct <- function(x){
    stop_defunct_scraper("https://coronavirus.ohio.gov/wps/portal/gov/covid-19/home")
}

ohio_psychiatric_extract <- function(x){
    x
}

#' Scraper class for general Ohio psychiatric COVID data
#' 
#' @name ohio_psychiatric_scraper
#' Ohio psychiatric wards post minimal data on seemingly active cases
#' 
#' @description 
#' \describe{
#'   \item{Hsopital}{Name}
#'   \item{Patients Positive Covid-19 Test}{Residents.Active}
#' }

ohio_psychiatric_scraper <- R6Class(
    "ohio_psychiatric_scraper",
    inherit = generic_scraper,
    public = list(
        log = NULL,
        initialize = function(
            log,
            url = "https://coronavirus.ohio.gov/wps/portal/gov/covid-19/home",
            id = "ohio_psychiatric",
            type = "pdf",
            state = "OH",
            jurisdiction = "psychiatric",
            # pull the JSON data directly from the API
            pull_func = ohio_psychiatric_pull,
            # restructuring the data means pulling out the data portion of the json
            restruct_func = ohio_psychiatric_restruct,
            # Rename the columns to appropriate database names
            extract_func = ohio_psychiatric_extract){
            super$initialize(
                url = url, id = id, pull_func = pull_func, type = type,
                restruct_func = restruct_func, extract_func = extract_func,
                log = log, state = state, jurisdiction = jurisdiction)
        }
    )
)

if(sys.nframe() == 0){
    ohio_psychiatric <- ohio_psychiatric_scraper$new(log=TRUE)
    ohio_psychiatric$raw_data
    ohio_psychiatric$pull_raw()
    ohio_psychiatric$raw_data
    ohio_psychiatric$save_raw()
    ohio_psychiatric$restruct_raw()
    ohio_psychiatric$restruct_data
    ohio_psychiatric$extract_from_raw()
    ohio_psychiatric$extract_data
    ohio_psychiatric$validate_extract()
    ohio_psychiatric$save_extract()
}

