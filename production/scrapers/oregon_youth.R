source("./R/generic_scraper.R")
source("./R/utilities.R")

oregon_youth_pull <- function(x){
    or_youth_html <- xml2::read_html(x)
    return(or_youth_html)
}

oregon_youth_restruct <- function(x){

}

oregon_youth_extract <- function(x){

}

#' Scraper class for general Oregon COVID data
#' 
#' @name oregon_youth_scraper
#' @description Oregon youth data comes from an HTML page on the internet.
#' \describe{
#'   \item{Cumulative number of cases among youth}{}
#'   \item{Cumulative number of deaths among youth}{}
#'   \item{Cumulative number of cases among staff}{}
#'   \item{Cumulative number of deaths among staff}{}
#'   \item{Cumulative number of tests administered to youth}{}
#'   \item{Cumulative number of vaccines initiated to youth}{}
#'   \item{Staff Positives}{}
#'   \item{Staff.Recovered}{}
#' }

oregon_youth_scraper <- R6Class(
    "oregon_youth_scraper",
    inherit = generic_scraper,
    public = list(
        log = NULL,
        initialize = function(
            log,
            url = "https://www.oregon.gov/oya/Pages/COVID.aspx",
            id = "oregon_youth",
            type = "html",
            state = "OR",
            jurisdiction = "state",
            # pull the JSON data directly from the API
            pull_func = oregon_youth_pull,
            # restructuring the data means pulling out the data portion of the json
            restruct_func = oregon_youth_restruct,
            # Rename the columns to appropriate database names
            extract_func = oregon_youth_extract){
            super$initialize(
                url = url, id = id, pull_func = pull_func, type = type,
                restruct_func = restruct_func, extract_func = extract_func,
                log = log, state = state, jurisdiction = jurisdiction)
        }
    )
)

if(sys.nframe() == 0){
    oregon_youth <- oregon_youth_scraper$new(log=TRUE)
    oregon_youth$raw_data
    oregon_youth$pull_raw()
    oregon_youth$raw_data
    oregon_youth$save_raw()
    oregon_youth$restruct_raw()
    oregon_youth$restruct_data
    oregon_youth$extract_from_raw()
    oregon_youth$extract_data
    oregon_youth$validate_extract()
    oregon_youth$save_extract()
}

