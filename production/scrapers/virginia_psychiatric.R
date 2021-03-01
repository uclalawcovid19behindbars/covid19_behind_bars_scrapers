source("./R/generic_scraper.R")
source("./R/utilities.R")

virginia_psychiatric_pull <- function(x){
    get_src_by_attr(x, "a", attr="href", attr_regex = "(?i)covid-tracker")
}

virginia_psychiatric_restruct <- function(x){
    NULL
}

virginia_psychiatric_extract <- function(x){
    NULL
}

#' Scraper class for general virginia_psychiatric COVID data
#' 
#' @name virginia_psychiatric_scraper
#' @description Data is stored in a pdf with a graph on top and facility level
#' data below. S indicates staff and P indicates patients and the names of the
#' facility are represented by the acronyms on top of the table. The variables
#' we would like corresponds to the following rows.
#' \describe{
#'   \item{Facility name}{The facility name}
#'   \item{Positive P}{Residents.Confirmed}
#'   \item{Positive S}{Staff.Confirmed}
#'   \item{Negative P}{Residents.Negative}
#'   \item{Negative S}{Staff.Negative}
#'   \item{Recovered P}{Residents.Recovered}
#'   \item{Recovered S}{Staff.Recovered}
#'   \item{Deceased P}{Residents.Deaths}
#'   \item{Deceased S}{Staff.Deaths}
#' }

virginia_psychiatric_scraper <- R6Class(
    "virginia_psychiatric_scraper",
    inherit = generic_scraper,
    public = list(
        log = NULL,
        initialize = function(
            log,
            url = "https://dbhds.virginia.gov/covid19",
            id = "virginia_psychiatric",
            type = "pdf",
            state = "VA",
            jurisdiction = "psychiatric",
            pull_func = virginia_psychiatric_pull,
            restruct_func = virginia_psychiatric_restruct,
            extract_func = virginia_psychiatric_extract){
            super$initialize(
                url = url, id = id, pull_func = pull_func, type = type,
                restruct_func = restruct_func, extract_func = extract_func,
                log = log, state = state, jurisdiction = jurisdiction)
        }
    )
)

if(sys.nframe() == 0){
    virginia_psychiatric <- virginia_psychiatric_scraper$new(log=TRUE)
    virginia_psychiatric$perma_save()
    virginia_psychiatric$raw_data
    virginia_psychiatric$pull_raw()
    virginia_psychiatric$raw_data
    virginia_psychiatric$save_raw()
    virginia_psychiatric$restruct_raw()
    virginia_psychiatric$restruct_data
    virginia_psychiatric$extract_from_raw()
    virginia_psychiatric$extract_data
    virginia_psychiatric$validate_extract()
    virginia_psychiatric$save_extract()
}

