source("./R/generic_scraper.R")
source("./R/utilities.R")

north_carolina_psychiatric_pull <- function(x){
    get_src_by_attr(x, "a", attr = "href", attr_regex = "ongoing-outbreaks")
}

north_carolina_psychiatric_restruct <- function(x){
    stop_defunct_scraper("https://covid19.ncdhhs.gov/dashboard/outbreaks-and-clusters")
}

north_carolina_psychiatric_extract <- function(x){
    x
}

#' Scraper class for general North Carolina COVID data
#' 
#' @name north_carolina_psychiatric_scraper
#' @description NC has data for many congregate settings is compiled by DHHS.
#' \describe{
#'   \item{Facility name}{The facility name.}
#'   \item{Residents Cases}{Residents.Confirmed}
#'   \item{Staff Cases}{Staff.Confirmed}
#'   \item{Residents Deaths}{Residents.Deaths}
#'   \item{Staff Deaths}{Staff.Deaths}
#' }

north_carolina_psychiatric_scraper <- R6Class(
    "north_carolina_psychiatric_scraper",
    inherit = generic_scraper,
    public = list(
        log = NULL,
        initialize = function(
            log,
            url = "https://covid19.ncdhhs.gov/dashboard/outbreaks-and-clusters",
            id = "north_carolina_psychiatric",
            type = "pdf",
            state = "NC",
            jurisdiction = "psychiatric",
            # pull the JSON data directly from the API
            pull_func = north_carolina_psychiatric_pull,
            # restructuring the data means pulling out the data portion of the json
            restruct_func = north_carolina_psychiatric_restruct,
            # Rename the columns to appropriate database names
            extract_func = north_carolina_psychiatric_extract){
            super$initialize(
                url = url, id = id, pull_func = pull_func, type = type,
                restruct_func = restruct_func, extract_func = extract_func,
                log = log, state = state, jurisdiction = jurisdiction)
        }
    )
)

if(sys.nframe() == 0){
    north_carolina_psychiatric <- north_carolina_psychiatric_scraper$new(log=TRUE)
    north_carolina_psychiatric$raw_data
    north_carolina_psychiatric$pull_raw()
    north_carolina_psychiatric$raw_data
    north_carolina_psychiatric$save_raw()
    north_carolina_psychiatric$restruct_raw()
    north_carolina_psychiatric$restruct_data
    north_carolina_psychiatric$extract_from_raw()
    north_carolina_psychiatric$extract_data
    north_carolina_psychiatric$validate_extract()
    north_carolina_psychiatric$save_extract()
}

