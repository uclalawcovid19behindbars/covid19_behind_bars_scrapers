source("./R/generic_scraper.R")
source("./R/utilities.R")

north_carolina_jails_pull <- function(x){
    x %>%
        xml2::read_html()
    
    get_src_by_attr(x, "a", attr = "href", attr_regex = "ongoing-outbreaks")
}

north_carolina_jails_restruct <- function(x){
    stop_defunct_scraper("https://covid19.ncdhhs.gov/dashboard/outbreaks-and-clusters")
}

north_carolina_jails_extract <- function(x){
    x
}

#' Scraper class for general North Carolina COVID data
#' 
#' @name north_carolina_jails_scraper
#' @description NC has data for many congregate settings is compiled by DHHS.
#' \describe{
#'   \item{Facility name}{The facility name.}
#'   \item{Residents Cases}{Residents.Confirmed}
#'   \item{Staff Cases}{Staff.Confirmed}
#'   \item{Residents Deaths}{Residents.Deaths}
#'   \item{Staff Deaths}{Staff.Deaths}
#' }

north_carolina_jails_scraper <- R6Class(
    "north_carolina_jails_scraper",
    inherit = generic_scraper,
    public = list(
        log = NULL,
        initialize = function(
            log,
            url = "https://covid19.ncdhhs.gov/dashboard/outbreaks-and-clusters",
            id = "north_carolina_jails",
            type = "pdf",
            state = "NC",
            jurisdiction = "county",
            # pull the JSON data directly from the API
            pull_func = north_carolina_jails_pull,
            # restructuring the data means pulling out the data portion of the json
            restruct_func = north_carolina_jails_restruct,
            # Rename the columns to appropriate database names
            extract_func = north_carolina_jails_extract){
            super$initialize(
                url = url, id = id, pull_func = pull_func, type = type,
                restruct_func = restruct_func, extract_func = extract_func,
                log = log, state = state, jurisdiction = jurisdiction)
        }
    )
)

if(sys.nframe() == 0){
    north_carolina_jails <- north_carolina_jails_scraper$new(log=TRUE)
    north_carolina_jails$raw_data
    north_carolina_jails$pull_raw()
    north_carolina_jails$raw_data
    north_carolina_jails$save_raw()
    north_carolina_jails$restruct_raw()
    north_carolina_jails$restruct_data
    north_carolina_jails$extract_from_raw()
    north_carolina_jails$extract_data
    north_carolina_jails$validate_extract()
    north_carolina_jails$save_extract()
}

