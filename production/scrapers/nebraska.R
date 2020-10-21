source("./R/generic_scraper.R")
source("./R/utilities.R")

nebraska_pull <- function(x){
    xml2::read_html(x)
}

nebraska_restruct <- function(x){
    x %>%
        rvest::html_nodes("table") %>%
        lapply(rvest::html_table)
}

nebraska_extract <- function(x){
    bind_cols(x[sapply(x, function(y) nrow(y) == 1)]) %>%
        mutate(Name = "State-Wide") %>%
        clean_scraped_df() %>%
        select(
            Name,
            Residents.Tested = "Total Tests Administered",
            Residents.Confirmed = "Total Number of Confirmed Cases",
            Residents.Recovered = "Total Recovered Cases",
            Residents.Deaths = "Total Deaths") %>%
        as_tibble()
}

#' Scraper class for general nebraska COVID data
#' 
#' @name nebraska_scraper
#' @description NE reports data mostly at the state level within several easy
#' to scrape html tables.
#' \describe{
#'   \item{Facility_Name}{The facility name}
#'   \item{Active}{Residents currently infected at facility}
#'   \item{Total Tests Administered}{State-wide only}
#'   \item{Total People Tested}{State-wide only}
#'   \item{Total Number of Confirmed Cases}{State-wide only}
#'   \item{Total Recovered Cases}{State-wide only}
#'   \item{Total Deaths}{State-wide only}
#' }

nebraska_scraper <- R6Class(
    "nebraska_scraper",
    inherit = generic_scraper,
    public = list(
        log = NULL,
        initialize = function(
            log,
            url = "https://corrections.nebraska.gov/ndcs-covid-19-data",
            id = "nebraska",
            type = "html",
            state = "NE",
            # pull the JSON data directly from the API
            pull_func = nebraska_pull,
            # restructuring the data means pulling out the data portion of the json
            restruct_func = nebraska_restruct,
            # Rename the columns to appropriate database names
            extract_func = nebraska_extract){
            super$initialize(
                url = url, id = id, pull_func = pull_func, type = type,
                restruct_func = restruct_func, extract_func = extract_func,
                log = log, state = state)
        }
    )
)

if(sys.nframe() == 0){
    nebraska <- nebraska_scraper$new(log=TRUE)
    nebraska$raw_data
    nebraska$pull_raw()
    nebraska$raw_data
    nebraska$save_raw()
    nebraska$restruct_raw()
    nebraska$restruct_data
    nebraska$extract_from_raw()
    nebraska$extract_data
    nebraska$validate_extract()
    nebraska$save_extract()
}

