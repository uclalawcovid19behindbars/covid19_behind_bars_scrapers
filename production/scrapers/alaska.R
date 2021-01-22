source("./R/generic_scraper.R")
source("./R/utilities.R")

alaska_pull <- function(x){
    xml2::read_html(x)
}

alaska_restruct <- function(x){
    
    tibble(
        Name = "State-Wide",
        Residents.Remand = x %>%
            rvest::html_node('#remand_div .tracker_count') %>%
            rvest::html_text() %>%
            parse_number(),
        Residents.Confirmed = x %>%
            rvest::html_node('#positive_div .tracker_count') %>%
            rvest::html_text() %>%
            parse_number(),
        Residents.Tadmin = x %>%
            rvest::html_node('#tested_div .tracker_count') %>%
            rvest::html_text() %>%
            parse_number(),
        Residents.Negative = x %>%
            rvest::html_node('#negative_div .tracker_count') %>%
            rvest::html_text() %>%
            parse_number(),
        Residents.Pending = x %>%
            rvest::html_node('#pending_div .tracker_count') %>%
            rvest::html_text() %>%
            parse_number(),
        Residents.Deaths = x %>%
            rvest::html_node('#deaths_div .tracker_count') %>%
            rvest::html_text() %>%
            parse_number()
        )
}

alaska_extract <- function(x){
    x %>%
        mutate(Residents.Confirmed = Residents.Confirmed + Residents.Remand) %>%
        select(-Residents.Remand)
}

#' Scraper class for general Alaska COVID data
#' 
#' @name alaska_scraper
#' @description Minimal html webscraping. Since scraping began website has
#' had minimal changes. Only publishes state-wide information and only for
#' residents. Data is updated weekly.
#' \describe{
#'   \item{Total Tests}{Cumulatiove tests administered.}
#'   \item{Negative Tests}{Cumulative negative results.}
#'   \item{Pending Results}{Currently pending.}
#'   \item{Remand Positives}{Positives cases not sure how different from below.}
#'   \item{General Population Psoitives}{Not sure on the distinction here.}
#' }

alaska_scraper <- R6Class(
    "alaska_scraper",
    inherit = generic_scraper,
    public = list(
        log = NULL,
        initialize = function(
            log,
            url = "https://doc.alaska.gov/covid-19",
            id = "alaska",
            type = "html",
            state = "AK",
            jurisdiction = "state",
            # pull the JSON data directly from the API
            pull_func = alaska_pull,
            # restructuring the data means pulling out the data portion of the json
            restruct_func = alaska_restruct,
            # Rename the columns to appropriate database names
            extract_func = alaska_extract){
            super$initialize(
                url = url, id = id, pull_func = pull_func, type = type,
                restruct_func = restruct_func, extract_func = extract_func,
                log = log, state = state, jurisdiction = jurisdiction)
        }
    )
)

if(sys.nframe() == 0){
    alaska <- alaska_scraper$new(log=FALSE)
    alaska$raw_data
    alaska$pull_raw()
    alaska$raw_data
    alaska$restruct_raw()
    alaska$restruct_data
    alaska$extract_from_raw()
    alaska$extract_data
    alaska$validate_extract()
    alaska$save_extract()
}

