source("./R/generic_scraper.R")
source("./R/utilities.R")

north_carolina_check_date <- function(url, date = Sys.Date()){
    base_html <- xml2::read_html(url)
    date_txt <- rvest::html_nodes(base_html, ".asOfText") %>%
        rvest::html_text()
    
    date_txt %>%
        {.[str_detect(., "(?i)20")]} %>% # look for year 20xx
        str_extract("\\d{1,2}/\\d{1,2}/\\d{2,4}") %>%
        lubridate::mdy() %>%
        error_on_date(expected_date = date)
}

north_carolina_pull <- function(x){
    x %>%
        xml2::read_html()
}

north_carolina_restruct <- function(x){
    x %>%
        rvest::html_table() %>%
        .[[1]]
}

north_carolina_extract <- function(x){
    x %>%
        as_tibble() %>%
        select(
            Name = Facility,
            Residents.Tadmin = CumulativeOffenderTests,
            Residents.Confirmed = Positive,
            Residents.Negative = Negative,
            Residents.Recovered = PresumedRecovered,
            Residents.Active = ActiveCases
            ) %>%
        clean_scraped_df() %>%
        filter(Name != "Statewide Totals")
}

#' Scraper class for general North Carolina COVID data
#' 
#' @name north_carolina_scraper
#' @description NC has data in an isolated html table which requires minimal
#' cleaning. Guidelines and definitions for facilities are found here
#' https://www.ncdps.gov/our-organization/adult-correction/prisons/prisons-info-covid-19
#' \describe{
#'   \item{Facility}{The facility name}
#'   \item{CumulativeOffenderTests}{All tests}
#'   \item{OffendersTested}{Number of individuals tested}
#'   \item{Positive}{Individuals positive, probably not cumulative as confirmed is sometimes less than recovered}
#'   \item{Negative}{Individuals Negative}
#'   \item{PresumedRecovered}{presumed recovered following NCDHHS guidance}
#'   \item{ActiveCases}{Active residents with infection who are also quarantined}
#' }

north_carolina_scraper <- R6Class(
    "north_carolina_scraper",
    inherit = generic_scraper,
    public = list(
        log = NULL,
        initialize = function(
            log,
            url = "https://opus.doc.state.nc.us/DOPCovid19Stats/services/facilitystatsServlet",
            id = "north_carolina",
            type = "html",
            state = "NC",
            jurisdiction = "state",
            check_date = north_carolina_check_date,
            # pull the JSON data directly from the API
            pull_func = north_carolina_pull,
            # restructuring the data means pulling out the data portion of the json
            restruct_func = north_carolina_restruct,
            # Rename the columns to appropriate database names
            extract_func = north_carolina_extract){
            super$initialize(
                url = url, id = id, pull_func = pull_func, type = type,
                restruct_func = restruct_func, extract_func = extract_func,
                log = log, state = state, jurisdiction = jurisdiction,
                check_date = check_date)
        }
    )
)

if(sys.nframe() == 0){
    north_carolina <- north_carolina_scraper$new(log=TRUE)
    north_carolina$run_check_date()
    north_carolina$raw_data
    north_carolina$pull_raw()
    north_carolina$raw_data
    north_carolina$save_raw()
    north_carolina$restruct_raw()
    north_carolina$restruct_data
    north_carolina$extract_from_raw()
    north_carolina$extract_data
    north_carolina$validate_extract()
    north_carolina$save_extract()
}

