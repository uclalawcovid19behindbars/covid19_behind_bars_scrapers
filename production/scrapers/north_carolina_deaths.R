source("./R/generic_scraper.R")
source("./R/utilities.R")

north_carolina_deaths_pull <- function(x){
    x %>%
        xml2::read_html()
}

north_carolina_deaths_restruct <- function(x){
    x %>%
        rvest::html_table() %>%
        .[[1]]
}

north_carolina_deaths_extract <- function(x){
    df_ <- x
    
    basic_check(
        names(df_),
        rep("Offender Deaths By Facility", 2)
    )
    
    names(df_) <- c("Name", "Residents.Deaths")
    
    df_ %>%
        as_tibble() %>%
        clean_scraped_df() %>%
        filter(!str_detect(Name, "(?i)total"))
}

#' Scraper class for general North Carolina COVID data
#' 
#' @name north_carolina_deaths_scraper
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

north_carolina_deaths_scraper <- R6Class(
    "north_carolina_deaths_scraper",
    inherit = generic_scraper,
    public = list(
        log = NULL,
        initialize = function(
            log,
            url = "https://www.ncdps.gov/our-organization/adult-correction/prisons/prisons-info-covid-19",
            id = "north_carolina_deaths",
            type = "html",
            state = "NC",
            jurisdiction = "state",
            # pull the JSON data directly from the API
            pull_func = north_carolina_deaths_pull,
            # restructuring the data means pulling out the data portion of the json
            restruct_func = north_carolina_deaths_restruct,
            # Rename the columns to appropriate database names
            extract_func = north_carolina_deaths_extract){
            super$initialize(
                url = url, id = id, pull_func = pull_func, type = type,
                restruct_func = restruct_func, extract_func = extract_func,
                log = log, state = state, jurisdiction = jurisdiction)
        }
    )
)

if(sys.nframe() == 0){
    north_carolina_deaths <- north_carolina_deaths_scraper$new(log=TRUE)
    north_carolina_deaths$raw_data
    north_carolina_deaths$pull_raw()
    north_carolina_deaths$raw_data
    north_carolina_deaths$save_raw()
    north_carolina_deaths$restruct_raw()
    north_carolina_deaths$restruct_data
    north_carolina_deaths$extract_from_raw()
    north_carolina_deaths$extract_data
    north_carolina_deaths$validate_extract()
    north_carolina_deaths$save_extract()
}

