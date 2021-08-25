source("./R/generic_scraper.R")
source("./R/utilities.R")

texas_deaths_tji_pull <- function(x){
    "1mOS1wggvyRUOpI-u2VabmnQ1yJPPEgOc2zdZjWxbAwQ" %>% 
        googlesheets4::read_sheet(
            sheet = "Inmate Deaths", 
            col_types = "cDDccccccccccccccc")
}

texas_deaths_tji_restruct <- function(x){
    x %>% 
        filter(FacilityType %in% c("State Prison")) %>% 
        group_by(Facility) %>% 
        summarise(Residents.Deaths = n()) 
}

texas_deaths_tji_extract <- function(x){
    x %>%
        select(Name = Facility,
               Residents.Deaths) %>% 
        clean_scraped_df()
}

#' Scraper class for Texas deaths data from the Texas Justice Initiative
#' 
#' @name texas_deaths_tji_scraper
#' @description Data pulled from Texas Justice Initiative. Data from TDCJ has 
#' not been reliable, so we pull data on deaths in custody from TJI instead. 
#' \describe{
#'   \item{no}{}
#'   \item{DateofDeath}{}
#'   \item{Date Entered}{}
#'   \item{Name}{}
#'   \item{Gender}{}
#'   \item{Race}{}
#'   \item{Age}{}
#'   \item{FacilityType}{}
#'   \item{Facility}{}
#'   \item{City}{}
#'   \item{County}{}
#'   \item{Geolocation}{}
#'   \item{source}{}
#'   \item{CDR link}{}
#'   \item{CDR summary}{}
#'   \item{CDR medical cause}{}
#'   \item{media}{}
#'   \item{Sentencing County}{}
#' }

texas_deaths_tji_scraper <- R6Class(
    "texas_deaths_tji_scraper",
    inherit = generic_scraper,
    public = list(
        log = NULL,
        initialize = function(
            log,
            url = "https://texasjusticeinitiative.org/publications/covid-deaths-in-texas",
            id = "texas_deaths_tji",
            type = "csv",
            state = "TX",
            jurisdiction = "state",
            check_date = NULL,
            # pull the JSON data directly from the API
            pull_func = texas_deaths_tji_pull,
            # restructuring the data means pulling out the data portion of the json
            restruct_func = texas_deaths_tji_restruct,
            # Rename the columns to appropriate database names
            extract_func = texas_deaths_tji_extract){
            super$initialize(
                url = url, id = id, pull_func = pull_func, type = type,
                restruct_func = restruct_func, extract_func = extract_func,
                log = log, state = state, jurisdiction = jurisdiction,
                check_date = check_date)
        }
    )
)

if(sys.nframe() == 0){
    texas_deaths_tji <- texas_deaths_tji_scraper$new(log=FALSE)
    texas_deaths_tji$run_check_date()
    texas_deaths_tji$raw_data
    texas_deaths_tji$pull_raw()
    texas_deaths_tji$raw_data
    texas_deaths_tji$save_raw()
    texas_deaths_tji$restruct_raw()
    texas_deaths_tji$restruct_data
    texas_deaths_tji$extract_from_raw()
    texas_deaths_tji$extract_data
    texas_deaths_tji$validate_extract()
    texas_deaths_tji$save_extract()
}

