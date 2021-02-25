source("./R/generic_scraper.R")
source("./R/utilities.R")

historical_ca_vaccine_pull <- function(x, date = NULL, file = NULL){
    "1VhAAbzipvheVRG0UWKMLT6mCVQRMdV98lUUkk-PCYtQ" %>%
        googlesheets4::read_sheet(sheet = "CA Vaccine", 
                                  col_types = "Dccc")
}

historical_ca_vaccine_restruct <- function(x, date = NULL){
    x %>% 
        select(Date, Facility, `Staff Received 1st Dose`, `Incarcerated Individuals Received 1st Dose`)
}

historical_ca_vaccine_extract <- function(x, date){
    x %>%
        filter(Date == date) %>% 
        {suppressWarnings(mutate_at(., vars(starts_with("Res")), as.numeric))} %>%
        {suppressWarnings(mutate_at(., vars(starts_with("Staff")), as.numeric))} %>%
        select(
            Name = `Facility`,
            Residents.Initiated = `Incarcerated Individuals Received 1st Dose`,
            Staff.Initiated = `Staff Received 1st Dose`) %>% 
        clean_scraped_df()
}

#' Historical scraper class for California vaccine data
#' 
#' @name california_vaccine_scraper
#' @description CA reports statewide vaccination data for incarcerated people and
#' staff through daily updates in paragraph / unstructured form. Note that we only update 
#' CA vaccine data if it is less than 7 days old. This scraper grabs manually entered 
#' Google Sheet data. 
#' \describe{
#'   \item{Staff Received 1st Dose}{Number of staff that ave received first round vaccines statewide.}
#'   \item{Incarcerated Individuals Received 1st Dose}{Number of incarcerated individuals that ave received first round vaccines statewide.}
#' }

historical_ca_vaccine_scraper <- R6Class(
    "california_vaccine_scraper",
    inherit = generic_scraper,
    public = list(
        log = NULL,
        initialize = function(
            log,
            url = "https://www.cdcr.ca.gov/covid19/",
            id = "california_vaccine",
            type = "manual",
            state = "CA",
            jurisdiction = "state",
            pull_func = historical_ca_vaccine_pull,
            restruct_func = historical_ca_vaccine_restruct,
            extract_func = historical_ca_vaccine_extract){
            super$initialize(
                url = url, id = id, pull_func = pull_func, type = type,
                restruct_func = restruct_func, extract_func = extract_func,
                log = log, state = state, jurisdiction  = jurisdiction)
        }
    )
)

if(sys.nframe() == 0){
    california_vaccine <- historical_ca_vaccine_scraper$new(log=TRUE)
    california_vaccine$reset_date("SET_DATE_HERE")
    california_vaccine$raw_data
    california_vaccine$pull_raw(date = california_vaccine$date, .dated_pull = TRUE)
    california_vaccine$raw_data
    california_vaccine$save_raw()
    california_vaccine$restruct_raw(date = california_vaccine$date)
    california_vaccine$restruct_data
    california_vaccine$extract_from_raw(date = california_vaccine$date)
    california_vaccine$extract_data
    california_vaccine$validate_extract()
    california_vaccine$save_extract()
}
