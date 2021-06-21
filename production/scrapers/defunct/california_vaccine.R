source("./R/generic_scraper.R")
source("./R/utilities.R")

california_vaccine_check_date <- function(x, date = Sys.Date()){
    "1VhAAbzipvheVRG0UWKMLT6mCVQRMdV98lUUkk-PCYtQ" %>%
        googlesheets4::read_sheet(sheet = "CA Vaccine", col_types = "Dccc") %>%
        pull(Date) %>%
        max(na.rm = TRUE) %>%
        error_on_date(date)
}

california_vaccine_pull <- function(x){
    stop_defunct_scraper(x)
}

california_vaccine_restruct <- function(x){
    x %>%
        filter(!is.na(Date)) %>% 
        filter(Date == max(Date))
}

california_vaccine_extract <- function(x, exp_date = Sys.Date()){

    check_names(x, c(
        "Date", 
        "Facility", 
        "Staff Received 1st Dose", 
        "Incarcerated Individuals Received 1st Dose")
    )
    
    x %>%
        select(
            Name = `Facility`,
            Residents.Initiated = `Incarcerated Individuals Received 1st Dose`,
            Staff.Initiated = `Staff Received 1st Dose`) %>% 
        clean_scraped_df()
}

#' Scraper class for California vaccine data
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

california_vaccine_scraper <- R6Class(
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
            check_date = california_vaccine_check_date,
            # pull the JSON data directly from the API
            pull_func = california_vaccine_pull,
            # restructuring the data means pulling out the data portion of the json
            restruct_func = california_vaccine_restruct,
            # Rename the columns to appropriate database names
            extract_func = california_vaccine_extract){
            super$initialize(
                url = url, id = id, pull_func = pull_func, type = type,
                restruct_func = restruct_func, extract_func = extract_func,
                log = log, state = state, jurisdiction = jurisdiction,
                check_date = check_date)
        }
    )
)

if(sys.nframe() == 0){
    california_vaccine <- california_vaccine_scraper$new(log=TRUE)
    california_vaccine$raw_data
    california_vaccine$pull_raw()
    california_vaccine$raw_data
    california_vaccine$save_raw()
    california_vaccine$restruct_raw()
    california_vaccine$restruct_data
    california_vaccine$extract_from_raw()
    california_vaccine$extract_data
    california_vaccine$validate_extract()
    california_vaccine$save_extract()
}
