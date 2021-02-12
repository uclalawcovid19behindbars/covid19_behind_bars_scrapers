source("./R/generic_scraper.R")
source("./R/utilities.R")

minnesota_vaccine_pull <- function(x){
    "1VhAAbzipvheVRG0UWKMLT6mCVQRMdV98lUUkk-PCYtQ" %>%
        googlesheets4::read_sheet(sheet = "Minnesota Vaccine")
}

minnesota_vaccine_restruct <- function(x){
    x %>%
        mutate(Date = lubridate::ymd(`Date`)) %>%
        filter(!is.na(Date)) %>% 
        filter(Date == max(Date))
}

minnesota_vaccine_extract <- function(x, exp_date = Sys.Date()){
    
    error_on_date(first(x$Date), exp_date)
    
    check_names(x, c(
        "Date", 
        "Facility", 
        "Staff Received 1st Dose", 
        "Staff Received 2nd Dose", 
        "Client Received 1st Dose", 
        "Client Received 2nd Dose"
        ))
    
    x %>%
        select(
            Name = `Facility`,
            Residents.Initiated = `Client Received 1st Dose`,
            Residents.Completed = `Client Received 2nd Dose`, 
            Staff.Initiated = `Staff Received 1st Dose`, 
            Staff.Completed = `Staff Received 2nd Dose`) %>% 
        clean_scraped_df()
}

#' Scraper class for Minnesota vaccine data 
#' 
#' @name minnesota_vaccine_scraper
#' @description Grabs manually entered Google Sheet data for MN vaccines 
#' 
#' \describe{
#'   \item{Facility}{The facilty name}
#'   \item{Staff Received 1st Dose}{Cumulative 1st doses received by staff}
#'   \item{Staff Received 2nd Dose}{Cumulative 2nd doses received by staff}
#'   \item{Client Received 2nd Dose}{Cumulative 1st doses received by incarcerated people}
#'   \item{Client Received 2nd Dose}{Cumulative 2nd doses received by incarcerated people}
#' }

minnesota_vaccine_scraper <- R6Class(
    "minnesota_vaccine_scraper",
    inherit = generic_scraper,
    public = list(
        log = NULL,
        initialize = function(
            log,
            url = "https://mn.gov/doc/about/covid-19-updates/",
            id = "minnesota_vaccine",
            type = "csv",
            state = "MN",
            jurisdiction = "state",
            # pull the JSON data directly from the API
            pull_func = minnesota_vaccine_pull,
            # restructuring the data means pulling out the data portion of the json
            restruct_func = minnesota_vaccine_restruct,
            # Rename the columns to appropriate database names
            extract_func = minnesota_vaccine_extract){
            super$initialize(
                url = url, id = id, pull_func = pull_func, type = type,
                restruct_func = restruct_func, extract_func = extract_func,
                log = log, state = state, jurisdiction = jurisdiction)
        }
    )
)

if(sys.nframe() == 0){
    minnesota_vaccine <- minnesota_vaccine_scraper$new(log=TRUE)
    minnesota_vaccine$raw_data
    minnesota_vaccine$pull_raw()
    minnesota_vaccine$raw_data
    minnesota_vaccine$save_raw()
    minnesota_vaccine$restruct_raw()
    minnesota_vaccine$restruct_data
    minnesota_vaccine$extract_from_raw()
    minnesota_vaccine$extract_data
    minnesota_vaccine$validate_extract()
    minnesota_vaccine$save_extract()
}
