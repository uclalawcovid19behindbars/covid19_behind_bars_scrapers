source("./R/generic_scraper.R")
source("./R/utilities.R")

ohio_vaccine_check_date <- function(x, date = Sys.Date()){
    dat <-  "1VhAAbzipvheVRG0UWKMLT6mCVQRMdV98lUUkk-PCYtQ" %>%
        googlesheets4::read_sheet(sheet = "OH Vaccine", 
                                  col_types = "Dccc")

    dat %>%
        filter(!is.na(Date)) %>% 
        filter(Date == max(Date)) %>%
        pull(Date) %>%
        error_on_date(expected_date = date)
}

ohio_vaccine_pull <- function(x){
    "1VhAAbzipvheVRG0UWKMLT6mCVQRMdV98lUUkk-PCYtQ" %>%
        googlesheets4::read_sheet(sheet = "OH Vaccine", 
                                  col_types = "Dccc")
}

ohio_vaccine_restruct <- function(x){
    x %>%
        filter(!is.na(Date)) %>% 
        filter(Date == max(Date))
}

ohio_vaccine_extract <- function(x, exp_date = Sys.Date()){
    
    # error_on_date(first(x$Date), exp_date)
    
    check_names(x, c(
        "Date", 
        "Name", 
        "Inmates", 
        "Staff")
    )
    
    x %>%
        select(
            Name = `Name`,
            Residents.Initiated = `Inmates`,
            Staff.Initiated = `Staff`) %>% 
        clean_scraped_df()
}

#' Scraper class for Ohio vaccine data 
#' 
#' @name ohio_vaccine_scraper
#' @description Grabs manually entered Google Sheet data for OH vaccines. Raw data
#' is received via email from the Ohio DOC weekly. Only statewide totals for 
#' incarcerated people and staff are shared.  
#' 
#' \describe{
#'   \item{Inmates}{Cumulative 1st doses received by staff}
#'   \item{Staff}{Cumulative 2nd doses received by staff}
#' }

ohio_vaccine_scraper <- R6Class(
    "ohio_vaccine_scraper",
    inherit = generic_scraper,
    public = list(
        log = NULL,
        initialize = function(
            log,
            url = "Ohio Department of Rehabilitation and Correction",
            id = "ohio_vaccine",
            type = "manual",
            state = "OH",
            jurisdiction = "state",
            check_date = ohio_vaccine_check_date,
            # pull the JSON data directly from the API
            pull_func = ohio_vaccine_pull,
            # restructuring the data means pulling out the data portion of the json
            restruct_func = ohio_vaccine_restruct,
            # Rename the columns to appropriate database names
            extract_func = ohio_vaccine_extract){
            super$initialize(
                url = url, id = id, pull_func = pull_func, type = type,
                restruct_func = restruct_func, extract_func = extract_func,
                log = log, state = state, jurisdiction = jurisdiction,
                check_date = check_date)
        }
    )
)

if(sys.nframe() == 0){
    ohio_vaccine <- ohio_vaccine_scraper$new(log=TRUE)
    ohio_vaccine$run_check_date()
    ohio_vaccine$raw_data
    ohio_vaccine$pull_raw()
    ohio_vaccine$raw_data
    ohio_vaccine$save_raw()
    ohio_vaccine$restruct_raw()
    ohio_vaccine$restruct_data
    ohio_vaccine$extract_from_raw()
    ohio_vaccine$extract_data
    ohio_vaccine$validate_extract()
    ohio_vaccine$save_extract()
}
