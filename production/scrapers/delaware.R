source("./R/generic_scraper.R")
source("./R/utilities.R")

delaware_check_date <- function(x, date = Sys.Date()){
    "1VhAAbzipvheVRG0UWKMLT6mCVQRMdV98lUUkk-PCYtQ" %>%
        googlesheets4::read_sheet(sheet = "DE", col_types = "Dccccccccccc") %>%
        pull(Date) %>%
        max(na.rm=TRUE) %>%
        error_on_date(date)
}

delaware_pull <- function(x){
    "1VhAAbzipvheVRG0UWKMLT6mCVQRMdV98lUUkk-PCYtQ" %>%
        googlesheets4::read_sheet(sheet = "DE", 
                                  col_types = "Dccccccccccc")
}

delaware_restruct <- function(x){
    x %>%
        filter(!is.na(Date)) %>% 
        filter(Date == max(Date))
}

delaware_extract <- function(x){
    
    x %>% 
        filter(!is.na(Name)) %>% 
        select(
            Name, Staff.Confirmed, Residents.Confirmed, 
            Residents.Tadmin, Residents.Deaths, 
            Staff.Active, Residents.Active, 
            Staff.Recovered, Residents.Recovered, 
            Residents.Initiated, Residents.Completed) %>% 
        clean_scraped_df()
}

#' Scraper class for general delaware COVID data
#' 
#' @name delaware_scraper
#' @description This will be a description of delaware data and what the scraper
#' does
#' \describe{
#'   \item{Facility_Name}{The faciilty name.}
#' }

delaware_scraper <- R6Class(
    "delaware_scraper",
    inherit = generic_scraper,
    public = list(
        log = NULL,
        initialize = function(
            log,
            url = "https://doc.delaware.gov/assets/documents/Confirmed_COVID_Cases.pdf",
            id = "delaware",
            type = "manual",
            state = "DE",
            jurisdiction = "state",
            check_date = delaware_check_date,
            # pull the JSON data directly from the API
            pull_func = delaware_pull,
            # restructuring the data means pulling out the data portion of the json
            restruct_func = delaware_restruct,
            # Rename the columns to appropriate database names
            extract_func = delaware_extract){
            super$initialize(
                url = url, id = id, pull_func = pull_func, type = type,
                restruct_func = restruct_func, extract_func = extract_func,
                log = log, state = state, jurisdiction = jurisdiction,
                check_date = check_date)
        }
    )
)

if(sys.nframe() == 0){
    delaware <- delaware_scraper$new(log=TRUE)
    delaware$run_check_date()
    delaware$raw_data
    delaware$pull_raw()
    delaware$raw_data
    delaware$save_raw()
    delaware$restruct_raw()
    delaware$restruct_data
    delaware$extract_from_raw()
    delaware$extract_data
    delaware$validate_extract()
    delaware$save_extract()
}
