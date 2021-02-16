source("./R/generic_scraper.R")
source("./R/utilities.R")

north_carolina_vaccine_pull <- function(x){
    "1VhAAbzipvheVRG0UWKMLT6mCVQRMdV98lUUkk-PCYtQ" %>%
        googlesheets4::read_sheet(sheet = "NC Vaccine", 
                                  col_types = "Dccc")
}

north_carolina_vaccine_restruct <- function(x){
    x %>%
        filter(!is.na(Date)) %>% 
        filter(Date == max(Date))
}

north_carolina_vaccine_extract <- function(x, exp_date = Sys.Date()){
    
    error_on_date(first(x$Date), exp_date)
    
    check_names(x, c(
        "Date", 
        "Name", 
        "Offenders", 
        "Staff")
    )
    
    x %>%
        select(
            Name = `Name`,
            Residents.Initiated = `Offenders`,
            Staff.Initiated = `Staff`) %>% 
        clean_scraped_df()
}

#' Scraper class for North Carolina vaccine data 
#' 
#' @name north_carolina_vaccine_scraper
#' @description Grabs manually entered Google Sheet data for NC vaccines. Raw data
#' is received via email from the NC DOC weekly. Only statewide totals for incarcerated 
#' people and staff are shared.  
#' 
#' \describe{
#'   \item{Offenders}{Number of incarcerated residents who received first doses}
#'   \item{Staff}{Number of staff who received first doses}
#' }

north_carolina_vaccine_scraper <- R6Class(
    "north_carolina_vaccine_scraper",
    inherit = generic_scraper,
    public = list(
        log = NULL,
        initialize = function(
            log,
            url = "North Carolina Department of Public Safety",
            id = "north_carolina_vaccine",
            type = "manual",
            state = "NC",
            jurisdiction = "state",
            # pull the JSON data directly from the API
            pull_func = north_carolina_vaccine_pull,
            # restructuring the data means pulling out the data portion of the json
            restruct_func = north_carolina_vaccine_restruct,
            # Rename the columns to appropriate database names
            extract_func = north_carolina_vaccine_extract){
            super$initialize(
                url = url, id = id, pull_func = pull_func, type = type,
                restruct_func = restruct_func, extract_func = extract_func,
                log = log, state = state, jurisdiction = jurisdiction)
        }
    )
)

if(sys.nframe() == 0){
    north_carolina_vaccine <- north_carolina_vaccine_scraper$new(log=TRUE)
    north_carolina_vaccine$raw_data
    north_carolina_vaccine$pull_raw()
    north_carolina_vaccine$raw_data
    north_carolina_vaccine$save_raw()
    north_carolina_vaccine$restruct_raw()
    north_carolina_vaccine$restruct_data
    north_carolina_vaccine$extract_from_raw()
    north_carolina_vaccine$extract_data
    north_carolina_vaccine$validate_extract()
    north_carolina_vaccine$save_extract()
}
