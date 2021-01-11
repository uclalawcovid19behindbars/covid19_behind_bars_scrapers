source("./R/generic_scraper.R")
source("./R/utilities.R")

santa_clara_county_jail_pull <- function(x){
    "1-Z4rttjVPf4gplH59Qdr0hhMHnDnZZr7rAmO1BAp5ls" %>%
        googlesheets4::read_sheet()
}

santa_clara_county_jail_restruct <- function(x){
    x %>%
        mutate(Date = lubridate::round_date(`Date`, unit = "day")) %>%
        mutate(Date = as.Date(Date)) %>%
        filter(Date == max(Date))
}

santa_clara_county_jail_extract <- function(x, exp_date = Sys.Date()){
    
    error_on_date(x$Date, exp_date)
    
    check_names(x, c(
        "Date", 
        "Active Cases In Custody", 
        "Incarcerated People In Custody", 
        "Population Change", 
        "Total Tests Completed", 
        "Positive Test Results", 
        "Negative Test Results", 
        "Cumulative Cases", 
        "New Cases", 
        "Notes"))
    
    x %>%
        select(
            Residents.Confirmed = `Cumulative Cases`,
            Residents.Active = `Active Cases In Custody`,
            Residents.Tadmin = `Total Tests Completed`,
            Residents.Population = `Incarcerated People In Custody`
            ) %>%
        mutate(Name = "SANTA CLARA COUNTY JAIL")
}

#' Scraper class for general santa_clara_county_jail COVID data
#' 
#' @name santa_clara_county_jail_scraper
#' @description This will be a description of santa_clara_county_jail data and what the scraper
#' does
#' \describe{
#'   \item{Facility_Name}{The faciilty name.}
#' }

santa_clara_county_jail_scraper <- R6Class(
    "santa_clara_county_jail_scraper",
    inherit = generic_scraper,
    public = list(
        log = NULL,
        initialize = function(
            log,
            url = "https://countysheriff.sccgov.org/covid-19/covid-19-dashboard",
            id = "santa_clara_county_jail",
            type = "csv",
            state = "CA",
            jurisdiction = "county",
            # pull the JSON data directly from the API
            pull_func = santa_clara_county_jail_pull,
            # restructuring the data means pulling out the data portion of the json
            restruct_func = santa_clara_county_jail_restruct,
            # Rename the columns to appropriate database names
            extract_func = santa_clara_county_jail_extract){
            super$initialize(
                url = url, id = id, pull_func = pull_func, type = type,
                restruct_func = restruct_func, extract_func = extract_func,
                log = log, state = state, jurisdiction = jurisdiction)
        }
    )
)

if(sys.nframe() == 0){
    santa_clara_county_jail <- santa_clara_county_jail_scraper$new(log=TRUE)
    santa_clara_county_jail$raw_data
    santa_clara_county_jail$pull_raw()
    santa_clara_county_jail$raw_data
    santa_clara_county_jail$save_raw()
    santa_clara_county_jail$restruct_raw()
    santa_clara_county_jail$restruct_data
    santa_clara_county_jail$extract_from_raw()
    santa_clara_county_jail$extract_data
    santa_clara_county_jail$validate_extract()
    santa_clara_county_jail$save_extract()
}

