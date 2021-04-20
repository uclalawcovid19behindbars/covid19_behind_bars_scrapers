source("./R/generic_scraper.R")
source("./R/utilities.R")

santa_clara_county_jail_pull <- function(x){
    "1-Z4rttjVPf4gplH59Qdr0hhMHnDnZZr7rAmO1BAp5ls" %>%
        googlesheets4::read_sheet()
}

santa_clara_county_jail_restruct <- function(x){
    x %>%
        mutate(Date = lubridate::round_date(`As of Date`, unit = "day")) %>%
        mutate(Date = as.Date(Date)) %>%
        filter(Date == max(Date))
}

santa_clara_county_jail_extract <- function(x, exp_date = Sys.Date()){
    
    error_on_date(x$Date, exp_date)
    
    check_names(x, c(
        "As of Date", 
        "Facility Name", 
        "County", 
        "Active Cases (Incarcerated population, current)", 
        "Population (Incarcerated population, current)", 
        "Population (Incarcerated population, 1-day diff)", 
        "Tests (Incarcerated population, cumulative)", 
        "New Tests (Incarcerated population, 1-day diff)", 
        "New Tests (Incarcerated population, 7-day diff)", 
        "Tests (Incarcerated population, current, positives)", 
        "Tests (Incarcerated population, current, negatives)", 
        "Cases Confirmed at Intake (Incarcerated population, cumulative)", 
        "Cases Confirmed at Intake (Incarcerated population, 1-day diff)", 
        "Cases Confirmed in Custody (Incarcerated population, cumulative)", 
        "Cases Confirmed in Custody (Incarcerated population, 1-day diff)", 
        "Confirmed Cases (Incarcerated population, cumulative)", 
        "Confirmed Cases (Incarcerated population, 1-day diff)", 
        "Tests (Custody Staff, cumulative)", 
        "Population (Custody Staff, current)", 
        "Offered Vaccines (Incarcerated population, cumulative)", 
        "Fully Vaccinated (Incarcerated population, cumulative)", 
        "Partially Vaccinated (Incarcerated population, current)", 
        "Fully Vaccinated (Custody Staff, cumulative)", 
        "Notes", 
        "Date"
    ))
    
    x %>%
        mutate(across(c(
            `Partially Vaccinated (Incarcerated population, current)`, 
            `Fully Vaccinated (Incarcerated population, cumulative)`
        ), ~ ifelse(is.na(.), 0, .))) %>%
        mutate(Residents.Initiated = `Partially Vaccinated (Incarcerated population, current)` + 
                   `Fully Vaccinated (Incarcerated population, cumulative)`) %>% 
        select(
            Residents.Confirmed = `Confirmed Cases (Incarcerated population, cumulative)`,
            Residents.Active = `Active Cases (Incarcerated population, current)`,
            Residents.Tadmin = `Tests (Incarcerated population, cumulative)`,
            Residents.Population = `Tests (Incarcerated population, cumulative)`,
            Staff.Tested = `Tests (Custody Staff, cumulative)`,
            Residents.Completed = `Fully Vaccinated (Incarcerated population, cumulative)`, 
            Residents.Initiated, 
            Staff.Completed = `Fully Vaccinated (Custody Staff, cumulative)`, 
            Staff.Population = `Population (Custody Staff, current)`
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
            url = "https://www.davisvanguard.org/tag/covid-19/",
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
