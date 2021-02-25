source("./R/generic_scraper.R")
source("./R/utilities.R")

historical_santa_rita_jail_pull <- function(x){
    "196jMpPfuE4IMlplsd7K_3mP1l018cIbS-oTO2SuVklw" %>%
        googlesheets4::read_sheet()
}

historical_santa_rita_jail_restruct <- function(x, date = NULL){
    if(date > lubridate::ymd("2020-12-28")){
        stop(
            "historical_santa_rita_jail should not be run past 2020-12-28 as to",
            "not overlap", "with santa_rita_jail scraper")
    }
    x %>%
        filter(!is.na(Date)) %>%
        mutate(Date = lubridate::ymd(Date)) 
}

historical_santa_rita_jail_extract <- function(x, date){
    
    check_names(x, c(
        "Date", 
        "SRJ Population (total)", 
        "SRJ Population (diff)", 
        "Tests (Incarcerated population, total)", 
        "Tests (Incarcerated population, difference)", 
        "Pending tests", 
        "Percentage of population tested within the past: 7 days", 
        "Percentage of population tested within the past: 14 days", 
        "Percentage of population tested within the past: 30 days", 
        "Incarcerated population cases (total)", 
        'Incarcerated population cases ("active")',
        "1-day change in 'active' cases",
        "Incarcerated population hospitalizations (total)",
        "Staff cases (total)",
        "1-day change in staff cases",
        "Red patients (current)",
        "Dark Red patients (current)",
        "Orange patients (current)",
        "1-day change in Orange patients",
        "Percent of Orange patients in population",
        "Total Resolved Cases",
        "Released while Active",
        "Percentage of total cases released while active",
        "Released after Resolved",
        "Percentage of total cases released after resolved",
        "Resolved in Custody",
        "Percentage of total cases resolved in custody",
        "Deaths",
        "Current staff cases"))
    
    x %>%
        select(
            Residents.Confirmed = `Incarcerated population cases (total)`,
            Residents.Active = `Incarcerated population cases ("active")`,
            Residents.Recovered = `Total Resolved Cases`,
            Residents.Deaths = Deaths,
            Residents.Tadmin = `Tests (Incarcerated population, total)`,
            Residents.Pending = `Pending tests`,
            Residents.Population = `SRJ Population (total)`,
            Staff.Confirmed = `Staff cases (total)`
        ) %>%
        mutate(Name = "SANTA RITA JAIL") %>%
        filter(Date == date) %>%
        select(-Date)
}

#' Scraper class for general historical_santa_rita_jail COVID data
#' 
#' @name historical_santa_rita_jail_scraper
#' @description This will be a description of historical_santa_rita_jail data and what the scraper
#' does
#' \describe{
#'   \item{Facility_Name}{The faciilty name.}
#' }

historical_santa_rita_jail_scraper <- R6Class(
    "historical_santa_rita_jail_scraper",
    inherit = generic_scraper,
    public = list(
        log = NULL,
        initialize = function(
            log,
            url = "https://www.davisvanguard.org/tag/covid-19/",
            id = "historical_santa_rita_jail",
            type = "csv",
            state = "CA",
            jurisdiction = "county",
            # pull the JSON data directly from the API
            pull_func = historical_santa_rita_jail_pull,
            # restructuring the data means pulling out the data portion of the json
            restruct_func = historical_santa_rita_jail_restruct,
            # Rename the columns to appropriate database names
            extract_func = historical_santa_rita_jail_extract){
            super$initialize(
                url = url, id = id, pull_func = pull_func, type = type,
                restruct_func = restruct_func, extract_func = extract_func,
                log = log, state = state, jurisdiction = jurisdiction)
        }
    )
)

if(sys.nframe() == 0){
    historical_santa_rita_jail <- historical_santa_rita_jail_scraper$new(log=TRUE)
    historical_santa_rita_jail$reset_date("SET_DATE_HERE")
    historical_santa_rita_jail$raw_data
    historical_santa_rita_jail$pull_raw(date = scraper$date, file = NULL, .dated_pull = TRUE)
    historical_santa_rita_jail$raw_data
    historical_santa_rita_jail$save_raw()
    historical_santa_rita_jail$restruct_raw(date = historical_santa_rita$date)
    historical_santa_rita_jail$restruct_data
    historical_santa_rita_jail$extract_from_raw(date = historical_santa_rita$date)
    historical_santa_rita_jail$extract_data
    historical_santa_rita_jail$validate_extract()
    historical_santa_rita_jail$save_extract()
}

