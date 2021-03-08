source("./R/generic_scraper.R")
source("./R/utilities.R")

historical_santa_rita_jail_pull <- function(x, date = NULL, file = NULL){
    ## ran this once and then downloaded it to avoid rate limiting
    # z <- "196jMpPfuE4IMlplsd7K_3mP1l018cIbS-oTO2SuVklw" %>%
    #     googlesheets4::read_sheet(
    #         sheet = "Sheet1",
    #         col_types = "cddddddddddddddddddddddddddddddddddd"
    #         )
    # z %>%
    #     mutate(Date = ifelse(Date == "9/26", "9/26/20", Date),
    #            Date = ifelse(Date == "10/16", "10/16/20", Date),
    #            Date = ifelse(Date == "11/25", "11/25/20", Date),
    #            Date = ifelse(Date == "11/26", "11/26/20", Date),
    #            Date = lubridate::mdy(Date))
    z <- read_csv("./production/historical_scrape/historical_santa_rita_jail.csv")
}

historical_santa_rita_jail_restruct <- function(x, date = NULL){
    x %>%
        filter(!is.na(Date))
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
        "Current staff cases",
        "Offered Vaccine (Incarcerated Population, total)",
        "Offered Vaccine (Incarcerated Population, 1-day diff)",
        "1st Dose Accepted (Incarcerated Population, total)",
        "1st Dose Accepted (Incarcerated Population, 1-day diff)",
        "2nd Dose Accepted (Incarcerated Population)",
        "Percent of Population Vaccinated",
        "Percent of Vaccines Accepted"))
    
    x %>%
        select(
            Date,
            Residents.Confirmed = `Incarcerated population cases (total)`,
            Residents.Active = `Incarcerated population cases ("active")`,
            Residents.Recovered = `Total Resolved Cases`,
            Residents.Deaths = Deaths,
            Residents.Tadmin = `Tests (Incarcerated population, total)`,
            Residents.Pending = `Pending tests`,
            Residents.Population = `SRJ Population (total)`,
            Staff.Confirmed = `Staff cases (total)`,
            Residents.Initiated = `1st Dose Accepted (Incarcerated Population, total)`,
            Residents.Completed = `2nd Dose Accepted (Incarcerated Population)`
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
    historical_santa_rita_jail <- historical_santa_rita_jail_scraper$new(log = TRUE)
    # historical_santa_rita_jail$reset_date("DATE")
    historical_santa_rita_jail$reset_date("2020-04-08")
    historical_santa_rita_jail$raw_data
    historical_santa_rita_jail$pull_raw(date = historical_santa_rita_jail$date, .dated_pull = TRUE)
    historical_santa_rita_jail$raw_data
    historical_santa_rita_jail$save_raw()
    historical_santa_rita_jail$restruct_raw(date = historical_santa_rita_jail$date)
    historical_santa_rita_jail$restruct_data
    historical_santa_rita_jail$extract_from_raw(date = historical_santa_rita_jail$date)
    historical_santa_rita_jail$extract_data
    historical_santa_rita_jail$validate_extract()
    historical_santa_rita_jail$save_extract()
}

