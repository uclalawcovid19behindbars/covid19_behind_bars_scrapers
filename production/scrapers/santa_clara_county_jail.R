source("./R/generic_scraper.R")
source("./R/utilities.R")

santa_clara_county_jail_check_date <- function(sheet_id, date=Sys.Date()){
        googlesheets4::read_sheet(sheet_id, skip = 1) %>%
        janitor::clean_names(case = "title") %>%
        mutate(Date = lubridate::round_date(`As of Date`, unit = "day")) %>%
        mutate(Date = as.Date(Date)) %>%
        # Pull most recent date with non-NA Residents.Confirmed 
        filter(!is.na(`Confirmed Cases Incarcerated Population Cumulative`)) %>% 
        filter(Date == max(Date, na.rm = T)) %>%
        pull(Date) %>%
        first() %>%
        error_on_date(date)
}

santa_clara_county_jail_pull <- function(sheet_id){
        googlesheets4::read_sheet(sheet_id, skip = 1)
}

santa_clara_county_jail_restruct <- function(x){
    x %>%
        janitor::clean_names(case = "title") %>%
        mutate(Date = lubridate::round_date(`As of Date`, unit = "day")) %>%
        mutate(Date = as.Date(Date)) %>%
        # Pull most recent date with non-NA Residents.Confirmed 
        filter(!is.na(`Confirmed Cases Incarcerated Population Cumulative`)) %>% 
        filter(Date == max(Date))
}

santa_clara_county_jail_extract <- function(x, exp_date = Sys.Date()){
    
    x %>% 
        select(
            Residents.Active = `Active Cases Incarcerated Population Current`, 
            Residents.Confirmed = `Confirmed Cases Incarcerated Population Cumulative`,
            Residents.Tadmin = `Tests Incarcerated Population Cumulative`,
            Residents.Population = `Population Incarcerated Population Current`,
            # Residents.Partial.Drop = `Partially Vaccinated Total Incarcerated Population Current`, 
            Residents.Completed = `Fully Vaccinated Incarcerated Population Cumulative`,
            # Staff.Completed = `Fully Vaccinated Custody Staff Cumulative`, 
        ) %>% 
        rowwise() %>% 
        mutate(Residents.Initiated = Residents.Completed) %>% 
        mutate_all(as.numeric) %>%
        select(-ends_with("Drop")) %>% 
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
            # data is hosted at https://covidincustody.org/data
            url = "1-Z4rttjVPf4gplH59Qdr0hhMHnDnZZr7rAmO1BAp5ls",
            id = "santa_clara_county_jail",
            type = "csv",
            state = "CA",
            jurisdiction = "county",
            check_date = santa_clara_county_jail_check_date,
            # pull the JSON data directly from the API
            pull_func = santa_clara_county_jail_pull,
            # restructuring the data means pulling out the data portion of the json
            restruct_func = santa_clara_county_jail_restruct,
            # Rename the columns to appropriate database names
            extract_func = santa_clara_county_jail_extract){
            super$initialize(
                url = url, id = id, pull_func = pull_func, type = type,
                restruct_func = restruct_func, extract_func = extract_func,
                log = log, state = state, jurisdiction = jurisdiction,
                check_date = check_date)
        }
    )
)

if(sys.nframe() == 0){
    santa_clara_county_jail <- santa_clara_county_jail_scraper$new(log=TRUE)
    santa_clara_county_jail$run_check_date()
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
