source("./R/generic_scraper.R")
source("./R/utilities.R")

santa_rita_jail_pull <- function(x){
    "196jMpPfuE4IMlplsd7K_3mP1l018cIbS-oTO2SuVklw" %>%
        googlesheets4::read_sheet()
}

santa_rita_jail_restruct <- function(x){
    x %>%
        janitor::clean_names(case = "title") %>%
        mutate(Date = lubridate::round_date(`As of Date`, unit = "day")) %>%
        mutate(Date = as.Date(Date)) %>% 
        # Pull most recent date with non-NA Residents.Confirmed 
        filter(!is.na(`Confirmed Cases Incarcerated Population Cumulative`)) %>% 
        filter(Date == max(Date, na.rm = TRUE))
}

santa_rita_jail_extract <- function(x, exp_date = Sys.Date()){
    
    error_on_date(x$Date, exp_date)
    
    x %>%
        select(
            Residents.Confirmed = `Confirmed Cases Incarcerated Population Cumulative`,
            Residents.Active = `Active Cases Incarcerated Population Current`,
            Residents.Recovered = `Resolved Cases Incarcerated Population Cumulative`,
            Residents.Deaths = `Deaths Incarcerated Population Cumulative`,
            Residents.Tadmin = `Tests Incarcerated Population Cumulative`,
            Residents.Pending = `Pending Tests Incarcerated Population Current`,
            Residents.Population = `Population Incarcerated Population Current`,
            # Staff.Confirmed = `Confirmed Cases Staff Cumulative`,
            # Staff.Active = `Active Cases Staff Current`, 
            Residents.Partial.Drop = `Partially Vaccinated Total Incarcerated Population Current`,
            Residents.Completed = `Fully Vaccinated Total Incarcerated Population Cumulative`, 
            ) %>% 
        rowwise() %>% 
        mutate(Residents.Initiated = sum(Residents.Partial.Drop, Residents.Completed, na.rm = T)) %>% 
        mutate(Residents.Initiated = ifelse(
            is.na(Residents.Partial.Drop) & is.na(Residents.Completed), NA, Residents.Initiated)) %>% 
        mutate_all(as.numeric) %>%
        select(-ends_with("Drop")) %>% 
        mutate(Name = "SANTA RITA JAIL") 
}

#' Scraper class for general santa_rita_jail COVID data
#' 
#' @name santa_rita_jail_scraper
#' @description This will be a description of santa_rita_jail data and what the scraper
#' does
#' \describe{
#'   \item{Facility_Name}{The faciilty name.}
#' }

santa_rita_jail_scraper <- R6Class(
    "santa_rita_jail_scraper",
    inherit = generic_scraper,
    public = list(
        log = NULL,
        initialize = function(
            log,
            url = "https://www.davisvanguard.org/tag/covid-19/",
            id = "santa_rita_jail",
            type = "csv",
            state = "CA",
            jurisdiction = "county",
            check_date = NULL,
            # pull the JSON data directly from the API
            pull_func = santa_rita_jail_pull,
            # restructuring the data means pulling out the data portion of the json
            restruct_func = santa_rita_jail_restruct,
            # Rename the columns to appropriate database names
            extract_func = santa_rita_jail_extract){
            super$initialize(
                url = url, id = id, pull_func = pull_func, type = type,
                restruct_func = restruct_func, extract_func = extract_func,
                log = log, state = state, jurisdiction = jurisdiction,
                check_date = check_date)
        }
    )
)

if(sys.nframe() == 0){
    santa_rita_jail <- santa_rita_jail_scraper$new(log=TRUE)
    santa_rita_jail$run_check_date()
    santa_rita_jail$raw_data
    santa_rita_jail$pull_raw()
    santa_rita_jail$raw_data
    santa_rita_jail$save_raw()
    santa_rita_jail$restruct_raw()
    santa_rita_jail$restruct_data
    santa_rita_jail$extract_from_raw()
    santa_rita_jail$extract_data
    santa_rita_jail$validate_extract()
    santa_rita_jail$save_extract()
}

