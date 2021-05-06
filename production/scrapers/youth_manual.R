source("./R/generic_scraper.R")
source("./R/utilities.R")

youth_manual_pull <- function(x){
    manual_sheet <- googlesheets4::read_sheet(ss = "17mC-uHp1jhMQO8JGqn4is6pJLrKHP0G0TR57R01MxrY",
                                              sheet = "Permanent", 
                                              col_types = "c")
    return(manual_sheet)
}

youth_manual_restruct <- function(x){
    x %>%
        janitor::clean_names(case = "title") %>%
        # filter(!stringr::str_detect(State, "(?i)total")) %>% 
        mutate(Date = lubridate::mdy(`Date of Last Positive Case Last Update`))
}

youth_manual_extract <- function(x, exp_date = Sys.Date()){
    
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
            Staff.Confirmed = `Confirmed Cases Staff Cumulative`,
            Staff.Active = `Active Cases Staff Current`, 
            Residents.Partial.Drop = `Partially Vaccinated Incarcerated Population Cumulative`, 
            Residents.Completed = `Fully Vaccinated Incarcerated Population Cumulative`, 
        ) %>% 
        rowwise() %>% 
        mutate(Residents.Initiated = sum(Residents.Partial.Drop, Residents.Completed, na.rm = T)) %>% 
        mutate(Residents.Initiated = ifelse(
            is.na(Residents.Partial.Drop) & is.na(Residents.Completed), NA, Residents.Initiated)) %>% 
        mutate_all(as.numeric) %>%
        select(-ends_with("Drop")) %>% 
        mutate(Name = "SANTA RITA JAIL") 
}

#' Scraper class for general youth_manual COVID data
#' 
#' @name youth_manual_scraper
#' @description This will be a description of youth_manual data and what the scraper
#' does
#' \describe{
#'   \item{Facility_Name}{The faciilty name.}
#' }

youth_manual_scraper <- R6Class(
    "youth_manual_scraper",
    inherit = generic_scraper,
    public = list(
        log = NULL,
        initialize = function(
            log,
            url = "Youth team manual data collection",
            id = "youth_manual",
            type = "csv",
            state = "",
            jurisdiction = "state",
            pull_func = youth_manual_pull,
            restruct_func = youth_manual_restruct,
            extract_func = youth_manual_extract){
            super$initialize(
                url = url, id = id, pull_func = pull_func, type = type,
                restruct_func = restruct_func, extract_func = extract_func,
                log = log, state = state, jurisdiction = jurisdiction)
        }
    )
)

if(sys.nframe() == 0){
    youth_manual <- youth_manual_scraper$new(log=TRUE)
    youth_manual$raw_data
    youth_manual$pull_raw()
    youth_manual$raw_data
    youth_manual$save_raw()
    youth_manual$restruct_raw()
    youth_manual$restruct_data
    youth_manual$extract_from_raw()
    youth_manual$extract_data
    youth_manual$validate_extract()
    youth_manual$save_extract()
}

