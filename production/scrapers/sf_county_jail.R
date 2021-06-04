source("./R/generic_scraper.R")
source("./R/utilities.R")

sf_county_jail_pull <- function(x){
    z = "1F2iSIveA0jglb2SILgN4fobYozSMNS_Ff-7x8OGs7tM" %>%
        googlesheets4::read_sheet()
    
    # for some reason this is saved weird and we need to do some
    # minimal edits in order to save teh file
    for(c in names(z)){
        for(j in 1:length(z[[c]])){
            if(length(z[[c]][[j]]) == 0){
                z[[c]][[j]] <- NA
            }
        }
    }
    
    z %>%
        mutate_all(unlist)
}

sf_county_jail_restruct <- function(x){
    x %>%
        janitor::clean_names(case = "title") %>%
        mutate(Date = lubridate::mdy(`As of Date`)) %>%
        # Pull most recent date with non-NA Residents.Confirmed 
        filter(!is.na(`Confirmed Cases Incarcerated Population Cumulative`)) %>% 
        filter(Date == max(Date))
}

sf_county_jail_extract <- function(x, exp_date = Sys.Date()){
    
    error_on_date(x$Date, exp_date)
    
    x %>%
        select(
            Residents.Confirmed = `Confirmed Cases Incarcerated Population Cumulative`,
            Residents.Active = `Active Cases Incarcerated Population Current`,
            Residents.Tadmin = `Tests Incarcerated Population Cumulative`, 
            Staff.Population = `Total Sfso Employees`, 
            Staff.Confirmed = `Sfso Employees Total Positive Results`, 
            Residents.Partial.Drop = `Partially Vaccinated Incarcerated Population Current`, 
            Residents.Completed = `Fully Vaccinated Incarcerated Population Cumulative`, 
            Residents.Deaths = `Deaths Incarcerated Population Cumulative`
            ) %>% 
        rowwise() %>% 
        mutate(Residents.Initiated = sum(Residents.Partial.Drop, Residents.Completed, na.rm = T)) %>% 
        mutate(Residents.Initiated = ifelse(
            is.na(Residents.Partial.Drop) & is.na(Residents.Completed), NA, Residents.Initiated)) %>% 
        mutate_all(as.numeric) %>%
        select(-ends_with("Drop")) %>% 
        mutate(Name = "SF COUNTY JAIL") 
}

#' Scraper class for general sf_county_jail COVID data
#' 
#' @name sf_county_jail_scraper
#' @description This will be a description of sf_county_jail data and what the scraper
#' does
#' \describe{
#'   \item{Facility_Name}{The faciilty name.}
#' }

sf_county_jail_scraper <- R6Class(
    "sf_county_jail_scraper",
    inherit = generic_scraper,
    public = list(
        log = NULL,
        initialize = function(
            log,
            url = "https://www.davisvanguard.org/tag/covid-19/",
            id = "sf_county_jail",
            type = "csv",
            state = "CA",
            jurisdiction = "county",
            # pull the JSON data directly from the API
            pull_func = sf_county_jail_pull,
            # restructuring the data means pulling out the data portion of the json
            restruct_func = sf_county_jail_restruct,
            # Rename the columns to appropriate database names
            extract_func = sf_county_jail_extract){
            super$initialize(
                url = url, id = id, pull_func = pull_func, type = type,
                restruct_func = restruct_func, extract_func = extract_func,
                log = log, state = state, jurisdiction = jurisdiction)
        }
    )
)

if(sys.nframe() == 0){
    sf_county_jail <- sf_county_jail_scraper$new(log=F)
    sf_county_jail$raw_data
    sf_county_jail$pull_raw()
    sf_county_jail$raw_data
    sf_county_jail$save_raw()
    sf_county_jail$restruct_raw()
    sf_county_jail$restruct_data
    sf_county_jail$extract_from_raw()
    sf_county_jail$extract_data
    sf_county_jail$validate_extract()
    sf_county_jail$save_extract()
}

