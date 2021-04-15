source("./R/generic_scraper.R")
source("./R/utilities.R")

sf_county_jail_pull <- function(x){
    z = "1hOBU6510jHZbxN3c3nVwXKGEUfyI9mlkv0vg2LiEJAA" %>%
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
        mutate(Date = lubridate::mdy(`As of Date`)) %>%
        filter(Date == max(Date))
}

sf_county_jail_extract <- function(x, exp_date = Sys.Date()){
    
    error_on_date(x$Date, exp_date)
    
    x %>%
        select(
            Residents.Confirmed = `Confirmed Cases (Incarcerated population, cumulative)`,
            Residents.Active = `Active Cases (Incarcerated population, current)`,
            Residents.Tadmin = `Tests (Incarcerated Population, cumulative )`, 
            Staff.Population = `Total SFSO Employees`, 
            Staff.Confirmed = `SFSO Employees Total Positive Results`, 
            Residents.Initiated = `Partially Vaccinated (Incarcerated population, cumulative)`, 
            Residents.Completed = `Fully Vaccinated (Incarcerated population, cumulative)`, 
            Residents.Deaths = `Deaths (Incarcerated population, cumulative)`
            ) %>% 
        mutate(
            Residents.Initiated = Residents.Initiated + Residents.Completed
        ) %>%
        mutate_all(unlist) %>%
        mutate_all(as.numeric) %>%
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
    sf_county_jail <- sf_county_jail_scraper$new(log=TRUE)
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

