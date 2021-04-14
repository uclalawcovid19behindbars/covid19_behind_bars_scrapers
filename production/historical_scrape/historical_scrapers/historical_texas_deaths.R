source("./R/generic_scraper.R")
source("./R/utilities.R")

historical_texas_deaths_pull <- function(x, date = NULL, file = NULL){
    ## ran this once and then downloaded it to avoid rate limiting
    z <- "1qnea3D6S18H2tKz0Q1STnJ47qkXxNE8u-axPA1p81Zc" %>%
        googlesheets4::read_sheet(
            sheet = "Sheet1",
            )
}

historical_texas_deaths_restruct <- function(z){
    # check against current scrape date
    tx_dat <- read_mpap_data(all_dates = TRUE) %>%
        filter(State == "Texas")
    
    out <- z %>%
        select(Date = `Date (YYYY-MM-DD)`,
               New.Residents.Deaths = Residents.Deaths, 
               Name) %>%
        filter(!is.na(Date)) %>%
        mutate(Date = lubridate::ymd(Date)) %>%
        group_by(Name) %>% 
        arrange(Date) %>%
        mutate(Residents.Deaths = cumsum(New.Residents.Deaths)) %>%
        ungroup() %>%
        select(-New.Residents.Deaths)
    
    cbb <- out %>%
        group_by(month_yr = glue('{month(Date)}_{year(Date)}')) %>% 
        summarise(Residents.Deaths_cbb = sum(Residents.Deaths)) 
    
    mpap <- tx_dat %>%
        group_by(month_yr = glue('{month(Date)}_{year(Date)}')) %>% 
        summarise(Residents.Deaths_mpap = sum(Residents.Deaths)) 
    
    ## some strange things going on here
    compare <- cbb %>%
        left_join(mpap, by = "month_yr")
}

historical_texas_deaths_extract <- function(x){
    x
    
}

#' Scraper class for general historical_texas_deaths COVID data
#' 
#' @name historical_texas_deaths_scraper
#' @description This will be a description of historical_texas_deaths data and what the scraper
#' does
#' \describe{
#'   \item{Facility_Name}{The faciilty name.}
#' }

historical_texas_deaths_scraper <- R6Class(
    "historical_texas_deaths_scraper",
    inherit = generic_scraper,
    public = list(
        log = NULL,
        initialize = function(
            log,
            url = "https://www.davisvanguard.org/tag/covid-19/",
            id = "historical_texas_deaths",
            type = "csv",
            state = "CA",
            jurisdiction = "county",
            # pull the JSON data directly from the API
            pull_func = historical_texas_deaths_pull,
            # restructuring the data means pulling out the data portion of the json
            restruct_func = historical_texas_deaths_restruct,
            # Rename the columns to appropriate database names
            extract_func = historical_texas_deaths_extract){
            super$initialize(
                url = url, id = id, pull_func = pull_func, type = type,
                restruct_func = restruct_func, extract_func = extract_func,
                log = log, state = state, jurisdiction = jurisdiction)
        }
    )
)

if(sys.nframe() == 0){
    historical_texas_deaths <- historical_texas_deaths_scraper$new(log = TRUE)
    historical_texas_deaths$raw_data
    historical_texas_deaths$pull_raw()
    historical_texas_deaths$raw_data
    historical_texas_deaths$save_raw()
    historical_texas_deaths$restruct_raw()
    historical_texas_deaths$restruct_data
    historical_texas_deaths$extract_from_raw()
    historical_texas_deaths$extract_data
    historical_texas_deaths$validate_extract()
    historical_texas_deaths$save_extract()
}

