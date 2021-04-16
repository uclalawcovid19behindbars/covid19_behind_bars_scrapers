source("./R/generic_scraper.R")
source("./R/utilities.R")

historical_texas_deaths_pull <- function(x, date = NULL, file = NULL){
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
    
    ## compare our data to marshall project, our historical data 
    # cbb <- out %>%
    #     group_by(month_yr = glue('{month(Date)}_{year(Date)}'), Name) %>% 
    #     summarise(Residents.Deaths_max_fac_month = max(Residents.Deaths)) %>% # get max cumulative count from each facility in each month 
    #     group_by(month_yr) %>% 
    #     ## need to arrange by correct date order and take the cumulative sum 
    #     summarize(Residents.Deaths_cbb_manual = sum(Residents.Deaths_max_fac_month),
    #               n = n()) 
    
    # mpap <- tx_dat %>%
    #     group_by(month_yr = glue('{month(Date)}_{year(Date)}')) %>% 
    #     summarise(Residents.Deaths_mpap = max(Residents.Deaths)) 
    
    # historical_deaths <- read_csv("~/Desktop/historical_tx_deaths.csv")
    
    ## some strange things going on here
    # compare <- cbb %>%
    #     left_join(mpap, by = "month_yr") %>%
    #     # left_join(historical_deaths, by = "month_yr") %>%
    #     relocate(month_yr, "Residents.Deaths_mpap", starts_with("Residents.Deaths_cbb"))
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
            url = "https://www.tdcj.texas.gov/covid-19/presumed.html",
            id = "historical_texas_deaths",
            type = "csv",
            state = "TX",
            jurisdiction = "state",
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

## DO NOT RUN 
## NB: CANNOT INTEGRATE THIS DATA WITH OUR HISTORICAL DATA AT THIS POINT IN TIME 
# if(sys.nframe() == 0){
#     historical_texas_deaths <- historical_texas_deaths_scraper$new(log = TRUE)
#     historical_texas_deaths$raw_data
#     historical_texas_deaths$pull_raw()
#     historical_texas_deaths$raw_data
#     historical_texas_deaths$save_raw()
#     historical_texas_deaths$restruct_raw()
#     historical_texas_deaths$restruct_data
#     historical_texas_deaths$extract_from_raw()
#     historical_texas_deaths$extract_data
#     historical_texas_deaths$validate_extract()
#     historical_texas_deaths$save_extract()
# }

