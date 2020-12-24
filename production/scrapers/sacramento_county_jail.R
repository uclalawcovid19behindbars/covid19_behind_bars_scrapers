source("./R/generic_scraper.R")
source("./R/utilities.R")

sacramento_county_jail_pull <- function(x){
    "1a4iXz_MHMvzanwnCbcE1m5xLwQ4_Ut0sAlm4gE4EDKA" %>%
        googlesheets4::read_sheet()
}

sacramento_county_jail_restruct <- function(x){
    x %>%
        mutate(Date = lubridate::round_date(`As of Date`, unit = "day")) %>%
        mutate(Date = as.Date(Date)) %>%
        filter(Date == max(Date))
}

sacramento_county_jail_extract <- function(x, exp_date = Sys.Date()){
    
    error_on_date(x$Date, exp_date)
    
    x %>%
        select(
            Residents.Confirmed = `Confirmed Cases`,
            Residents.Active = `Active in Custody`,
            Residents.Recovered = `Resolved`,
            Residents.Deaths = Deaths,
            Residents.Tadmin = `Cumulative Tested`,
            Residents.Population = `Total Population`
            ) %>%
        mutate(Name = "SACRAMENTO COUNTY JAIL")
}

#' Scraper class for general sacramento_county_jail COVID data
#' 
#' @name sacramento_county_jail_scraper
#' @description This will be a description of sacramento_county_jail data and what the scraper
#' does
#' \describe{
#'   \item{Facility_Name}{The faciilty name.}
#' }

sacramento_county_jail_scraper <- R6Class(
    "sacramento_county_jail_scraper",
    inherit = generic_scraper,
    public = list(
        log = NULL,
        initialize = function(
            log,
            url = "https://www.sacsheriff.com/pages/covid19.php",
            id = "sacramento_county_jail",
            type = "csv",
            state = "CA",
            jurisdiction = "county",
            # pull the JSON data directly from the API
            pull_func = sacramento_county_jail_pull,
            # restructuring the data means pulling out the data portion of the json
            restruct_func = sacramento_county_jail_restruct,
            # Rename the columns to appropriate database names
            extract_func = sacramento_county_jail_extract){
            super$initialize(
                url = url, id = id, pull_func = pull_func, type = type,
                restruct_func = restruct_func, extract_func = extract_func,
                log = log, state = state, jurisdiction = jurisdiction)
        }
    )
)

if(sys.nframe() == 0){
    sacramento_county_jail <- sacramento_county_jail_scraper$new(log=TRUE)
    sacramento_county_jail$raw_data
    sacramento_county_jail$pull_raw()
    sacramento_county_jail$raw_data
    sacramento_county_jail$save_raw()
    sacramento_county_jail$restruct_raw()
    sacramento_county_jail$restruct_data
    sacramento_county_jail$extract_from_raw()
    sacramento_county_jail$extract_data
    sacramento_county_jail$validate_extract()
    sacramento_county_jail$save_extract()
}

