source("./R/generic_scraper.R")
source("./R/utilities.R")

santa_rita_jail_pull <- function(x){
    "1Uhc5P7yPG7d50rbgskqJbabRGxzmmY0NCovSamzV2Uc" %>%
        googlesheets4::read_sheet()
}

santa_rita_jail_restruct <- function(x){
    x %>%
        mutate(Date = lubridate::mdy(`As of Date`)) %>%
        filter(Date == max(Date))
}

santa_rita_jail_extract <- function(x, exp_date = Sys.Date()){
    
    error_on_date(x$Date, exp_date)
    
    x %>%
        select(
            Residents.Confirmed = `Confirmed Cases`,
            Residents.Active = `Active in Custody`,
            Residents.Recovered = `Resolved in Custody`,
            Residents.Deaths = Deaths,
            Residents.Tadmin = `Cumulative Tested`,
            Residents.Pending = `Tests Pending`
            ) %>%
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
            url = "https://www.alamedacountysheriff.org/dc_srj_visiting.php",
            id = "santa_rita_jail",
            type = "csv",
            state = "CA",
            jurisdiction = "county",
            # pull the JSON data directly from the API
            pull_func = santa_rita_jail_pull,
            # restructuring the data means pulling out the data portion of the json
            restruct_func = santa_rita_jail_restruct,
            # Rename the columns to appropriate database names
            extract_func = santa_rita_jail_extract){
            super$initialize(
                url = url, id = id, pull_func = pull_func, type = type,
                restruct_func = restruct_func, extract_func = extract_func,
                log = log, state = state, jurisdiction = jurisdiction)
        }
    )
)

if(sys.nframe() == 0){
    santa_rita_jail <- santa_rita_jail_scraper$new(log=TRUE)
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

