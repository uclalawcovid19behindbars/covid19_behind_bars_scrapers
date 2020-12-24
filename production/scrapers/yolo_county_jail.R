source("./R/generic_scraper.R")
source("./R/utilities.R")

yolo_county_jail_pull <- function(x){
    "1FwW5oaUWmzmvzw4BXvrUYoCf4LBNCIGtUo5RbR_ZldA" %>%
        googlesheets4::read_sheet()
}

yolo_county_jail_restruct <- function(x){
    x %>%
        mutate(Date = lubridate::mdy(`As of Date`)) %>%
        filter(Date == max(Date))
}

yolo_county_jail_extract <- function(x, exp_date = Sys.Date()){
    
    error_on_date(x$Date, exp_date)
    
    x %>%
        select(
            Residents.Confirmed = `Confirmed Cases`,
            Residents.Active = `Active in Custody`,
            Residents.Recovered = `Resolved`,
            Residents.Deaths = Deaths,
            Residents.Tadmin = `Cumulative Tested`,
            Residents.Population = `Population`,
            Residents.Quarantine = `Medical Isolation`
            ) %>%
        mutate(Name = "YOLO COUNTY JAIL")
}

#' Scraper class for general yolo_county_jail COVID data
#' 
#' @name yolo_county_jail_scraper
#' @description This will be a description of yolo_county_jail data and what the scraper
#' does
#' \describe{
#'   \item{Facility_Name}{The faciilty name.}
#' }

yolo_county_jail_scraper <- R6Class(
    "yolo_county_jail_scraper",
    inherit = generic_scraper,
    public = list(
        log = NULL,
        initialize = function(
            log,
            url = "https://www.yolocountysheriff.com/services/jail/covid-19-mitigation/",
            id = "yolo_county_jail",
            type = "csv",
            state = "CA",
            jurisdiction = "county",
            # pull the JSON data directly from the API
            pull_func = yolo_county_jail_pull,
            # restructuring the data means pulling out the data portion of the json
            restruct_func = yolo_county_jail_restruct,
            # Rename the columns to appropriate database names
            extract_func = yolo_county_jail_extract){
            super$initialize(
                url = url, id = id, pull_func = pull_func, type = type,
                restruct_func = restruct_func, extract_func = extract_func,
                log = log, state = state, jurisdiction = jurisdiction)
        }
    )
)

if(sys.nframe() == 0){
    yolo_county_jail <- yolo_county_jail_scraper$new(log=TRUE)
    yolo_county_jail$raw_data
    yolo_county_jail$pull_raw()
    yolo_county_jail$raw_data
    yolo_county_jail$save_raw()
    yolo_county_jail$restruct_raw()
    yolo_county_jail$restruct_data
    yolo_county_jail$extract_from_raw()
    yolo_county_jail$extract_data
    yolo_county_jail$validate_extract()
    yolo_county_jail$save_extract()
}

