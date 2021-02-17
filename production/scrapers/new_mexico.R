source("./R/generic_scraper.R")
source("./R/utilities.R")

new_mexico_pull <- function(x){
    "1VhAAbzipvheVRG0UWKMLT6mCVQRMdV98lUUkk-PCYtQ" %>%
        googlesheets4::read_sheet(sheet = "NM", 
                                  col_types = "Dcccccc")
}

new_mexico_restruct <- function(x){
    x %>%
        filter(!is.na(Date)) %>% 
        filter(Date == max(Date))
}

new_mexico_extract <- function(x, exp_date = Sys.Date()){
    
    error_on_date(first(x$Date), exp_date)
    
    check_names(x, c(
        "Date", 
        "Name", 
        "Active Cases", 
        "Recoveries", 
        "Total Positives to Date", 
        "Deaths", 
        "Total Tests Conducted")
    )
    
    x %>%
        select(
            Name = `Name`,
            Residents.Active = `Active Cases`, 
            Residents.Recovered = `Recoveries`, 
            Residents.Confirmed = `Total Positives to Date`, 
            Residents.Deaths = `Deaths`, 
            Residents.Tadmin = `Total Tests Conducted`) %>% 
        clean_scraped_df()
}

#' Scraper class for general new_mexico COVID data
#' 
#' @name new_mexico_scraper
#' @description New Mexico's dashboard isn't machine-readable, so we manually
#' extract the relevant information. 
#' does
#' \describe{
#'   \item{Name}{The facility name.}
#'   \item{Active cases}{Current active cases among incarcerated people by facility.}
#'   \item{Recoveries}{Total recovered cases among incarcerated people by facility.}
#'   \item{Total Positives to Date}{Cumulative cases among incarcerated people by facility.}
#'   \item{Deaths}{Cumulative deaths among incarcerated people by facility.}
#'   \item{Total Tests Conducted}{Statewide total number of tests conducted.}
#' }

new_mexico_scraper <- R6Class(
    "new_mexico_scraper",
    inherit = generic_scraper,
    public = list(
        log = NULL,
        initialize = function(
            log,
            url = "https://cd.nm.gov/covid-19-updates/",
            id = "new_mexico",
            type = "manual",
            state = "NM",
            jurisdiction = "state",
            # pull the JSON data directly from the API
            pull_func = new_mexico_pull,
            # restructuring the data means pulling out the data portion of the json
            restruct_func = new_mexico_restruct,
            # Rename the columns to appropriate database names
            extract_func = new_mexico_extract){
            super$initialize(
                url = url, id = id, pull_func = pull_func, type = type,
                restruct_func = restruct_func, extract_func = extract_func,
                log = log, state = state, jurisdiction = jurisdiction)
        }
    )
)

if(sys.nframe() == 0){
    new_mexico <- new_mexico_scraper$new(log=TRUE)
    new_mexico$raw_data
    new_mexico$pull_raw()
    new_mexico$raw_data
    new_mexico$save_raw()
    new_mexico$restruct_raw()
    new_mexico$restruct_data
    new_mexico$extract_from_raw()
    new_mexico$extract_data
    new_mexico$validate_extract()
    new_mexico$save_extract()
}
