source("./R/generic_scraper.R")
source("./R/utilities.R")

historical_tx_vaccine_records_pull <- function(x, date){
    "/tmp/sel_dl/Responsive Information - Vaccination Rates.pdf"
}

historical_tx_vaccine_records_restruct <- function(x){
    bind_rows(
        tabulizer::extract_tables(x, pages = 1) %>% 
            as.data.frame(), 
        tabulizer::extract_tables(x, pages = 2) %>% 
            as.data.frame()
    )
}

historical_tx_vaccine_records_extract <- function(x){
    
    names(x) <- c(
        "Name", 
        "Staff.Population", 
        "Residents.Population", 
        "Staff.Initiated", 
        "Residents.Initiated", 
        "Staff.External.Drop", 
        "Staff.Pct.Drop", 
        "Residents.Pct.Drop", 
        "Total.Pct.Drop", 
        "Total.Needed.Drop"
        )
    
    x %>%
        clean_scraped_df() %>% 
        mutate(Staff.Initiated = Staff.Initiated + Staff.External.Drop) %>% 
        select(-ends_with("Drop"))
}

#' Scraper class for general Texas vaccine data from records request 
#' 
#' @name historical_texas_vaccine_records_scraper
#' @description Facility-level vaccine data provided by the DOC via a records request. 
#' \describe{
#'   \item{Unit}{}
#'   \item{Total Staff Population}{}
#'   \item{Total Inmate Population}{}
#'   \item{Total Staff Vaccinated}{}
#'   \item{Total Inmate Vaccinated}{}
#'   \item{Total External Staff Vaccinated}{}
#'   \item{Total Staff Vaccinated %}{}
#'   \item{Total Inmate Vaccinated %}{}
#'   \item{Total Vaccinated %}{}
#'   \item{Total Vaccinated Needed}{}
#' }

historical_tx_vaccine_records_scraper <- R6Class(
    "historical_tx_vaccine_records_scraper",
    inherit = generic_scraper,
    public = list(
        log = NULL,
        initialize = function(
            log,
            url = "Texas Department of Criminal Justice Records Response",
            id = "historical_texas_vaccine_records",
            type = "pdf",
            state = "TX",
            jurisdiction = "state",
            check_date = NULL,
            # pull the JSON data directly from the API
            pull_func = historical_tx_vaccine_records_pull,
            # restructuring the data means pulling out the data portion of the json
            restruct_func = historical_tx_vaccine_records_restruct,
            # Rename the columns to appropriate database names
            extract_func = historical_tx_vaccine_records_extract){
            super$initialize(
                url = url, id = id, pull_func = pull_func, type = type,
                restruct_func = restruct_func, extract_func = extract_func,
                log = log, state = state, jurisdiction = jurisdiction,
                check_date = NULL)
        }
    )
)

if(sys.nframe() == 0){
    historical_tx_vaccine_records <- historical_tx_vaccine_records_scraper$new(log=FALSE)
    historical_tx_vaccine_records$raw_data
    historical_tx_vaccine_records$reset_date(date = "2021-09-16")
    historical_tx_vaccine_records$pull_raw(date = historical_tx_vaccine_records$date, .dated_pull = TRUE)
    historical_tx_vaccine_records$raw_data
    historical_tx_vaccine_records$save_raw()
    historical_tx_vaccine_records$restruct_raw()
    historical_tx_vaccine_records$restruct_data
    historical_tx_vaccine_records$extract_from_raw()
    historical_tx_vaccine_records$extract_data
    historical_tx_vaccine_records$validate_extract()
    historical_tx_vaccine_records$save_extract()
}

