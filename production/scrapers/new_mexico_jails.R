source("./R/generic_scraper.R")
source("./R/utilities.R")

new_mexico_jails_pull <- function(x){
    "1sdA9huEQNSWYHN5Ygin9xB2Xcv-sHPhdC0G-xT8Z_gA" %>%
        googlesheets4::read_sheet()
}

new_mexico_jails_restruct <- function(x){
    x %>%
        mutate(Date = lubridate::mdy(`Date`)) %>%
        filter(!is.na(Date)) %>% 
        filter(Date == max(Date))
}

new_mexico_jails_extract <- function(x, exp_date = Sys.Date()){
    
    error_on_date(first(x$Date), exp_date, days = 10)
    
    check_names(x, c(
        "Date", 
        "Detention Center", 
        "Total", 
        "Detainee", 
        "Staff"
        ))
    
    x %>%
        select(
            Name = `Detention Center`,
            Residents.Confirmed = Detainee,
            Staff.Confirmed = Staff,
        ) %>%
        filter(!str_detect(Name, "(?i)all")) # remove sum total
}

#' Scraper class for general new_mexico_jails COVID data
#' 
#' @name new_mexico_jails_scraper
#' @description Grabs google sheet data and reads it in for NM jails
#' 
#' \describe{
#'   \item{Facility_Name}{The faciilty name.}
#' }

new_mexico_jails_scraper <- R6Class(
    "new_mexico_jails_scraper",
    inherit = generic_scraper,
    public = list(
        log = NULL,
        initialize = function(
            log,
            url = "NM Dept. of Health",
            id = "new_mexico_jails",
            type = "csv",
            state = "NM",
            jurisdiction = "county",
            # pull the JSON data directly from the API
            pull_func = new_mexico_jails_pull,
            # restructuring the data means pulling out the data portion of the json
            restruct_func = new_mexico_jails_restruct,
            # Rename the columns to appropriate database names
            extract_func = new_mexico_jails_extract){
            super$initialize(
                url = url, id = id, pull_func = pull_func, type = type,
                restruct_func = restruct_func, extract_func = extract_func,
                log = log, state = state, jurisdiction = jurisdiction)
        }
    )
)

if(sys.nframe() == 0){
    new_mexico_jails <- new_mexico_jails_scraper$new(log=TRUE)
    new_mexico_jails$raw_data
    new_mexico_jails$pull_raw()
    new_mexico_jails$raw_data
    new_mexico_jails$save_raw()
    new_mexico_jails$restruct_raw()
    new_mexico_jails$restruct_data
    new_mexico_jails$extract_from_raw()
    new_mexico_jails$extract_data
    new_mexico_jails$validate_extract()
    new_mexico_jails$save_extract()
}

