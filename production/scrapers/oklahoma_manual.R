source("./R/generic_scraper.R")
source("./R/utilities.R")

oklahoma_manual_pull <- function(x){
    z <- "1VhAAbzipvheVRG0UWKMLT6mCVQRMdV98lUUkk-PCYtQ" %>%
        googlesheets4::read_sheet(sheet = "OK", col_types = "ccccccccc")
    
    z %>%
        mutate(Date = lubridate::as_date(Date))
}

oklahoma_manual_restruct <- function(x){
    x %>%
        filter(!is.na(Date)) %>% 
        filter(Date == max(Date)) 
}

oklahoma_manual_extract <- function(x, exp_date = Sys.Date()){
    
    error_on_date(first(x$Date), exp_date)
    
    check_names(x, c(
        "Date", 
        "Name", 
        "Inmate Deaths by Facility", 
        "Inmate Total Tested", 
        "Inmate Total Positive", 
        "Inmate Total Currently Positive", 
        "Employee Total Tested", 
        "Employee Total Positive", 
        "Employee Total Currently Positive")
    )
    
    x %>%
        select(
            Name = `Name`,
            Residents.Deaths = `Inmate Deaths by Facility`, 
            Residents.Tadmin = `Inmate Total Tested`, 
            Residents.Confirmed = `Inmate Total Positive`, 
            Residents.Active = `Inmate Total Currently Positive`, 
            Staff.Confirmed = `Employee Total Positive`) %>% 
        # To avoid double-counting, statewide deaths should be NA 
        # These are reported at the facility-level instead 
        # This should be reflected in the manual entry, so this is just to be safe  
        mutate(Residents.Deaths = ifelse(Name == "STATEWIDE", NA, Residents.Deaths)) %>% 
        {suppressWarnings(mutate_at(., vars(starts_with("Res")), as.numeric))} %>%
        {suppressWarnings(mutate_at(., vars(starts_with("Staff")), as.numeric))} %>%
        clean_scraped_df()
}

#' Scraper class for Oklahoma COVID data
#' 
#' @name oklahoma_manual_scraper
#' @description Oklahoma's dashboard isn't machine-readable, so we manually extract 
#' the relevant information. 
#' \describe{
#'   \item{Name}{The facility name.}
#'   \item{Inmate Deaths by Facility}{Facility-level cumulative deaths}
#'   \item{Inmate Total Tested}{Statewide-level total tests administered}
#'   \item{Inmate Total Positive}{Statewide-level cumulative COVID cases for inmates}
#'   \item{Inmate Total Currently Positive}{Statewide-level current active COVID infections}
#'   \item{Employee Total Positive}{Statewide-level cumulative COVID cases for staff}

oklahoma_manual_scraper <- R6Class(
    "oklahoma_manual_scraper",
    inherit = generic_scraper,
    public = list(
        log = NULL,
        initialize = function(
            log,
            url = "https://oklahoma.gov/doc/covid-19-stats.html",
            id = "oklahoma_manual",
            type = "manual",
            state = "OK",
            jurisdiction = "state",
            # pull the JSON data directly from the API
            pull_func = oklahoma_manual_pull,
            # restructuring the data means pulling out the data portion of the json
            restruct_func = oklahoma_manual_restruct,
            # Rename the columns to appropriate database names
            extract_func = oklahoma_manual_extract){
            super$initialize(
                url = url, id = id, pull_func = pull_func, type = type,
                restruct_func = restruct_func, extract_func = extract_func,
                log = log, state = state, jurisdiction = jurisdiction)
        }
    )
)

if(sys.nframe() == 0){
    oklahoma_manual <- oklahoma_manual_scraper$new(log=TRUE)
    oklahoma_manual$raw_data
    oklahoma_manual$pull_raw()
    oklahoma_manual$raw_data
    oklahoma_manual$save_raw()
    oklahoma_manual$restruct_raw()
    oklahoma_manual$restruct_data
    oklahoma_manual$extract_from_raw()
    oklahoma_manual$extract_data
    oklahoma_manual$validate_extract()
    oklahoma_manual$save_extract()
}
