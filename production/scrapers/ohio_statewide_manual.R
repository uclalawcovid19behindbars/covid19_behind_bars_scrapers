source("./R/generic_scraper.R")
source("./R/utilities.R")

ohio_statewide_manual_pull <- function(x){
    z <- "1VhAAbzipvheVRG0UWKMLT6mCVQRMdV98lUUkk-PCYtQ" %>%
        googlesheets4::read_sheet(sheet = "OH Statewide", col_types = "DDciiii")
    
    z %>%
        mutate(Date = lubridate::ymd(Date))
}

ohio_statewide_manual_restruct <- function(x){
    x %>%
        filter(!is.na(Date)) %>% 
        filter(Date == max(Date)) 
}

ohio_statewide_manual_extract <- function(x, exp_date = Sys.Date()){
    
    site_updated_date <- lubridate::ymd(x$`Site updated date`)
    
    error_on_date(max(site_updated_date), exp_date)
    
    check_names(x, c(
        "Date", 
        "Site updated date",
        "Name", 
        "Month-to-date Tested", 
        "Month-to-date Positive", 
        "Barchart positive sum (pink bars)", 
        "Barchart tests sum (purple bars)")
    )
    
    out <- x %>%
        mutate(
            Name = `Name`,
            Residents.Confirmed = `Month-to-date Positive` + `Barchart positive sum (pink bars)`, 
            Residents.Tadmin = `Month-to-date Tested` + `Barchart tests sum (purple bars)`, 
            Residents.Active = `Month-to-date Positive`) %>% 
        select(Name, 
               Residents.Confirmed,
               Residents.Tadmin,
               Residents.Active) %>%
        clean_scraped_df()
    
    return(out)
}

#' Scraper class for Ohio statewide COVID data 
#' 
#' @name ohio_statewide_manual_scraper
#' @description Ohio's statewide COVID dashboard isn't machine-readable, so we manually extract 
#' the relevant information. 
#' \describe{
#'   \item{Name}{The facility name.}
#'   \item{Inmate Total Tested}{Statewide-level total tests administered}
#'   \item{Inmate Total Positive}{Statewide-level cumulative COVID cases for inmates}
#'   \item{Inmate Total Currently Positive}{Statewide-level current active COVID infections}

ohio_statewide_manual_scraper <- R6Class(
    "ohio_statewide_manual_scraper",
    inherit = generic_scraper,
    public = list(
        log = NULL,
        initialize = function(
            log,
            url = "https://coronavirus.ohio.gov/static/reports/DRCCOVID-19Information.pdf",
            id = "ohio_statewide_manual",
            type = "manual",
            state = "OH",
            jurisdiction = "state",
            check_date = NULL,
            # pull the JSON data directly from the API
            pull_func = ohio_statewide_manual_pull,
            # restructuring the data means pulling out the data portion of the json
            restruct_func = ohio_statewide_manual_restruct,
            # Rename the columns to appropriate database names
            extract_func = ohio_statewide_manual_extract){
            super$initialize(
                url = url, id = id, pull_func = pull_func, type = type,
                restruct_func = restruct_func, extract_func = extract_func,
                log = log, state = state, jurisdiction = jurisdiction,
                check_date = check_date)
        }
    )
)

if(sys.nframe() == 0){
    ohio_statewide_manual <- ohio_statewide_manual_scraper$new(log=TRUE)
    ohio_statewide_manual$run_check_date()
    ohio_statewide_manual$raw_data
    ohio_statewide_manual$pull_raw()
    ohio_statewide_manual$raw_data
    ohio_statewide_manual$save_raw()
    ohio_statewide_manual$restruct_raw()
    ohio_statewide_manual$restruct_data
    ohio_statewide_manual$extract_from_raw()
    ohio_statewide_manual$extract_data
    ohio_statewide_manual$validate_extract()
    ohio_statewide_manual$save_extract()
}
