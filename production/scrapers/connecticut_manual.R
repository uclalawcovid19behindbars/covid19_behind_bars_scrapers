source("./R/generic_scraper.R")
source("./R/utilities.R")

ct_manual_check_date <- function(x, date = Sys.Date()){
    "1VhAAbzipvheVRG0UWKMLT6mCVQRMdV98lUUkk-PCYtQ" %>%
        googlesheets4::read_sheet(sheet = "CT", col_types = "cDccc") %>%
        pull(Date) %>%
        max(na.rm = TRUE) %>%
        error_on_date(date)
}

ct_manual_pull <- function(x){
    "1VhAAbzipvheVRG0UWKMLT6mCVQRMdV98lUUkk-PCYtQ" %>%
        googlesheets4::read_sheet(sheet = "CT", col_types = "cDccc")
}

ct_manual_restruct <- function(x){
    x %>%
        filter(!is.na(Date)) %>% 
        filter(Date == max(Date))
}

ct_manual_extract <- function(x, exp_date = Sys.Date()){
    
    check_names(x, c(
        "State",
        "Date",
        "Name", 
        "Staff.Confirmed", 
        "Staff.Recovered")
    )
    
    x %>%
        select(Name, Staff.Confirmed, Staff.Recovered) %>% 
        {suppressWarnings(mutate_at(., vars(starts_with("Res")), as.numeric))} %>%
        {suppressWarnings(mutate_at(., vars(starts_with("Staff")), as.numeric))} %>%
        clean_scraped_df()
}

#' Scraper class for Connecticut COVID data collected directly from the DOC 
#' 
#' @name connecticut_manual_scraper
#' @description Data on staff confirmed and staff recovered collected directly from
#' the Connecticut DOC. 
#' \describe{
#'   \item{Staff.Confirmed}{}
#'   \item{Staff.Recovered}{}
#' }

connecticut_manual_scraper <- R6Class(
    "connecticut_manual_scraper",
    inherit = generic_scraper,
    public = list(
        log = NULL,
        initialize = function(
            log,
            url = "Connecticut Department of Corrections",
            id = "connecticut_manual",
            type = "manual",
            state = "CT",
            jurisdiction = "state",
            check_date = ct_manual_check_date,
            # pull the JSON data directly from the API
            pull_func = ct_manual_pull,
            # restructuring the data means pulling out the data portion of the json
            restruct_func = ct_manual_restruct,
            # Rename the columns to appropriate database names
            extract_func = ct_manual_extract){
            super$initialize(
                url = url, id = id, pull_func = pull_func, type = type,
                restruct_func = restruct_func, extract_func = extract_func,
                log = log, state = state, jurisdiction = jurisdiction,
                check_date = check_date)
        }
    )
)

if(sys.nframe() == 0){
    ct_manual <- connecticut_manual_scraper$new(log=TRUE)
    ct_manual$run_check_date()
    ct_manual$raw_data
    ct_manual$pull_raw()
    ct_manual$raw_data
    ct_manual$save_raw()
    ct_manual$restruct_raw()
    ct_manual$restruct_data
    ct_manual$extract_from_raw()
    ct_manual$extract_data
    ct_manual$validate_extract()
    ct_manual$save_extract()
}
