source("./R/generic_scraper.R")
source("./R/utilities.R")

north_carolina_manual_check_date <- function(x, date = Sys.Date()){
    "1VhAAbzipvheVRG0UWKMLT6mCVQRMdV98lUUkk-PCYtQ" %>%
        googlesheets4::read_sheet(sheet = "NC", col_types = "cDcc") %>%
        pull(Date) %>%
        max(na.rm = TRUE) %>%
        error_on_date(date)
}

north_carolina_manual_pull <- function(x){
    "1VhAAbzipvheVRG0UWKMLT6mCVQRMdV98lUUkk-PCYtQ" %>%
        googlesheets4::read_sheet(sheet = "NC", col_types = "cDcc")
}

north_carolina_manual_restruct <- function(x){
    x %>%
        filter(!is.na(Date)) %>% 
        filter(Date == max(Date))
}

north_carolina_manual_extract <- function(x, exp_date = Sys.Date()){
    
    check_names(x, c(
        "State",
        "Date",
        "Name", 
        "Staff.Confirmed")
    )
    
    x %>%
        select(Name, Staff.Confirmed) %>% 
        {suppressWarnings(mutate_at(., vars(starts_with("Res")), as.numeric))} %>%
        {suppressWarnings(mutate_at(., vars(starts_with("Staff")), as.numeric))} %>%
        clean_scraped_df()
}

#' Scraper class for North carolina COVID data collected directly from the DOC 
#' 
#' @name north_carolina_manual_scraper
#' @description Data on staff confirmed and staff deaths collected directly from
#' the North Carolina DOC. 
#' \describe{
#'   \item{Staff.Confirmed}{}
#' }

north_carolina_manual_scraper <- R6Class(
    "north_carolina_manual_scraper",
    inherit = generic_scraper,
    public = list(
        log = NULL,
        initialize = function(
            log,
            url = "North Carolina Department of Corrections",
            id = "north_carolina_manual",
            type = "manual",
            state = "NC",
            jurisdiction = "state",
            check_date = north_carolina_manual_check_date,
            # pull the JSON data directly from the API
            pull_func = north_carolina_manual_pull,
            # restructuring the data means pulling out the data portion of the json
            restruct_func = north_carolina_manual_restruct,
            # Rename the columns to appropriate database names
            extract_func = north_carolina_manual_extract){
            super$initialize(
                url = url, id = id, pull_func = pull_func, type = type,
                restruct_func = restruct_func, extract_func = extract_func,
                log = log, state = state, jurisdiction = jurisdiction,
                check_date = check_date)
        }
    )
)

if(sys.nframe() == 0){
    nc_manual <- north_carolina_manual_scraper$new(log=TRUE)
    nc_manual$run_check_date()
    nc_manual$raw_data
    nc_manual$pull_raw()
    nc_manual$raw_data
    nc_manual$save_raw()
    nc_manual$restruct_raw()
    nc_manual$restruct_data
    nc_manual$extract_from_raw()
    nc_manual$extract_data
    nc_manual$validate_extract()
    nc_manual$save_extract()
}
