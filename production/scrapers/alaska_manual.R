source("./R/generic_scraper.R")
source("./R/utilities.R")

alaska_manual_check_date <- function(x, date = Sys.Date()){
    "1VhAAbzipvheVRG0UWKMLT6mCVQRMdV98lUUkk-PCYtQ" %>%
        googlesheets4::read_sheet(sheet = "AK", col_types = "cDccc") %>%
        pull(Date) %>%
        max(na.rm = TRUE) %>%
        error_on_date(date)
}

alaska_manual_pull <- function(x){
    "1VhAAbzipvheVRG0UWKMLT6mCVQRMdV98lUUkk-PCYtQ" %>%
        googlesheets4::read_sheet(sheet = "AK", col_types = "cDccc")
}

alaska_manual_restruct <- function(x){
    x %>%
        filter(!is.na(Date)) %>% 
        filter(Date == max(Date))
}

alaska_manual_extract <- function(x, exp_date = Sys.Date()){
    
    check_names(x, c(
        "State",
        "Date",
        "Name", 
        "Staff.Confirmed", 
        "Staff.Deaths")
    )
    
    x %>%
        select(Name, Staff.Confirmed, Staff.Deaths) %>% 
        {suppressWarnings(mutate_at(., vars(starts_with("Res")), as.numeric))} %>%
        {suppressWarnings(mutate_at(., vars(starts_with("Staff")), as.numeric))} %>%
        clean_scraped_df()
}

#' Scraper class for Alaska COVID data collected directly from the DOC 
#' 
#' @name alaska_manual_scraper
#' @description Data on staff confirmed and staff recovered collected directly from
#' the Alaska DOC. 
#' \describe{
#'   \item{Staff.Confirmed}{}
#'   \item{Staff.Deaths}{}
#' }

alaska_manual_scraper <- R6Class(
    "alaska_manual_scraper",
    inherit = generic_scraper,
    public = list(
        log = NULL,
        initialize = function(
            log,
            url = "Alaska Department of Corrections",
            id = "alaska_manual",
            type = "manual",
            state = "AK",
            jurisdiction = "state",
            check_date = alaska_manual_check_date,
            # pull the JSON data directly from the API
            pull_func = alaska_manual_pull,
            # restructuring the data means pulling out the data portion of the json
            restruct_func = alaska_manual_restruct,
            # Rename the columns to appropriate database names
            extract_func = alaska_manual_extract){
            super$initialize(
                url = url, id = id, pull_func = pull_func, type = type,
                restruct_func = restruct_func, extract_func = extract_func,
                log = log, state = state, jurisdiction = jurisdiction,
                check_date = check_date)
        }
    )
)

if(sys.nframe() == 0){
    alaska_manual <- alaska_manual_scraper$new(log=TRUE)
    alaska_manual$run_check_date()
    alaska_manual$raw_data
    alaska_manual$pull_raw()
    alaska_manual$raw_data
    alaska_manual$save_raw()
    alaska_manual$restruct_raw()
    alaska_manual$restruct_data
    alaska_manual$extract_from_raw()
    alaska_manual$extract_data
    alaska_manual$validate_extract()
    alaska_manual$save_extract()
}
