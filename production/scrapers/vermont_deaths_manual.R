source("./R/generic_scraper.R")
source("./R/utilities.R")

vermont_deaths_manual_check_date <- function(x, date = Sys.Date()){
    "1VhAAbzipvheVRG0UWKMLT6mCVQRMdV98lUUkk-PCYtQ" %>%
        googlesheets4::read_sheet(sheet = "VT Deaths", col_types = "cDcc") %>%
        pull(Date) %>%
        max(na.rm = TRUE) %>%
        error_on_date(date)
}

vermont_deaths_manual_pull <- function(x){
    "1VhAAbzipvheVRG0UWKMLT6mCVQRMdV98lUUkk-PCYtQ" %>%
        googlesheets4::read_sheet(sheet = "VT Deaths", col_types = "cDcc")
}

vermont_deaths_manual_restruct <- function(x){
    x %>%
        filter(!is.na(Date)) %>% 
        filter(Date == max(Date))
}

vermont_deaths_manual_extract <- function(x, exp_date = Sys.Date()){
    
    check_names(x, c(
        "State",
        "Date",
        "Name", 
        "Residents.Deaths")
    )
    
    x %>%
        select(Name, Residents.Deaths) %>% 
        {suppressWarnings(mutate_at(., vars(starts_with("Res")), as.numeric))} %>%
        {suppressWarnings(mutate_at(., vars(starts_with("Staff")), as.numeric))} %>%
        clean_scraped_df()
}

#' Scraper class for Vermont COVID data collected directly from the DOC 
#' 
#' @name vermont_deaths_manual_scraper
#' @description Data on staff confirmed and staff recovered collected directly from
#' the Vermont DOC. 
#' \describe{
#'   \item{Residents.Deaths}{}
#' }

vermont_deaths_manual_scraper <- R6Class(
    "vermont_deaths_manual_scraper",
    inherit = generic_scraper,
    public = list(
        log = NULL,
        initialize = function(
            log,
            url = "Vermont Department of Corrections",
            id = "vermont_deaths_manual",
            type = "manual",
            state = "VT",
            jurisdiction = "state",
            check_date = vermont_deaths_manual_check_date,
            # pull the JSON data directly from the API
            pull_func = vermont_deaths_manual_pull,
            # restructuring the data means pulling out the data portion of the json
            restruct_func = vermont_deaths_manual_restruct,
            # Rename the columns to appropriate database names
            extract_func = vermont_deaths_manual_extract){
            super$initialize(
                url = url, id = id, pull_func = pull_func, type = type,
                restruct_func = restruct_func, extract_func = extract_func,
                log = log, state = state, jurisdiction = jurisdiction,
                check_date = check_date)
        }
    )
)

if(sys.nframe() == 0){
    vermont_deaths_manual <- vermont_deaths_manual_scraper$new(log=TRUE)
    vermont_deaths_manual$run_check_date()
    vermont_deaths_manual$raw_data
    vermont_deaths_manual$pull_raw()
    vermont_deaths_manual$raw_data
    vermont_deaths_manual$save_raw()
    vermont_deaths_manual$restruct_raw()
    vermont_deaths_manual$restruct_data
    vermont_deaths_manual$extract_from_raw()
    vermont_deaths_manual$extract_data
    vermont_deaths_manual$validate_extract()
    vermont_deaths_manual$save_extract()
}
