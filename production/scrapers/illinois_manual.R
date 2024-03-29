source("./R/generic_scraper.R")
source("./R/utilities.R")

illinois_manual_check_date <- function(x, date = Sys.Date()){
    "1VhAAbzipvheVRG0UWKMLT6mCVQRMdV98lUUkk-PCYtQ" %>%
        googlesheets4::read_sheet(sheet = "IL", col_types = "cDcc") %>%
        pull(Date) %>%
        max(na.rm = TRUE) %>%
        error_on_date(date)
}

illinois_manual_pull <- function(x){
    "1VhAAbzipvheVRG0UWKMLT6mCVQRMdV98lUUkk-PCYtQ" %>%
        googlesheets4::read_sheet(sheet = "IL", col_types = "cDcc")
}

illinois_manual_restruct <- function(x){
    x %>%
        filter(!is.na(Date)) %>% 
        filter(Date == max(Date))
}

illinois_manual_extract <- function(x, exp_date = Sys.Date()){
    
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

#' Scraper class for Illinois COVID data collected directly from the DOC 
#' 
#' @name illinois_manual_scraper
#' @description Data on staff confirmed and staff deaths collected directly from
#' the Illinois DOC. 
#' \describe{
#'   \item{Staff.Confirmed}{}
#'   \item{Staff.Deaths}{}
#' }

illinois_manual_scraper <- R6Class(
    "illinois_manual_scraper",
    inherit = generic_scraper,
    public = list(
        log = NULL,
        initialize = function(
            log,
            url = "Illinois Department of Corrections",
            id = "illinois_manual",
            type = "manual",
            state = "IL",
            jurisdiction = "state",
            check_date = illinois_manual_check_date,
            # pull the JSON data directly from the API
            pull_func = illinois_manual_pull,
            # restructuring the data means pulling out the data portion of the json
            restruct_func = illinois_manual_restruct,
            # Rename the columns to appropriate database names
            extract_func = illinois_manual_extract){
            super$initialize(
                url = url, id = id, pull_func = pull_func, type = type,
                restruct_func = restruct_func, extract_func = extract_func,
                log = log, state = state, jurisdiction = jurisdiction,
                check_date = check_date)
        }
    )
)

if(sys.nframe() == 0){
    illinois_manual <- illinois_manual_scraper$new(log=TRUE)
    illinois_manual$run_check_date()
    illinois_manual$raw_data
    illinois_manual$pull_raw()
    illinois_manual$raw_data
    illinois_manual$save_raw()
    illinois_manual$restruct_raw()
    illinois_manual$restruct_data
    illinois_manual$extract_from_raw()
    illinois_manual$extract_data
    illinois_manual$validate_extract()
    illinois_manual$save_extract()
}
