source("./R/generic_scraper.R")
source("./R/utilities.R")

ms_manual_check_date <- function(x, date = Sys.Date()){
    "1VhAAbzipvheVRG0UWKMLT6mCVQRMdV98lUUkk-PCYtQ" %>%
        googlesheets4::read_sheet(sheet = "MS Deaths", col_types = "cDcc") %>%
        pull(Date) %>%
        max(na.rm = TRUE) %>%
        error_on_date(date)
}

ms_manual_pull <- function(x){
    "1VhAAbzipvheVRG0UWKMLT6mCVQRMdV98lUUkk-PCYtQ" %>%
        googlesheets4::read_sheet(sheet = "MS Deaths", col_types = "cDcc")
}

ms_manual_restruct <- function(x){
    x %>%
        filter(!is.na(Date)) %>% 
        filter(Date == max(Date))
}

ms_manual_extract <- function(x, exp_date = Sys.Date()){
    
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

#' Scraper class for Mississippi COVID data collected directly from the DOC 
#' 
#' @name illinois_manual_scraper
#' @description Data on staff confirmed and staff deaths collected directly from
#' the Mississippi DOC. 
#' \describe{
#'   \item{Residents.Deaths}{}
#' }

mississippi_manual_scraper <- R6Class(
    "mississippi_manual_scraper",
    inherit = generic_scraper,
    public = list(
        log = NULL,
        initialize = function(
            log,
            url = "Mississippi Department of Corrections",
            id = "mississippi_manual",
            type = "manual",
            state = "MS",
            jurisdiction = "state",
            check_date = ms_manual_check_date,
            # pull the JSON data directly from the API
            pull_func = ms_manual_pull,
            # restructuring the data means pulling out the data portion of the json
            restruct_func = ms_manual_restruct,
            # Rename the columns to appropriate database names
            extract_func = ms_manual_extract){
            super$initialize(
                url = url, id = id, pull_func = pull_func, type = type,
                restruct_func = restruct_func, extract_func = extract_func,
                log = log, state = state, jurisdiction = jurisdiction,
                check_date = check_date)
        }
    )
)

if(sys.nframe() == 0){
    mississippi_manual <- mississippi_manual_scraper$new(log=TRUE)
    mississippi_manual$run_check_date()
    mississippi_manual$raw_data
    mississippi_manual$pull_raw()
    mississippi_manual$raw_data
    mississippi_manual$save_raw()
    mississippi_manual$restruct_raw()
    mississippi_manual$restruct_data
    mississippi_manual$extract_from_raw()
    mississippi_manual$extract_data
    mississippi_manual$validate_extract()
    mississippi_manual$save_extract()
}
