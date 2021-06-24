source("./R/generic_scraper.R")
source("./R/utilities.R")

kansas_statewide_pull <- function(x){
    "1VhAAbzipvheVRG0UWKMLT6mCVQRMdV98lUUkk-PCYtQ" %>%
        googlesheets4::read_sheet(sheet = "KS Deaths", 
                                  col_types = "Dccc")
}

kansas_statewide_restruct <- function(x){
    x %>%
        filter(!is.na(Date)) %>% 
        filter(Date == max(Date))
}

kansas_statewide_extract <- function(x, exp_date = Sys.Date()){
    
    error_on_date(first(x$Date), exp_date)
    
    check_names(x, c(
        "Date", 
        "Name", 
        "Staff Deaths", 
        "Residents Deaths")
    )
    
    x %>%
        select(
            Name = `Name`,
            Residents.Deaths = `Residents Deaths`,
            Staff.Deaths = `Staff Deaths`) %>% 
        clean_scraped_df()
}

#' Scraper class for general kansas_statewide COVID data
#' 
#' @name kansas_statewide_scraper
#' @description Kansas reports deaths in a footnote that we record manually. 
#' \describe{
#'   \item{Staff Deaths}{Number of staff deaths attributed to COVID.}
#'   \item{Resident Deaths}{Number of incarcerated individual deaths attributed to COVID.}
#' }

kansas_statewide_scraper <- R6Class(
    "kansas_statewide_scraper",
    inherit = generic_scraper,
    public = list(
        log = NULL,
        initialize = function(
            log,
            url = "https://www.doc.ks.gov/kdoc-coronavirus-updates/kdoc-covid-19-status",
            id = "kansas_statewide",
            type = "manual",
            state = "KS",
            jurisdiction = "state",
            check_date = NULL,
            # pull the JSON data directly from the API
            pull_func = kansas_statewide_pull,
            # restructuring the data means pulling out the data portion of the json
            restruct_func = kansas_statewide_restruct,
            # Rename the columns to appropriate database names
            extract_func = kansas_statewide_extract){
            super$initialize(
                url = url, id = id, pull_func = pull_func, type = type,
                restruct_func = restruct_func, extract_func = extract_func,
                log = log, state = state, jurisdiction = jurisdiction,
                check_date = check_date)
        }
    )
)

if(sys.nframe() == 0){
    kansas_statewide <- kansas_statewide_scraper$new(log=TRUE)
    kansas_statewide$run_check_date()
    kansas_statewide$raw_data
    kansas_statewide$pull_raw()
    kansas_statewide$raw_data
    kansas_statewide$save_raw()
    kansas_statewide$restruct_raw()
    kansas_statewide$restruct_data
    kansas_statewide$extract_from_raw()
    kansas_statewide$extract_data
    kansas_statewide$validate_extract()
    kansas_statewide$save_extract()
}
