source("./R/generic_scraper.R")
source("./R/utilities.R")

arizona_vaccine_manual_pull <- function(x){
    "1VhAAbzipvheVRG0UWKMLT6mCVQRMdV98lUUkk-PCYtQ" %>%
        googlesheets4::read_sheet(sheet = "AZ Vaccine", 
                                  col_types = "Dccc")
}

arizona_vaccine_manual_restruct <- function(x){
    x %>%
        filter(!is.na(Date)) %>% 
        filter(Date == max(Date))
}

arizona_vaccine_manual_extract <- function(x, exp_date = Sys.Date()){
    
    error_on_date(first(x$Date), exp_date)
    
    check_names(x, c(
        "Date",
        "Name", 
        "Res.Anchor.Pop", 
        "Res.Pct.Completed")
    )
    
    if (as.numeric(x$Res.Pct.Completed) > 1.0){
        stop(str_c("Vaccination percentage ", x$Res.Pct.Completed, 
                   " should be between 0 and 1."))
    }
    
    x %>%
        {suppressWarnings(mutate_at(., vars(starts_with("Res")), as.numeric))} %>%
        mutate(Residents.Completed = round(Res.Anchor.Pop * Res.Pct.Completed)) %>% 
        select(
            Name = `Name`,
            Residents.Completed 
        ) %>% 
        clean_scraped_df()
}

#' Scraper class for Arizona vaccine data
#' 
#' @name arizona_vaccine_manual_scraper
#' @description Arizona posts weekly updates (on Fridays) with statewide vaccine 
#' updates. They report the percentage of people who are fully vaccinated, from 
#' which we back out the number of people (based on the population).  
#' \describe{
#'   \item{Name}{The facility name.}
#'   \item{Res.Anchor.Pop}{Population denominator used for Arizona.}
#'   \item{Res.Pct.Completed{Percentage of people fully vaccinated.}
#' }

arizona_vaccine_manual_scraper <- R6Class(
    "arizona_vaccine_manual_scraper",
    inherit = generic_scraper,
    public = list(
        log = NULL,
        initialize = function(
            log,
            url = "https://corrections.az.gov/covid-19-management-updates",
            id = "arizona_vaccine_manual",
            type = "manual",
            state = "AZ",
            jurisdiction = "state",
            # pull the JSON data directly from the API
            pull_func = arizona_vaccine_manual_pull,
            # restructuring the data means pulling out the data portion of the json
            restruct_func = arizona_vaccine_manual_restruct,
            # Rename the columns to appropriate database names
            extract_func = arizona_vaccine_manual_extract){
            super$initialize(
                url = url, id = id, pull_func = pull_func, type = type,
                restruct_func = restruct_func, extract_func = extract_func,
                log = log, state = state, jurisdiction = jurisdiction)
        }
    )
)

if(sys.nframe() == 0){
    arizona_vaccine_manual <- arizona_vaccine_manual_scraper$new(log=TRUE)
    arizona_vaccine_manual$raw_data
    arizona_vaccine_manual$pull_raw()
    arizona_vaccine_manual$raw_data
    arizona_vaccine_manual$save_raw()
    arizona_vaccine_manual$restruct_raw()
    arizona_vaccine_manual$restruct_data
    arizona_vaccine_manual$extract_from_raw()
    arizona_vaccine_manual$extract_data
    arizona_vaccine_manual$validate_extract()
    arizona_vaccine_manual$save_extract()
}
