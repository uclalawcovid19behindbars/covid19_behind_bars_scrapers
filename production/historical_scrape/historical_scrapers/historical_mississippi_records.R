source("./R/generic_scraper.R")
source("./R/utilities.R")

historical_ms_records_pull <- function(x, date){
    readxl::read_excel("results/tmp/COVID-19 DAILY REPORT VitalCore 090321.xlsx")
}

historical_ms_records_restruct <- function(x, date){
    
    file_date <- names(x[3]) %>% 
        str_extract("\\d{1,2}.\\d{1,2}.\\d{2,4}") %>% 
        lubridate::mdy() 
        
    if (file_date != date){
        warning(paste("File date", file_date, "different from expected date", date))
    }
    
    x %>% 
        janitor::row_to_names(row_number = 2)
}

historical_ms_records_extract <- function(x){
    
    exp_names <- c(
        "FACILITY", 
        NA, 
        "TOTAL POSITIVES", 
        "TOTAL NEGATIVES", 
        "TOTAL TESTED", 
        "TOTAL DEATHS R/T COVID", 
        "CURRENT ACTIVE", 
        "HOSPITALIZED", 
        "Inmates 1st dose", 
        "% 1st Dose", 
        "Inmates 2nd Dose", 
        "% 2nd Dose" 
    )
    
    check_names(x, exp_names)
    
    names(x) <- c(
        "Type.Drop", 
        "Name", 
        "Residents.Confirmed", 
        "Residents.Negative", 
        "Residents.Tested", 
        "Residents.Deaths", 
        "Residents.Active.Drop", # Dropping bc unclear how often will be updated
        "Residents.Hospitalized.Drop", 
        "Residents.Initiated", 
        "Residents.Initiated.Pct.Drop", 
        "Residents.Completed", 
        "Residents.Completed.Pct.Drop"
    )
    
    x_ <- x %>% 
        mutate(Name = coalesce(Name, Type.Drop)) %>% 
        filter(!is.na(Residents.Tested)) %>% 
        select(-ends_with("Drop")) %>% 
        {suppressWarnings(mutate_at(., vars(-Name), as.numeric))}
    
    fac_ <- x_ %>% 
        filter(!str_detect(Name, "(?i)total"))

    tot_ <- x_ %>% 
        filter(str_detect(Name, "(?i)total")) 
    
    for (i in 2:length(fac_)){ # Start at 2 to skip Name column 
        if (sum_na_rm(fac_[,i]) != sum_na_rm(tot_[,i])){
            stop(paste("Total for", names(fac_[i]), "doesn't match sum of facilities.", 
                       "Check for issues with numeric parsing."))
        }
    }
    
    fac_
}

#' Scraper class for Mississippi COVID data from records request 
#' 
#' @name historical_mississippi_records_scraper
#' @description Facilty-level data on cases, deaths, vaccines provided by the 
#' Mississippi DOC via a records request. 
#' \describe{
#'   \item{Facility}
#'   \item{Total Positives}
#'   \item{Total Negatives}
#'   \item{Total Tested}
#'   \item{Total Deaths R/T COVID}
#'   \item{Current Active}
#'   \item{Hospitalized}
#'   \item{Inmate 1st Dose}
#'   \item{% 1st Dose}
#'   \item{Inmates 2nd Dose}
#'   \item{% 2nd Dose}
#' }

historical_mississippi_records_scraper <- R6Class(
    "historical_mississippi_records_scraper",
    inherit = generic_scraper,
    public = list(
        log = NULL,
        initialize = function(
            log,
            url = "Mississippi Department of Corrections Records Response",
            id = "historical_mississippi_records",
            type = "csv",
            state = "MS",
            jurisdiction = "state",
            check_date = NULL,
            # pull the JSON data directly from the API
            pull_func = historical_ms_records_pull,
            # restructuring the data means pulling out the data portion of the json
            restruct_func = historical_ms_records_restruct,
            # Rename the columns to appropriate database names
            extract_func = historical_ms_records_extract){
            super$initialize(
                url = url, id = id, pull_func = pull_func, type = type,
                restruct_func = restruct_func, extract_func = extract_func,
                log = log, state = state, jurisdiction = jurisdiction,
                check_date = NULL)
        }
    )
)

if(sys.nframe() == 0){
    historical_ms_records <- historical_mississippi_records_scraper$new(log=TRUE)
    historical_ms_records$reset_date("2021-09-03")
    historical_ms_records$raw_data
    historical_ms_records$pull_raw(date = historical_ms_records$date, .dated_pull = TRUE)
    historical_ms_records$raw_data
    historical_ms_records$save_raw()
    historical_ms_records$restruct_raw(date = historical_ms_records$date)
    historical_ms_records$restruct_data
    historical_ms_records$extract_from_raw()
    historical_ms_records$extract_data
    historical_ms_records$validate_extract()
    historical_ms_records$save_extract()
}
