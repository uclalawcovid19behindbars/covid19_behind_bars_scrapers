source("./R/generic_scraper.R")
source("./R/utilities.R")

mississippi_records_pull <- function(x){
    readxl::read_excel("/tmp/sel_dl/COVID-19 DAILY REPORT VitalCore 090321.xlsx")
}

mississippi_records_restruct <- function(x){
    x %>% 
        janitor::row_to_names(row_number = 2)
}

mississippi_records_extract <- function(x){
    
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
#' @name mississippi_records_scraper
#' @description MS provides data for a number of facilities through a pdf
#' however minimal data is provided for each facility.
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

mississippi_records_scraper <- R6Class(
    "mississippi_records_scraper",
    inherit = generic_scraper,
    public = list(
        log = NULL,
        initialize = function(
            log,
            url = "Mississippi Department of Corrections",
            id = "mississippi_records",
            type = "csv",
            state = "MS",
            jurisdiction = "state",
            check_date = NULL,
            # pull the JSON data directly from the API
            pull_func = mississippi_records_pull,
            # restructuring the data means pulling out the data portion of the json
            restruct_func = mississippi_records_restruct,
            # Rename the columns to appropriate database names
            extract_func = mississippi_records_extract){
            super$initialize(
                url = url, id = id, pull_func = pull_func, type = type,
                restruct_func = restruct_func, extract_func = extract_func,
                log = log, state = state, jurisdiction = jurisdiction,
                check_date = check_date)
        }
    )
)

if(sys.nframe() == 0){
    mississippi_records <- mississippi_records_scraper$new(log=TRUE)
    mississippi_records$run_check_date()
    mississippi_records$raw_data
    mississippi_records$pull_raw()
    mississippi_records$raw_data
    mississippi_records$save_raw()
    mississippi_records$restruct_raw()
    mississippi_records$restruct_data
    mississippi_records$extract_from_raw()
    mississippi_records$extract_data
    mississippi_records$validate_extract()
    mississippi_records$save_extract()
}
