source("./R/generic_scraper.R")
source("./R/utilities.R")

# Tableau downloads from Firefox aren't working  
# Download the csv file manually in Chrome and save it in this location 
wisconsin_vaccine_pull <- function(x){
    read.csv("/tmp/sel_dl/Vaccines.csv")
}

wisconsin_vaccine_restruct <- function(x, exp_date = Sys.Date()){
    check_names(x, c("X", "As.of.Date..Vaccine."))
    names(x) <- c("Name", "Residents.Initiated")
    
    if (x[1, 1] != "Facility"){
        warning("Facility not in expected position, check if file structure has changed")
    }
    
    date <- x[1, 2] %>% lubridate::mdy()
    error_on_date(date, exp_date)
    
    x
}

wisconsin_vaccine_extract <- function(x){
    x %>% 
        filter(!Name == "Facility") %>% 
        filter(!str_detect(Name, "(?i)total")) %>% 
        clean_scraped_df()
}

#' Scraper class for general COVID data
#' 
#' @name wisconsin_vaccine_scraper
#' @description Comes from the COVID-19 Vaccines tab in the dashboard. 
#' Firefox/Tableau compatibility is an issue, so the scraper assumes you'll 
#' manually download the csv and put it in /tmp/sel_dl/. 
#' Dashboard says data is updated on Monday of every week. 
#' \describe{
#'   \item{Facility_Name}{The facility name.}
#'   \item{PIOC vaccinated}{The sum of PIOC who have received at 
#'   least one dose of a COVID-19 vaccine}
#' }

wisconsin_vaccine_scraper <- R6Class(
    "wisconsin_vaccine_scraper",
    inherit = generic_scraper,
    public = list(
        log = NULL,
        initialize = function(
            log,
            url = "https://doc.wi.gov/Pages/COVID19(Coronavirus)/COVID19TestingDashboard.aspx",
            id = "wisconsin_vaccine",
            type = "manual",
            state = "WI",
            jurisdiction = "state",
            # pull the JSON data directly from the API
            pull_func = wisconsin_vaccine_pull,
            # restructuring the data means pulling out the data portion of the json
            restruct_func = wisconsin_vaccine_restruct,
            # Rename the columns to appropriate database names
            extract_func = wisconsin_vaccine_extract){
            super$initialize(
                url = url, id = id, pull_func = pull_func, type = type,
                restruct_func = restruct_func, extract_func = extract_func,
                log = log, state = state, jurisdiction = jurisdiction)
        }
    )
)

if(sys.nframe() == 0){
    wisconsin_vaccine <- wisconsin_vaccine_scraper$new(log=TRUE)
    wisconsin_vaccine$raw_data
    wisconsin_vaccine$pull_raw()
    wisconsin_vaccine$raw_data
    wisconsin_vaccine$save_raw()
    wisconsin_vaccine$restruct_raw()
    wisconsin_vaccine$restruct_data
    wisconsin_vaccine$extract_from_raw()
    wisconsin_vaccine$extract_data
    wisconsin_vaccine$validate_extract()
    wisconsin_vaccine$save_extract()
}

