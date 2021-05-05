source("./R/generic_scraper.R")
source("./R/utilities.R")

# Tableau downloads from Firefox aren't working  
# Download the csv file manually in Chrome and save it in this location 
wisconsin_vaccine_pull <- function(x){
    # if this is giving you trouble, try save-as'ing it in 
    # excel with UTF-8 .csv file encoding
    read.csv("/tmp/sel_dl/Vaccines.csv")
}

wisconsin_vaccine_restruct <- function(x, exp_date = Sys.Date()){

    x_ <- x %>% 
        janitor::clean_names()
    
    check_names(x_, c(
        "facility", 
        "as_of_date_vaccine", 
        "first_doses_moderna_or_pfizer",
        "second_doses_moderna_or_pfizer", 
        "johnson_johnson_doses", 
        "total_doses"
    ))
    
    # Use earliest date if rows differ 
    date <- min(x_$as_of_date_vaccine) %>% 
        lubridate::mdy()

    error_on_date(date, exp_date)
    
    x_
}

wisconsin_vaccine_extract <- function(x){
    x %>% 
        rename(Name = facility) %>% 
        filter(!str_detect(Name, "(?i)total")) %>% 
        clean_scraped_df() %>% 
        mutate(Residents.Initiated = first_doses_moderna_or_pfizer + johnson_johnson_doses, 
               Residents.Completed = second_doses_moderna_or_pfizer + johnson_johnson_doses, 
               Residents.Vadmin = total_doses) %>% 
        select(Name, starts_with("Res"))
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
#'   \item{As of Date (Vaccine)}{}
#'   \item{First Doses (Moderna or Pfizer)}{}
#'   \item{Second Doses (Moderna or Pfizer)}{}
#'   \item{Johnson & Johnson Doses}
#'   \item{Total Doses}
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

