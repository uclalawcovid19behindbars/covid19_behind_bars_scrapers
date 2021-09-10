source("./R/generic_scraper.R")
source("./R/utilities.R")

georgia_youth_check_date <- function(x, date = Sys.Date()){
    base_html <- xml2::read_html(x)
    
    base_html %>%
        rvest::html_node(".page-top--news") %>%
        rvest::html_nodes("p") %>%
        rvest::html_text() %>% 
        str_squish() %>%
        lubridate::mdy() %>%
        error_on_date(date)
}

georgia_youth_pull <- function(x){
    xml2::read_html(x)
}

georgia_youth_restruct <- function(x){
    x %>%
        rvest::html_nodes('table') %>%
        .[[1]] %>%
        rvest::html_table()
}

georgia_youth_extract <- function(x){
    check_names(x,
                c("DJJ Location",
                  "Staff\nConfirmed",
                  "Staff\nRecovered",
                  "Youth\nConfirmed",
                  "Youth\nRecovered"))
    
    clean <- x %>%
        janitor::clean_names() %>% 
        select(Name = `djj_location`, 
               Residents.Confirmed = `youth_confirmed`,
               Residents.Recovered = `youth_recovered`,
               Staff.Confirmed = `staff_confirmed`,
               Staff.Recovered = `staff_recovered`
               ) %>%
        mutate(Name = str_c(toupper(Name), " YOUTH")) %>%
        filter(!str_detect(Name, "(i?)TOTAL"))
    
    out <- clean %>%
        clean_scraped_df() %>%
        as_tibble()
    
    return(out)
}

#' Scraper class for general Georgia Youth COVID data
#' 
#' @name georgia_youth_scraper
#' @description GA data self contained within html table. 
#' \describe{
#'   \item{Location}{The facilty name}
#'   \item{Staff.Confirmed}{Staff Confirmed}
#'   \item{Staff.Recovered}{Staff Recovered}
#'   \item{Residents.Confirmed}{Residents Confirmed}
#'   \item{Residents.Recovered}{Residents Recovered}
#' }

georgia_youth_scraper <- R6Class(
    "georgia_youth_scraper",
    inherit = generic_scraper,
    public = list(
        log = NULL,
        initialize = function(
            log,
            url = "https://djj.georgia.gov/news-cloned/2021-04-14/covid-19-case-update",
            id = "georgia_youth",
            type = "html",
            state = "GA",
            jurisdiction = "state",
            check_date = georgia_youth_check_date,
            # pull the JSON data directly from the API
            pull_func = georgia_youth_pull,
            # restructuring the data means pulling out the data portion of the json
            restruct_func = georgia_youth_restruct,
            # Rename the columns to appropriate database names
            extract_func = georgia_youth_extract){
            super$initialize(
                url = url, id = id, pull_func = pull_func, type = type,
                restruct_func = restruct_func, extract_func = extract_func,
                log = log, state = state, jurisdiction = jurisdiction,
                check_date = check_date)
        }
    )
)

if(sys.nframe() == 0){
    georgia_youth <- georgia_youth_scraper$new(log=TRUE)
    georgia_youth$run_check_date()
    georgia_youth$raw_data
    georgia_youth$pull_raw()
    georgia_youth$raw_data
    georgia_youth$save_raw()
    georgia_youth$restruct_raw()
    georgia_youth$restruct_data
    georgia_youth$extract_from_raw()
    georgia_youth$extract_data
    georgia_youth$validate_extract()
    georgia_youth$save_extract()
}

