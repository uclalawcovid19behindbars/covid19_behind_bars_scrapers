source("./R/generic_scraper.R")
source("./R/utilities.R")

california_staff_check_date <- function(x, date = Sys.Date()){
    base_html <- xml2::read_html(x)
    
    base_html %>%
        rvest::html_node(".page-standard__content") %>%
        rvest::html_nodes("p") %>%
        rvest::html_text() %>%
        {.[str_detect(., "(?i)as of")]} %>%
        str_split("(?i)as of") %>% 
        unlist() %>% 
        .[2] %>% 
        str_squish() %>%
        lubridate::mdy() %>%
        error_on_date(date)
}

california_staff_pull <- function(x, wait = 10){
    xml2::read_html(x)
}

california_staff_restruct <- function(x){
    x %>%
        rvest::html_table(header = TRUE) %>%
        .[[1]] %>%
        as_tibble() %>%
        filter(!str_detect(Locations, "(?i)total"))
}

california_staff_extract <- function(x){
    ext <- c(
        Name = "Locations",
        Staff.Confirmed = "Cumulative Confirmed"
    )
    
    check_names(x, ext)
    
    ext_df <- x
    names(ext_df) <- names(ext)
    
    ext_df %>%
        select(-contains("Drop")) %>%
        clean_scraped_df()
}

#' Scraper class for general california_staff COVID data
#' 
#' @name california_staff_scraper
#' @description california_staff data scraped from html table. Names do not
#' correspond to the names listed in the resident table and must be harmonized.
#' \describe{
#'   \item{Locations}{The facility name.}
#'   \item{Cumulative Confirmed}{The number of confimed cases among staff.}
#'   \item{New In Last 14 Days}{The number of new cases among Staff.}
#'   \item{Active Cases}{The number of active cases among staff.}
#'   \item{Staff Returned to Work}{Staff Recovered.}
#' }

california_staff_scraper <- R6Class(
    "california_staff_scraper",
    inherit = generic_scraper,
    public = list(
        log = NULL,
        initialize = function(
            log,
            url = "https://www.cdcr.ca.gov/covid19/cdcr-cchcs-covid-19-status/",
            id = "california_staff",
            type = "html",
            state = "CA",
            jurisdiction = "state",
            check_date = california_staff_check_date,
            # pull the JSON data directly from the API
            pull_func = california_staff_pull,
            # restructuring the data means pulling out the data portion of the json
            restruct_func = california_staff_restruct,
            # Rename the columns to appropriate database names
            extract_func = california_staff_extract){
            super$initialize(
                url = url, id = id, pull_func = pull_func, type = type,
                restruct_func = restruct_func, extract_func = extract_func,
                log = log, state = state, jurisdiction = jurisdiction,
                check_date = check_date)
        }
    )
)

if(sys.nframe() == 0){
    california_staff <- california_staff_scraper$new(log=TRUE)
    california_staff$run_check_date()
    california_staff$raw_data
    california_staff$pull_raw()
    california_staff$raw_data
    california_staff$restruct_raw()
    california_staff$restruct_data
    california_staff$extract_from_raw()
    california_staff$extract_data
    california_staff$validate_extract()
    california_staff$save_extract()
}

