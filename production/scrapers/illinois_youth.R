source("./R/generic_scraper.R")
source("./R/utilities.R")

illinois_youth_date_check <- function(url, date = Sys.Date()){
    base_html <- xml2::read_html(url)
    
    base_html %>%
        rvest::html_nodes("p") %>%
        rvest::html_text() %>% 
        {.[str_detect(., "(?i)as of")]} %>% 
        {.[str_detect(., "22")]} %>% 
        lubridate::mdy() %>%
        error_on_date(date)
}

illinois_youth_pull <- function(x){
    xml2::read_html(x)
}

illinois_youth_restruct <- function(x){
    x %>%
        rvest::html_nodes('table') %>%
        .[[1]] %>%
        rvest::html_table()
}

illinois_youth_extract <- function(x){
    x <- x %>%
      janitor::row_to_names(row_number = 1) 
  
    check_names(x, 
                c("IYC Facility", 
                  "Confirmed COVID-19 Cases *", 
                  "Confirmed COVID-19 Cases *"))
    
    clean <- x %>%
        janitor::row_to_names(row_number = 1) %>%
        setNames(., c("Name", "Residents.Confirmed", "Staff.Confirmed")) %>%
        mutate(Name = str_c(toupper(Name), " YOUTH"))
    
    out <- clean %>%
        clean_scraped_df() %>%
        as_tibble()
    
    return(out)
}

#' Scraper class for general Illinois Youth COVID data
#' 
#' @name illinois_youth_scraper
#' @description IL data self contained within html table. Death data may have
#' have been reported in the past.
#' \describe{
#'   \item{Location}{The facilty name}
#'   \item{Staff.Confirmed}{Staff Confirmed}
#'   \item{Residents.Confirmed}{Residents Confirmed}
#' }

illinois_youth_scraper <- R6Class(
    "illinois_youth_scraper",
    inherit = generic_scraper,
    public = list(
        log = NULL,
        initialize = function(
            log,
            url = "https://www2.illinois.gov/idjj/Pages/COVID19.aspx",
            id = "illinois_youth",
            type = "html",
            state = "IL",
            jurisdiction = "state",
            check_date = illinois_youth_date_check,
            # pull the JSON data directly from the API
            pull_func = illinois_youth_pull,
            # restructuring the data means pulling out the data portion of the json
            restruct_func = illinois_youth_restruct,
            # Rename the columns to appropriate database names
            extract_func = illinois_youth_extract){
            super$initialize(
                url = url, id = id, pull_func = pull_func, type = type,
                restruct_func = restruct_func, extract_func = extract_func,
                log = log, state = state, jurisdiction = jurisdiction,
                check_date = check_date)
        }
    )
)

if(sys.nframe() == 0){
    illinois_youth <- illinois_youth_scraper$new(log=TRUE)
    illinois_youth$run_check_date()
    illinois_youth$raw_data
    illinois_youth$pull_raw()
    illinois_youth$raw_data
    illinois_youth$save_raw()
    illinois_youth$restruct_raw()
    illinois_youth$restruct_data
    illinois_youth$extract_from_raw()
    illinois_youth$extract_data
    illinois_youth$validate_extract()
    illinois_youth$save_extract()
}

