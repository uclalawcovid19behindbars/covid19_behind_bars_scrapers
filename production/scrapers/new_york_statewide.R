source("./R/generic_scraper.R")
source("./R/utilities.R")

new_york__statewide_check_date <- function(url, date = Sys.Date()){
    base_html <- xml2::read_html(url)
    date_txt <- rvest::html_nodes(base_html, "h4") %>%
        rvest::html_text()

    date_txt %>%
        {.[str_detect(., "(?i)20")]} %>% # look for year 20xx
        lubridate::mdy() %>%
        error_on_date(expected_date = date)
}

new_york_statewide_pull <- function(url){
    
    base_html <- xml2::read_html(url)
    
}

new_york_statewide_restruct <- function(raw_html){
    tables <- raw_html %>%
        rvest::html_nodes("table")
    
    is_confirmed <- tables[[1]] %>%
        rvest::html_nodes("h5") %>%
        rvest::html_text() %>%
        str_detect("(?i)confirmed")
    
    if(!is_confirmed){
        warning("Table structure may have changed")
    }
    
    tables[[1]] %>%
        rvest::html_table() %>%
        select(Staff.Confirmed = Staff) %>%
        mutate(Name = "State-Wide") %>%
        mutate(
            Staff.Deaths = raw_html %>%
                rvest::html_node(
                    xpath="//h5[contains(text(),'DEATH')]/following::table") %>%
                   rvest::html_table() %>%
                   pull(Staff))
}

new_york_statewide_extract <- function(restructured_data){
    restructured_data %>%
        clean_scraped_df() %>%
        as_tibble()
}

#' Scraper class for general NY statewide COVID data
#' 
#' @name new_york_statewide_scraper
#' @description NY only produces staff data at the state level within the
#' html of the site. Data is scrapped from tables.
#' \describe{
#'   \item{Staff Deaths}{Staff Deaths}
#'   \item{Staff Confirmed}{Staff Confirmed}
#' }

new_york_statewide_scraper <- R6Class(
    "new_york_statewide_scraper",
    inherit = generic_scraper,
    public = list(
        log = NULL,
        initialize = function(
            log,
            url = "https://doccs.ny.gov/doccs-covid-19-report",
            id = "new_york_statewide",
            type = "html",
            state = "NY",
            jurisdiction = "state",
            check_date = new_york__statewide_check_date,
            # pull the JSON data directly from the API
            pull_func = new_york_statewide_pull,
            # restructuring the data means pulling out the data portion of the json
            restruct_func = new_york_statewide_restruct,
            # Rename the columns to appropriate database names
            extract_func = new_york_statewide_extract){
            super$initialize(
                url = url, id = id, pull_func = pull_func, type = type,
                restruct_func = restruct_func, extract_func = extract_func,
                log = log, state = state, jurisdiction = jurisdiction,
                check_date = check_date)
        }
    )
)

if(sys.nframe() == 0){
    new_york_statewide <- new_york_statewide_scraper$new(log=TRUE)
    new_york_statewide$run_check_date()
    new_york_statewide$raw_data
    new_york_statewide$pull_raw()
    new_york_statewide$raw_data
    new_york_statewide$save_raw()
    new_york_statewide$restruct_raw()
    new_york_statewide$restruct_data
    new_york_statewide$extract_from_raw()
    new_york_statewide$extract_data
    new_york_statewide$validate_extract()
    new_york_statewide$save_extract()
}

