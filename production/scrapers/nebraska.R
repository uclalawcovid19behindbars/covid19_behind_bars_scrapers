source("./R/generic_scraper.R")
source("./R/utilities.R")

nebraska_check_date <- function(x, date = Sys.Date()){
    base_html <- xml2::read_html(x)
    date_txt <- rvest::html_nodes(base_html, xpath="//*[@id=\"node-1516\"]/div/div/div[2]/div/div/h2[1]/span/span") %>%
        rvest::html_text()
    
    date_txt %>%
        {.[str_detect(., "(?i)Updated")]} %>%
        str_split("Updated ") %>%
        unlist() %>%
        .[2] %>%
        lubridate::mdy() %>%
        error_on_date(expected_date = date)
}


nebraska_pull <- function(x){
    xml2::read_html(x)
}

nebraska_restruct <- function(x){
    
    list(
        all_tabs = x %>%
            rvest::html_nodes("table") %>%
            lapply(rvest::html_table),
    
        active = x %>%
            rvest::html_node(
                xpath = str_c(
                    # find the span that has active cases
                    "//span[contains(text(), 'ACTIVE CASES')]",
                    # get the grandparent node and find the next table for
                    # active cases
                    "/../../following-sibling::table[1]")) %>%
            rvest::html_table() %>%
            as_tibble() %>%
            rename(Name = X1, Residents.Active = X2))
}

nebraska_extract <- function(x){
    bind_cols(x$all_tabs[sapply(x$all_tabs, function(y) nrow(y) == 1)]) %>%
        mutate(Name = "State-Wide") %>%
        clean_scraped_df() %>%
        select(
            Name,
            Residents.Tadmin = "Total Tests Administered",
            Residents.Confirmed = "Total Number of Confirmed Cases",
            Residents.Recovered = "Total Recovered Cases",
            Residents.Deaths = "Total Deaths") %>%
        as_tibble() %>%
        bind_rows(x$active)
}

#' Scraper class for general nebraska COVID data
#' 
#' @name nebraska_scraper
#' @description NE reports data mostly at the state level within several easy
#' to scrape html tables.
#' \describe{
#'   \item{Facility_Name}{The facility name}
#'   \item{Active}{Residents currently infected at facility}
#'   \item{Total Tests Administered}{State-wide only}
#'   \item{Total People Tested}{State-wide only}
#'   \item{Total Number of Confirmed Cases}{State-wide only}
#'   \item{Total Recovered Cases}{State-wide only}
#'   \item{Total Deaths}{State-wide only}
#' }

nebraska_scraper <- R6Class(
    "nebraska_scraper",
    inherit = generic_scraper,
    public = list(
        log = NULL,
        initialize = function(
            log,
            url = "https://corrections.nebraska.gov/ndcs-covid-19-data",
            id = "nebraska",
            type = "html",
            state = "NE",
            jurisdiction = "state",
            check_date = nebraska_check_date,
            # pull the JSON data directly from the API
            pull_func = nebraska_pull,
            # restructuring the data means pulling out the data portion of the json
            restruct_func = nebraska_restruct,
            # Rename the columns to appropriate database names
            extract_func = nebraska_extract){
            super$initialize(
                url = url, id = id, pull_func = pull_func, type = type,
                restruct_func = restruct_func, extract_func = extract_func,
                log = log, state = state, jurisdiction = jurisdiction,
                check_date = check_date)
        }
    )
)

if(sys.nframe() == 0){
    nebraska <- nebraska_scraper$new(log=TRUE)
    nebraska$run_check_date()
    nebraska$raw_data
    nebraska$pull_raw()
    nebraska$raw_data
    nebraska$save_raw()
    nebraska$restruct_raw()
    nebraska$restruct_data
    nebraska$extract_from_raw()
    nebraska$extract_data
    nebraska$validate_extract()
    nebraska$save_extract()
}

