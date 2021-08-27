source("./R/generic_scraper.R")
source("./R/utilities.R")

california_check_date <- function(x, date = Sys.Date()){
    # scrape from the power bi iframe directly
    y <- "https://app.powerbigov.us/view?r=" %>%
        str_c(
            "eyJrIjoiODBjZjExNDktYWUxNi00NmM1LTllODMtY2VkMDM1MjlkODRiIiwidCI",
            "6IjA2NjI0NzdkLWZhMGMtNDU1Ni1hOGY1LWMzYmM2MmFhMGQ5YyJ9&",
            "pageName=ReportSection90204f76f18a02b19c96")
    
    remDr <- RSelenium::remoteDriver(
        remoteServerAddr = "localhost",
        port = 4445,
        browserName = "firefox"
    )
    
    del_ <- capture.output(remDr$open())
    remDr$navigate(y)
    
    Sys.sleep(10)
    
    base_page <- xml2::read_html(remDr$getPageSource()[[1]])
    
    site_date <- base_page %>%
        rvest::html_nodes("title") %>%
        rvest::html_text() %>%
        {.[str_starts(., "(?i)data last updated")]} %>%
        str_remove("(?i)data last updated: ") %>%
        lubridate::mdy_hm() %>%
        lubridate::floor_date(unit="day") %>%
        as.Date()
    
    error_on_date(site_date, date)
}

california_pull <- function(x, wait = 10){
    # scrape from the power bi iframe directly
    y <- "https://app.powerbigov.us/view?r=" %>%
        str_c(
            "eyJrIjoiODBjZjExNDktYWUxNi00NmM1LTllODMtY2VkMDM1MjlkODRiIiwidCI",
            "6IjA2NjI0NzdkLWZhMGMtNDU1Ni1hOGY1LWMzYmM2MmFhMGQ5YyJ9&",
            "pageName=ReportSection90204f76f18a02b19c96")
    
    remDr <- RSelenium::remoteDriver(
        remoteServerAddr = "localhost",
        port = 4445,
        browserName = "firefox"
    )
    
    del_ <- capture.output(remDr$open())
    remDr$navigate(y)
    
    Sys.sleep(wait)
    
    xml2::read_html(remDr$getPageSource()[[1]])
}

california_restruct <- function(x, date = Sys.Date()){

    tab <- x %>%
        rvest::html_node(".tableEx") %>%
        rvest::html_node(".innerContainer")
    
    col_dat <- tab %>%
        rvest::html_node(".bodyCells") %>%
        rvest::html_node("div") %>%
        rvest::html_children()
    
    dat_df <- do.call(rbind, lapply(col_dat, function(p){
        sapply(rvest::html_children(p), function(z){
            z %>% 
                rvest::html_nodes("div") %>%
                rvest::html_attr("title")})})) %>%
        as.data.frame()

    names(dat_df) <- tab %>%
        rvest::html_node(".columnHeaders") %>%
        rvest::html_node("div") %>%
        rvest::html_nodes("div") %>% 
        rvest::html_attr("title") %>%
        na.omit() %>%
        as.vector()
    
    dat_df %>%
        select(-Institution) %>%
        rename(Name = "Institution Name") %>%
        mutate_at(vars(-Name), string_to_clean_numeric) %>%
        as_tibble()
}

california_extract <- function(x){
    ext <- c(
        "Name", "Confirmed", "New In Last 14 Days", "Active In Custody",
        "Released While Active", "Resolved", "Deaths"
    )
    
    check_names(x, ext)
    
    ext_df <- x
    names(ext_df) <- c(
        "Name", "Residents.Confirmed", "Drop.New", "Residents.Active",
        "Drop.Released", "Residents.Recovered", "Residents.Deaths")
    
    ext_df %>%
        select(-contains("Drop"))
}

#' Scraper class for general California COVID data
#' 
#' @name california_scraper
#' @description California data scraped from rendered power BI iframe. Testing
#' data is also available at the facility level but will need to be scraped
#' separately. Note that time series data for the state exists for comparison.
#' \describe{
#'   \item{Institution Name}{The facility name.}
#'   \item{Confirmed}{The number of confimed cases among residents.}
#'   \item{New In Last 14 Days}{The number of new cases among residents.}
#'   \item{Active in Custody}{The number of active cases.}
#'   \item{Released While Active}{Residents released after testing positive.}
#'   \item{Resolved}{Recovered cases.}
#'   \item{Deaths}{Resident Deaths.}
#' }

california_scraper <- R6Class(
    "california_scraper",
    inherit = generic_scraper,
    public = list(
        log = NULL,
        initialize = function(
            log,
            url = "https://www.cdcr.ca.gov/covid19/updates/",
            id = "california",
            type = "html",
            state = "CA",
            jurisdiction = "state",
            check_date = california_check_date,
            # pull the JSON data directly from the API
            pull_func = california_pull,
            # restructuring the data means pulling out the data portion of the json
            restruct_func = california_restruct,
            # Rename the columns to appropriate database names
            extract_func = california_extract){
            super$initialize(
                url = url, id = id, pull_func = pull_func, type = type,
                restruct_func = restruct_func, extract_func = extract_func,
                log = log, state = state, jurisdiction = jurisdiction,
                check_date = check_date)
        }
    )
)

if(sys.nframe() == 0){
    california <- california_scraper$new(log=TRUE)
    california$run_check_date()
    california$raw_data
    california$pull_raw()
    california$raw_data
    california$save_raw()
    california$restruct_raw()
    california$restruct_data
    california$extract_from_raw()
    california$extract_data
    california$validate_extract()
    california$save_extract()
}

