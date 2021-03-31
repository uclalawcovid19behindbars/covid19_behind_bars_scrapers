source("./R/generic_scraper.R")
source("./R/utilities.R")

california_statewide_check_date <- function(x, date = Sys.Date()){
    # scrape from the power bi iframe directly
    y <- "https://app.powerbigov.us/view?r=" %>%
        str_c(
            "eyJrIjoiODBjZjExNDktYWUxNi00NmM1LTllODMtY2VkMDM1MjlkODRiIiwidCI",
            "6IjA2NjI0NzdkLWZhMGMtNDU1Ni1hOGY1LWMzYmM2MmFhMGQ5YyJ9&",
            "pageName=ReportSection90204f76f18a02b19c96&",
            "pageName=ReportSection5125b4fa2c9a288b050a")
    
    remDr <- RSelenium::remoteDriver(
        remoteServerAddr = "localhost",
        port = 4445,
        browserName = "firefox"
    )
    
    del_ <- capture.output(remDr$open())
    remDr$navigate(y)
    
    Sys.sleep(15)
    
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

california_statewide_pull <- function(x, wait = 15){
    tf <- tempfile(fileext = ".png")
    
    # scrape from the power bi iframe directly
    y <- "https://app.powerbigov.us/view?r=" %>%
        str_c(
            "eyJrIjoiODBjZjExNDktYWUxNi00NmM1LTllODMtY2VkMDM1MjlkODRiIiwidCI",
            "6IjA2NjI0NzdkLWZhMGMtNDU1Ni1hOGY1LWMzYmM2MmFhMGQ5YyJ9&",
            "pageName=ReportSection90204f76f18a02b19c96&",
            "pageName=ReportSection5125b4fa2c9a288b050a")
    
    remDr <- RSelenium::remoteDriver(
        remoteServerAddr = "localhost",
        port = 4445,
        browserName = "firefox"
    )
    
    del_ <- capture.output(remDr$open())
    remDr$navigate(y)
    
    Sys.sleep(wait)
    
    remDr$screenshot(file = tf)
    
    magick::image_read(tf)
}

california_statewide_restruct <- function(x){
    sub_txt <- x %>%
        magick::image_crop("690x30+375+150") %>%
        magick::image_ocr() %>%
        clean_fac_col_txt()
    
    if(!str_detect(sub_txt, "(?i)test")){
        warning("Field does mot match expected text")
    }
    
    as.numeric(str_c(unlist(str_extract_all(sub_txt, "[0-9]+")), collapse = ""))
}

california_statewide_extract <- function(x){
    tibble(Name = "State-Wide", Residents.Tadmin = x)
}

#' Scraper class for general california_statewide COVID data
#' 
#' @name california_statewide_scraper
#' @description california_statewide data scraped from rendered power BI iframe. Testing
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

california_statewide_scraper <- R6Class(
    "california_statewide_scraper",
    inherit = generic_scraper,
    public = list(
        log = NULL,
        initialize = function(
            log,
            url = "https://www.cdcr.ca.gov/covid19/updates/",
            id = "california_statewide",
            type = "img",
            state = "CA",
            jurisdiction = "state",
            check_date = california_statewide_check_date,
            # pull the JSON data directly from the API
            pull_func = california_statewide_pull,
            # restructuring the data means pulling out the data portion of the json
            restruct_func = california_statewide_restruct,
            # Rename the columns to appropriate database names
            extract_func = california_statewide_extract){
            super$initialize(
                url = url, id = id, pull_func = pull_func, type = type,
                restruct_func = restruct_func, extract_func = extract_func,
                log = log, state = state, jurisdiction = jurisdiction,
                check_date = check_date)
        }
    )
)

if(sys.nframe() == 0){
    california_statewide <- california_statewide_scraper$new(log=TRUE)
    california_statewide$raw_data
    california_statewide$pull_raw()
    california_statewide$raw_data
    california_statewide$restruct_raw()
    california_statewide$restruct_data
    california_statewide$extract_from_raw()
    california_statewide$extract_data
    california_statewide$validate_extract()
    california_statewide$save_extract()
}

