source("./R/generic_scraper.R")
source("./R/utilities.R")
source("./R/selenium_driver.R")

california_statewide_check_date <- function(x, date = Sys.Date()){
    # scrape from the power bi iframe directly
    y <- "https://app.powerbigov.us/view?r=" %>%
        str_c(
            "eyJrIjoiODBjZjExNDktYWUxNi00NmM1LTllODMtY2VkMDM1MjlkODRiIiwidCI", 
            "6IjA2NjI0NzdkLWZhMGMtNDU1Ni1hOGY1LWMzYmM2MmFhMGQ5YyJ9&", 
            "pageName=ReportSectionc5f6f269bd82e37ccad7"
        )
    remDr <- initiate_remote_driver()
    remDr$open(silent = TRUE)
    remDr$navigate(y)
    
    Sys.sleep(15)
    
    base_page <- xml2::read_html(remDr$getPageSource()[[1]])
    
    remDr$quit()
    
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

california_statewide_pull <- function(x, wait = 25){
    tf <- tempfile(fileext = ".png")
    
    # scrape from the power bi iframe directly
    y <- "https://app.powerbigov.us/view?r=" %>%
        str_c(
            "eyJrIjoiODBjZjExNDktYWUxNi00NmM1LTllODMtY2VkMDM1MjlkODRiIiwidCI", 
            "6IjA2NjI0NzdkLWZhMGMtNDU1Ni1hOGY1LWMzYmM2MmFhMGQ5YyJ9&", 
            "pageName=ReportSectionc5f6f269bd82e37ccad7"
        )

    remDr <- initiate_remote_driver()
    remDr$open(silent = TRUE)
    remDr$navigate(y)
    
    Sys.sleep(wait)
    
    remDr$screenshot(file = tf)
    
    remDr$quit()
    
    magick::image_read(tf)
}

california_statewide_restruct <- function(x){
    tadmin_txt <- x %>%
        magick::image_crop("400x120+400+140") %>%
        magick::image_ocr() %>%
        clean_fac_col_txt()
    
    if(!str_detect(tadmin_txt, "(?i)tests performed")){
        warning("Field does mot match expected text")
    }
    
    tadmin_num <- as.numeric(
        str_c(unlist(str_extract_all(tadmin_txt, "[0-9]+")), collapse = ""))
    
    if(is.na(tadmin_num)){
        warning("State-wide # of tests extracted is NA - please inspect")
    }
    
    if(!(tadmin_num > 2000000)){
      warning("State-wide # of tests extracted is less than 2 million - please inspect")
    }
    
    tibble(Residents.Tadmin = tadmin_num, 
           Name = "State-Wide")
}

#' Scraper class for general california_statewide COVID data
#' 
#' @name california_statewide_scraper
#' @description california_statewide data scraped from rendered power BI iframe. Testing
#' data is also available at the facility level but will need to be scraped
#' separately. Note that time series data for the state exists for comparison.
#' Added Tadmin around June 18 2021. 
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
            extract_func = function(x){x}){
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
    california_statewide$run_check_date()
    california_statewide$raw_data
    california_statewide$pull_raw()
    california_statewide$raw_data
    california_statewide$save_raw()
    california_statewide$restruct_raw()
    california_statewide$restruct_data
    california_statewide$extract_from_raw()
    california_statewide$extract_data
    california_statewide$validate_extract()
    california_statewide$save_extract()
}

