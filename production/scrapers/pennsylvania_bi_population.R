source("./R/generic_scraper.R")
source("./R/utilities.R")
source("./R/selenium_driver.R")

pennsylvania_bi_population_pull <- function(url, wait = 7){
    # scrape from the power bi iframe directly

    remDr <- initiate_remote_driver()
    remDr$open(silent = TRUE)
    remDr$navigate(url)
    Sys.sleep(wait)
    
    next_node <- remDr$findElement("xpath", "//button[@aria-label='Next Page']")
    next_node$clickElement()
    Sys.sleep(2)
    
    raw_html <- xml2::read_html(remDr$getPageSource()[[1]])

    remDr$close()
    
    is_population <- raw_html %>%
        rvest::html_nodes(xpath="//h3[@class='preTextWithEllipsis']") %>%
        rvest::html_text() %>%
        str_detect("(?=.*Inmate)(?=.*Population)") %>%
        any()
    
    if(!is_population){
        warning("Page structure may have changed please inspect.")
    }
    
    raw_html
}

pennsylvania_bi_population_restruct  <- function(raw_html){
    val_sr_str <- "//text[@class='label sub-selectable' and contains(@transform,'translate')]"
    lab_sr_str <- "//text[@class='setFocusRing']//title"

    tibble(
        Name = raw_html %>%
            rvest::html_nodes(xpath=lab_sr_str) %>%
            rvest::html_text(),
        
        Residents.Population = raw_html %>%
            rvest::html_nodes(xpath=val_sr_str) %>%
            rvest::html_text())
}




pennsylvania_bi_population_extract <- function(restructured_data){
    restructured_data %>%
        clean_scraped_df()
}

#' Scraper class for general PA population data from dashboard
#' 
#' @name pennsylvania_bi_population_scraper
#' @description One page in PAs power BI tool which is dedicated to inmate
#' population. We scrape each page with relevant data from the PA bi tool with
#' separate scrapers.
#' 
#' \describe{
#'   \item{Facility}{Facility abbreviation}
#'   \item{Population}{inmates population}
#' }

pennsylvania_bi_population_scraper <- R6Class(
    "pennsylvania_bi_population_scraper",
    inherit = generic_scraper,
    public = list(
        log = NULL,
        initialize = function(
            log,
            # The landing page for the BI report is https://www.cor.pa.gov/Pages/COVID-19.aspx
            url = str_c(
                "https://app.powerbigov.us/view?r=",
                "eyJrIjoiMzQ4MGIzNzUtYmU5Mi00MGQxLTlkMTgtYm",
                "ZhZWM4NDc3YmIxIiwidCI6IjQxOGUyODQxLTAxMjgt",
                "NGRkNS05YjZjLTQ3ZmM1YTlhMWJkZSJ9"),
            id = "pennsylvania_bi_population",
            type = "html",
            state = "PA",
            jurisdiction = "state",
            check_date = NULL,
            # pull the JSON data directly from the API
            pull_func = pennsylvania_bi_population_pull,
            # restructuring the data means pulling out the data portion of the 
            restruct_func = pennsylvania_bi_population_restruct,
            # Rename the columns to appropriate database names
            extract_func = pennsylvania_bi_population_extract){
            super$initialize(
                url = url, id = id, pull_func = pull_func, type = type,
                restruct_func = restruct_func, extract_func = extract_func,
                log = log, state = state, jurisdiction = jurisdiction,
                check_date = check_date)
        }
    )
)

if(sys.nframe() == 0){
    pennsylvania_bi_population <- pennsylvania_bi_population_scraper$new(log=TRUE)
    pennsylvania_bi_population$run_check_date()
    pennsylvania_bi_population$raw_data
    pennsylvania_bi_population$pull_raw()
    pennsylvania_bi_population$raw_data
    pennsylvania_bi_population$save_raw()
    pennsylvania_bi_population$restruct_raw()
    pennsylvania_bi_population$restruct_data
    pennsylvania_bi_population$extract_from_raw()
    pennsylvania_bi_population$extract_data
    pennsylvania_bi_population$validate_extract()
    pennsylvania_bi_population$save_extract()
}

