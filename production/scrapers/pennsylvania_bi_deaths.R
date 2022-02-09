source("./R/generic_scraper.R")
source("./R/utilities.R")
source("./R/selenium_driver.R")

pennsylvania_bi_deaths_pull <- function(url, wait = 7){
    # scrape from the power bi iframe directly
    deaths_page <- str_c(url,"&pageName=ReportSectionc6adf1f0d7011b2ed62c")
    
    remDr <- initiate_remote_driver()
    remDr$open(silent = TRUE)
    remDr$navigate(deaths_page)
    
    Sys.sleep(wait)
    
    raw_html <- xml2::read_html(remDr$getPageSource()[[1]])
    
    remDr$close()
    
    is_covid_deaths <- raw_html %>%
        rvest::html_nodes(xpath="//h3[@class='preTextWithEllipsis']") %>%
        rvest::html_text() %>%
        str_detect("(?=.*Inmate)(?=.*Death)(?=.*COVID)") %>%
        any()
    
    if(!is_covid_deaths){
        warning("Page structure may have changed please inspect.")
    }
    
    raw_html
}

pennsylvania_bi_deaths_restruct  <- function(raw_html){
    val_sr_str <- "//text[@class='label' and contains(@transform,'translate')]"
    lab_sr_str <- "//text[@class='setFocusRing']//title"

    tibble(
        Name = raw_html %>%
            rvest::html_nodes(xpath=lab_sr_str) %>%
            rvest::html_text(),
        
        Residents.Deaths = raw_html %>%
            rvest::html_nodes(xpath=val_sr_str) %>%
            rvest::html_text())
}




pennsylvania_bi_deaths_extract <- function(restructured_data){
    restructured_data %>%
        clean_scraped_df()
}

#' Scraper class for general PA death data from dashboard
#' 
#' @name pennsylvania_bi_deaths_scraper
#' @description One page in PAs power BI tool which is dedicated to inmate
#' deaths. We scrape each page with relevant data from the PA bi tool with
#' separate scrapers.
#' 
#' \describe{
#'   \item{Facility}{Facility abbreviation}
#'   \item{Deaths}{COVID related death among inmates}
#' }

pennsylvania_bi_deaths_scraper <- R6Class(
    "pennsylvania_bi_deaths_scraper",
    inherit = generic_scraper,
    public = list(
        log = NULL,
        initialize = function(
            log,
            # The landing page for the BI report is https://www.cor.pa.gov/Pages/COVID-19.aspx
            url = str_c(
                "https://app.powerbigov.us/view?r=",
                "eyJrIjoiMTcyY2I2MjMtZjJjNC00NjNjLWJjNWYtNTZlZWE1YmRkYWYwIiwidCI",
                "6IjQxOGUyODQxLTAxMjgtNGRkNS05YjZjLTQ3ZmM1YTlhMWJkZSJ9"),
            id = "pennsylvania_bi_deaths",
            type = "html",
            state = "PA",
            jurisdiction = "state",
            check_date = NULL,
            # pull the JSON data directly from the API
            pull_func = pennsylvania_bi_deaths_pull,
            # restructuring the data means pulling out the data portion of the 
            restruct_func = pennsylvania_bi_deaths_restruct,
            # Rename the columns to appropriate database names
            extract_func = pennsylvania_bi_deaths_extract){
            super$initialize(
                url = url, id = id, pull_func = pull_func, type = type,
                restruct_func = restruct_func, extract_func = extract_func,
                log = log, state = state, jurisdiction = jurisdiction,
                check_date = check_date)
        }
    )
)

if(sys.nframe() == 0){
    pennsylvania_bi_deaths <- pennsylvania_bi_deaths_scraper$new(log=TRUE)
    pennsylvania_bi_deaths$run_check_date()
    pennsylvania_bi_deaths$raw_data
    pennsylvania_bi_deaths$pull_raw()
    pennsylvania_bi_deaths$raw_data
    pennsylvania_bi_deaths$save_raw()
    pennsylvania_bi_deaths$restruct_raw()
    pennsylvania_bi_deaths$restruct_data
    pennsylvania_bi_deaths$extract_from_raw()
    pennsylvania_bi_deaths$extract_data
    pennsylvania_bi_deaths$validate_extract()
    pennsylvania_bi_deaths$save_extract()
}

