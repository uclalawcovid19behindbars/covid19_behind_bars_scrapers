source("./R/generic_scraper.R")
source("./R/utilities.R")
source("./R/selenium_driver.R")

pennsylvania_bi_testing_pull <- function(url, wait = 7){
    # scrape from the power bi iframe directly
    url = str_c(
    "https://app.powerbigov.us/view?r=",
    "eyJrIjoiMzQ4MGIzNzUtYmU5Mi00MGQxLTlkMTgtYm",
    "ZhZWM4NDc3YmIxIiwidCI6IjQxOGUyODQxLTAxMjgt",
    "NGRkNS05YjZjLTQ3ZmM1YTlhMWJkZSJ9")
    remDr <- initiate_remote_driver()
    remDr$open(silent = TRUE)
    remDr$navigate(url)
    
    Sys.sleep(wait)
    
    next_node <- remDr$findElement("xpath", "//button[@aria-label='Next Page']")
    next_node$clickElement()
    Sys.sleep(2)
    next_node$clickElement()
    Sys.sleep(2)
    next_node$clickElement()
    Sys.sleep(2)
    next_node$clickElement()
    Sys.sleep(2)
    
    raw_html <- xml2::read_html(remDr$getPageSource()[[1]])
    
    remDr$close()
    
    is_covid_testing <- raw_html %>%
        rvest::html_nodes("h3.preTextWithEllipsis") %>%
        rvest::html_text() %>%
        str_detect("(?=.*Inmate)(?=.*Tests)(?=.*Facility)") %>%
        any()
    
    if(!is_covid_testing){
        warning("Page structure may have changed please inspect.")
    }
    
    raw_html
}

pennsylvania_bi_testing_restruct  <- function(raw_html){
  
    labs <- raw_html %>%
        rvest::html_nodes("text.setFocusRing") %>%
        sapply(function(z){
            rvest::html_text(rvest::html_node(z, "title"))})
    
    windows <- raw_html %>%
        rvest::html_nodes(".labelGraphicsContext")
    
    
    possible_values <- lapply(windows, function(z){
        z %>%
            rvest::html_nodes(".label") %>%
            rvest::html_text()
    })
    ## if a bar is missing a label, manually insert it
    # possible_values[[2]] <- append(possible_values[[2]], "1440", 19)
    
    idx <- which(lapply(possible_values, length) == length(labs))
    
    if(length(idx) != 1){
        stop("Page is not as expected please inspect.")
    }
    
    tibble(
        Name = labs,
        Residents.Tadmin = possible_values[[idx]]
    )    
}

pennsylvania_bi_testing_extract <- function(restructured_data){
    restructured_data %>%
        clean_scraped_df()
}

#' Scraper class for general PA testing data from dashboard
#' 
#' @name pennsylvania_bi_testing_scraper
#' @description One page in PAs power BI tool which is dedicated to inmate
#' testing. We scrape each page with relevant data from the PA bi tool with
#' separate scrapers.
#' 
#' \describe{
#'   \item{Facility}{Facility abbreviation}
#'   \item{Tests}{Residents.Tadmin}

#' }

pennsylvania_bi_testing_scraper <- R6Class(
    "pennsylvania_bi_testing_scraper",
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
            id = "pennsylvania_bi_testing",
            type = "html",
            state = "PA",
            jurisdiction = "state",
            check_date = NULL,
            # pull the JSON data directly from the API
            pull_func = pennsylvania_bi_testing_pull,
            # restructuring the data means pulling out the data portion of the 
            restruct_func = pennsylvania_bi_testing_restruct,
            # Rename the columns to appropriate database names
            extract_func = pennsylvania_bi_testing_extract){
            super$initialize(
                url = url, id = id, pull_func = pull_func, type = type,
                restruct_func = restruct_func, extract_func = extract_func,
                log = log, state = state, jurisdiction = jurisdiction,
                check_date = check_date)
        }
    )
)

if(sys.nframe() == 0){
    pennsylvania_bi_testing <- pennsylvania_bi_testing_scraper$new(log=TRUE)
    pennsylvania_bi_testing$run_check_date()
    pennsylvania_bi_testing$raw_data
    pennsylvania_bi_testing$pull_raw()
    pennsylvania_bi_testing$raw_data
    pennsylvania_bi_testing$save_raw()
    pennsylvania_bi_testing$restruct_raw()
    pennsylvania_bi_testing$restruct_data
    pennsylvania_bi_testing$extract_from_raw()
    pennsylvania_bi_testing$extract_data
    pennsylvania_bi_testing$validate_extract()
    pennsylvania_bi_testing$save_extract()
}
