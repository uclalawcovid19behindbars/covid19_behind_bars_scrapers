source("./R/generic_scraper.R")
source("./R/utilities.R")

pennsylvania_bi_testing_pull <- function(x, wait = 10){
    # scrape from the power bi iframe directly
    y <- "https://app.powerbigov.us/view?r=" %>%
        str_c(
            "eyJrIjoiMTcyY2I2MjMtZjJjNC00NjNjLWJjNWYtNTZlZWE1YmRkYWYwIiwidCI",
            "6IjQxOGUyODQxLTAxMjgtNGRkNS05YjZjLTQ3ZmM1YTlhMWJkZSJ9&",
            "pageName=ReportSection2804d81ebd8989abad15")
    
    remDr <- RSelenium::remoteDriver(
        remoteServerAddr = "localhost", 
        port = 4445,
        browserName = "firefox"
    )
    
    del_ <- capture.output(remDr$open())
    remDr$navigate(y)
    
    Sys.sleep(wait)
    
    raw_html <- xml2::read_html(remDr$getPageSource()[[1]])
    
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

pennsylvania_bi_testing_restruct  <- function(x){
  
    labs <- x %>%
        rvest::html_nodes("text.setFocusRing") %>%
        sapply(function(z){
            rvest::html_text(rvest::html_node(z, "title"))})
    
    windows <- x %>%
        rvest::html_nodes(".labelGraphicsContext")
    
    
    possible_values <- lapply(windows, function(z){
        z %>%
            rvest::html_nodes(".label") %>%
            rvest::html_text()
    })
    
    idx <- which(lapply(possible_values, length) == length(labs))
    
    if(length(idx) != 1){
        stop("Page is not as expected please inspect.")
    }
    
    tibble(
        Name = labs,
        Residents.Tadmin = possible_values[[idx]]
    )    
}

pennsylvania_bi_testing_extract <- function(x){
    x %>%
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
            url = "https://www.cor.pa.gov/Pages/COVID-19.aspx",
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
