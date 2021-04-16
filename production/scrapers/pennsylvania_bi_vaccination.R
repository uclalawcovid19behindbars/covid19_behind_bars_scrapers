source("./R/generic_scraper.R")
source("./R/utilities.R")

pennsylvania_bi_vaccination_pull <- function(x, wait = 10){
    # scrape from the power bi iframe directly
    y <- "https://app.powerbigov.us/view?r=" %>%
        str_c(
            "eyJrIjoiMTcyY2I2MjMtZjJjNC00NjNjLWJjNWYtNTZlZWE1YmRkYWYwIiwidCI",
            "6IjQxOGUyODQxLTAxMjgtNGRkNS05YjZjLTQ3ZmM1YTlhMWJkZSJ9",
            "&pageName=ReportSection7b14ce996120a5295481")
    
    remDr <- RSelenium::remoteDriver(
        remoteServerAddr = "localhost",
        port = 4445,
        browserName = "firefox"
    )
    
    del_ <- capture.output(remDr$open())
    remDr$navigate(y)
    
    Sys.sleep(wait)
    
    raw_html <- xml2::read_html(remDr$getPageSource()[[1]])
    
    is_vacc <- raw_html %>%
        rvest::html_node(xpath="//div[@class='preTextWithEllipsis']") %>%
        rvest::html_text() %>%
        str_detect("(?i)vaccination") %>%
        any()
    
    if(!is_vacc){
        warning("Page structure may have changed please inspect.")
    }
    
    raw_html
}

pennsylvania_bi_vaccination_restruct  <- function(x){
    data_cards <- x %>%
        rvest::html_nodes(xpath="//svg[@class='card']/../../..")
    
    titles <- x %>%
        rvest::html_nodes(xpath="//div[@class='preTextWithEllipsis']") %>%
        rvest::html_text()
    
    res_idx <- min(which(str_detect(titles, "(?i)inmate")))
    staff_idx <- min(which(str_detect(titles, "(?i)staff")))
    
    if(res_idx > staff_idx){
        stop("Scraper is not as expected, please inspect.")
    }
    
    data_cards <- x %>%
        rvest::html_nodes(xpath="//svg[@class='card']/../../..")
    
    card_labs <- sapply(data_cards, function(z){
        rvest::html_attr(rvest::html_nodes(z, "div.visualTitle"), "title")
        }) %>%
        str_replace(" ", ".")
    
    card_group <- rep(c("Residents.", "Staff."), each = length(card_labs)/2)
    
    card_vals <- sapply(data_cards, function(z){
        rvest::html_text(rvest::html_nodes(z, "title"))
        }) %>%
        string_to_clean_numeric()
    
    tibble(
        name = str_c(card_group, card_labs),
        value = card_vals
    )
}

pennsylvania_bi_vaccination_extract <- function(x){
    x %>%
        pivot_wider() %>%
        mutate(Residents.Initiated = Residents.Partial + Residents.Full) %>%
        mutate(Staff.Initiated = Staff.Partial + Staff.Full) %>%
        select(-ends_with("Not.Vaccinated"), -ends_with("Partial")) %>%
        mutate(Name = "STATEWIDE") %>%
        clean_scraped_df() %>%
        rename(
            Residents.Completed = Residents.Full, Staff.Completed = Staff.Full)
}

#' Scraper class for general PA death data from dashboard
#' 
#' @name pennsylvania_bi_vaccination_scraper
#' @description One page in PAs power BI tool which is dedicated to inmate
#' and staff vaccinations. We scrape each page with relevant data from the PA
#' bi tool with separate scrapers.
#' 
#' \describe{
#'   \item{Facility}{Facility abbreviation}
#'   \item{Partial}{Completed - Initiated}
#'   \item{Full}{Completed}
#' }

pennsylvania_bi_vaccination_scraper <- R6Class(
    "pennsylvania_bi_vaccination_scraper",
    inherit = generic_scraper,
    public = list(
        log = NULL,
        initialize = function(
            log,
            url = "https://www.cor.pa.gov/Pages/COVID-19.aspx",
            id = "pennsylvania_bi_vaccination",
            type = "html",
            state = "PA",
            jurisdiction = "state",
            # pull the JSON data directly from the API
            pull_func = pennsylvania_bi_vaccination_pull,
            # restructuring the data means pulling out the data portion of the 
            restruct_func = pennsylvania_bi_vaccination_restruct,
            # Rename the columns to appropriate database names
            extract_func = pennsylvania_bi_vaccination_extract){
            super$initialize(
                url = url, id = id, pull_func = pull_func, type = type,
                restruct_func = restruct_func, extract_func = extract_func,
                log = log, state = state, jurisdiction = jurisdiction)
        }
    )
)

if(sys.nframe() == 0){
    pennsylvania_bi_vaccination <- pennsylvania_bi_vaccination_scraper$new(log=TRUE)
    pennsylvania_bi_vaccination$raw_data
    pennsylvania_bi_vaccination$pull_raw()
    pennsylvania_bi_vaccination$raw_data
    pennsylvania_bi_vaccination$save_raw()
    pennsylvania_bi_vaccination$restruct_raw()
    pennsylvania_bi_vaccination$restruct_data
    pennsylvania_bi_vaccination$extract_from_raw()
    pennsylvania_bi_vaccination$extract_data
    pennsylvania_bi_vaccination$validate_extract()
    pennsylvania_bi_vaccination$save_extract()
}

