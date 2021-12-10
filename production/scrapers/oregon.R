source("./R/generic_scraper.R")
source("./R/utilities.R")

oregon_check_date <- function(url, date = Sys.Date()){
    base_html <- xml2::read_html(url)
    date_txt <- rvest::html_nodes(base_html, 
                                  xpath="//*[@id=\"ctl00_MainContentPlaceHolder_PageContentPlaceHolder_RichHtmlField1__ControlWrapper_OregonRichHtmlField\"]/ul/li[1]") %>%
        rvest::html_text()
    
    date_txt %>%
        {.[str_detect(., "(?i)21")]} %>%
        str_split(., "(?i)as of:") %>%
        unlist() %>%
        .[2] %>%
        lubridate::mdy() %>%
        error_on_date(expected_date = date)
}

oregon_pull <- function(url){
    remDr <- RSelenium::remoteDriver(
        remoteServerAddr = "localhost",
        port = 4445,
        browserName = "firefox"
    )
    
    del_ <- capture.output(remDr$open())
    remDr$navigate(url)
    
    remDr$getPageSource() %>%
        {xml2::read_html(.[[1]])}
}

oregon_restruct <- function(raw_html){
    raw_html %>%
        rvest::html_node("table") %>%
        rvest::html_table() %>%
        mutate(Location = ifelse(
            Location == "STATE TOTAL", "State-Wide", Location)) %>% 
        mutate(Staff.Recovered = ifelse(
            Location != "State-Wide", NA,
            numeric_from_css(raw_html, '.ms-rteElement-Heading4:nth-child(5)')
            ))
}

oregon_extract <- function(restructured_data){
    restructured_data %>%
        select(Name = Location,
               Staff.Confirmed = "Staff Positives",
               Staff.Recovered,
               Residents.Confirmed = "AIC Positives to Date",
               Residents.Recovered = "Total AICs Recovered",
               Residents.Negative = "AIC Negatives to Date",
               Residents.Active = "Current AIC Active Cases",
               Residents.Deaths = "AIC Deaths") %>%
        clean_scraped_df() %>%
        mutate(Residents.Tadmin = 
                   ifelse(Name == "State-Wide", NA, Residents.Confirmed + Residents.Negative)) %>% 
        mutate(Staff.Confirmed =
                   ifelse(Name == "State-Wide", NA, Staff.Confirmed)) %>%
        # doing this because Residents.Confirmed = positive tests, not positive cases (?)
        mutate(Residents.Confirmed =
                   ifelse(Name == "State-Wide", NA, Residents.Confirmed)) %>%
        mutate(Residents.Negative =
                   ifelse(Name == "State-Wide", NA, Residents.Negative)) %>%
        mutate(Residents.Active =
                   ifelse(Name == "State-Wide", NA, Residents.Active)) %>%
        as_tibble()
        
}

#' Scraper class for general Oregon COVID data
#' 
#' @name oregon_scraper
#' @description Oregon data comes from an HTML table that has been altered 3
#' times from April 2020 to October 2020. Data is limited to those currently in
#' custody.
#' \describe{
#'   \item{Location}{}
#'   \item{Tier Status}{}
#'   \item{Current AIC Active Cases}{}
#'   \item{Total AICs Recovered}{}
#'   \item{AIC Positives to Date}{}
#'   \item{AIC Negatives to Date}{}
#'   \item{Positive AICs Paroled}{}
#'   \item{AIC Deaths}{}
#'   \item{Staff Positives}{}
#'   \item{Staff.Recovered}{}
#' }

oregon_scraper <- R6Class(
    "oregon_scraper",
    inherit = generic_scraper,
    public = list(
        log = NULL,
        initialize = function(
            log,
            url = "https://www.oregon.gov/doc/covid19/Pages/covid19-tracking.aspx",
            id = "oregon",
            type = "html",
            state = "OR",
            jurisdiction = "state",
            check_date = oregon_check_date,
            # pull the JSON data directly from the API
            pull_func = oregon_pull,
            # restructuring the data means pulling out the data portion of the json
            restruct_func = oregon_restruct,
            # Rename the columns to appropriate database names
            extract_func = oregon_extract){
            super$initialize(
                url = url, id = id, pull_func = pull_func, type = type,
                restruct_func = restruct_func, extract_func = extract_func,
                log = log, state = state, jurisdiction = jurisdiction,
                check_date = check_date)
        }
    )
)

if(sys.nframe() == 0){
    oregon <- oregon_scraper$new(log=TRUE)
    oregon$run_check_date()
    oregon$raw_data
    oregon$pull_raw()
    oregon$raw_data
    oregon$save_raw()
    oregon$restruct_raw()
    oregon$restruct_data
    oregon$extract_from_raw()
    oregon$extract_data
    oregon$validate_extract()
    oregon$save_extract()
}

