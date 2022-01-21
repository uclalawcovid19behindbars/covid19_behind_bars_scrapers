source("./R/generic_scraper.R")
source("./R/utilities.R")

pennsylvania_bi_staff_cases_pull <- function(url, wait = 7){
    # scrape from the power bi iframe directly
    staff_cases_page <- str_c(url,"&pageName=ReportSectiond9eef38c45a60b9e059a")
    
    remDr <- RSelenium::remoteDriver(
        remoteServerAddr = "localhost",
        port = 4445,
        browserName = "firefox"
    )
    
    del_ <- capture.output(remDr$open())
    remDr$navigate(staff_cases_page)
    
    Sys.sleep(wait)
    
    raw_html <- xml2::read_html(remDr$getPageSource()[[1]])
    
    remDr$quit()
    
    is_covid_cases <- raw_html %>%
        rvest::html_node("h3.preTextWithEllipsis") %>%
        rvest::html_text() %>%
        str_detect("(?=.*Staff)(?=.*Cases)") %>%
        any()
    
    if(!is_covid_cases){
        warning("Page structure may have changed please inspect.")
    }
    
    raw_html
}

pennsylvania_bi_staff_cases_restruct  <- function(raw_html){
    
    windows <- raw_html %>%
        rvest::html_nodes(xpath="//transform[@class='bringToFront']")
    
    df_list <- bind_rows(lapply(1:length(windows), function(i){
        
        wtitle <- windows[[i]] %>%
            rvest::html_nodes(".preTextWithEllipsis") %>%
            rvest::html_text() %>%
            {ifelse(length(.) < 1, "", .)}
        
        if(str_detect(wtitle, "(?i)facility")){
            if(str_detect(wtitle, "(?i)active")){
                sub_df <- tibble(
                    measure = "Staff.Active",
                    
                    Value = windows[[i]] %>%
                        rvest::html_nodes(".label") %>%
                        rvest::html_text(),
                    
                    Name = windows[[i]] %>%
                        rvest::html_nodes("text.setFocusRing") %>%
                        sapply(function(z){
                            rvest::html_text(rvest::html_node(z, "title"))})
                )
            }
            else if(str_detect(wtitle, "(?i)cumulative")){
                sub_df <- tibble(
                    measure = "Staff.Confirmed",
                    
                    Value = windows[[i]] %>%
                        rvest::html_nodes(".label") %>%
                        rvest::html_text(),
                    
                    Name = windows[[i]] %>%
                        rvest::html_nodes("text.setFocusRing") %>%
                        sapply(function(z){
                            rvest::html_text(rvest::html_node(z, "title"))})
                )
            }
        }
        else{
            sub_df <- tibble(Name = vector(mode = "character"))
        }
        sub_df
        })) %>%
        pivot_wider(names_from = measure, values_from = Value)
    
    
}

pennsylvania_bi_staff_cases_extract <- function(restructured_data){
    restructured_data %>%
        clean_scraped_df() %>%
        select(-starts_with("Drop"))
        
}

#' Scraper class for general PA death staff cases from dashboard
#' 
#' @name pennsylvania_bi_staff_cases_scraper
#' @description One page in PAs power BI tool which is dedicated to staff
#' cases. We scrape each page with relevant data from the PA bi tool with
#' separate scrapers.
#' 
#' \describe{
#'   \item{Facility}{Facility abbreviation}
#'   \item{Active}{COVID active cases among staff}
#'   \item{Confirmed}{COVID confirmed cases among staff}
#' }

pennsylvania_bi_staff_cases_scraper <- R6Class(
    "pennsylvania_bi_staff_cases_scraper",
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
            id = "pennsylvania_bi_staff_cases",
            type = "html",
            state = "PA",
            jurisdiction = "state",
            check_date = NULL, 
            # pull the JSON data directly from the API
            pull_func = pennsylvania_bi_staff_cases_pull,
            # restructuring the data means pulling out the data portion of the 
            restruct_func = pennsylvania_bi_staff_cases_restruct,
            # Rename the columns to appropriate database names
            extract_func = pennsylvania_bi_staff_cases_extract){
            super$initialize(
                url = url, id = id, pull_func = pull_func, type = type,
                restruct_func = restruct_func, extract_func = extract_func,
                log = log, state = state, jurisdiction = jurisdiction,
                check_date = check_date)
        }
    )
)

if(sys.nframe() == 0){
    pennsylvania_bi_staff_cases <- pennsylvania_bi_staff_cases_scraper$new(log=TRUE)
    pennsylvania_bi_staff_cases$run_check_date()
    pennsylvania_bi_staff_cases$raw_data
    pennsylvania_bi_staff_cases$pull_raw()
    pennsylvania_bi_staff_cases$raw_data
    pennsylvania_bi_staff_cases$save_raw()
    pennsylvania_bi_staff_cases$restruct_raw()
    pennsylvania_bi_staff_cases$restruct_data
    pennsylvania_bi_staff_cases$extract_from_raw()
    pennsylvania_bi_staff_cases$extract_data
    pennsylvania_bi_staff_cases$validate_extract()
    pennsylvania_bi_staff_cases$save_extract()
}

