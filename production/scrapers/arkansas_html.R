source("./R/generic_scraper.R")
source("./R/utilities.R")

arkansas_html_check_date <- function(x, date = Sys.Date()){
    base_page <- xml2::read_html(x)
    
    site_date <- base_page %>%
        rvest::html_node("h3") %>%
        rvest::html_text() %>%
        lubridate::mdy()
    
    error_on_date(site_date, date)
}

arkansas_html_pull <- function(x){
    xml2::read_html(x)
}

arkansas_html_restruct <- function(x){
    
    fsp <- x %>%
        # look for the latest inmate data
        rvest::html_node(xpath = "//strong[contains(text(), 'INMATES')]") %>%
        # get the parent
        rvest::html_node(xpath = "parent::p")
    
    if(str_detect(rvest::html_text(fsp), "(?i)testing")){
        mid_text <- fsp %>%
            rvest::html_text() %>%
            str_remove("INMATES/RESIDENTS")
    }
    
    else{
        p_txt <- x %>%
            rvest::html_nodes("p") %>%
            rvest::html_text()
        
        fidx <- which((str_detect(p_txt, "INMATES/RESIDENTS")))[1] + 1:2
        
        mid_text <- p_txt[fidx] %>%
            str_c(collapse = "")
        
    }
    
    data_txt <- mid_text %>%
        str_split("Testing ") %>%
        unlist() %>%
        .[.!=""] %>%
        str_split_fixed(" – ", n=2)
    
    ak_vars <- data_txt[,2]
    names(ak_vars) <- data_txt[,1]
    
    as_tibble(t(ak_vars))
}

arkansas_html_extract <- function(x){
    x %>%
        mutate(Name = "STATEWIDE") %>%
        clean_scraped_df() %>%
        mutate(Residents.Active = `positive/not recovered`) %>%
        mutate(Residents.Confirmed =
                   `Residents.Active` + `positive/recovered`) %>%
        select(Name, starts_with("Residents"))
}

#' Scraper class for general Arkansas COVID data
#'
#' @name arkansas_html_scraper
#' @description AR data is downloaded from a new weekly post in html. This
#' scraper is sensitive to changes in the text
#' \describe{
#'   \item{Residents Recovered}{Residents Recovered.}
#'   \item{Residents Positive Not Recovered}{Residents Positive Not Recovered.}
#'   \item{Staff Positive Not Recovered}{Staff Positive Not Recovered.}
#' }

arkansas_html_scraper <- R6Class(
    "arkansas_html_scraper",
    inherit = generic_scraper,
    public = list(
        log = NULL,
        initialize = function(
            log,
            url = "https://doc.arkansas.gov/covid-19-updates/",
            id = "arkansas_html",
            state = "AR",
            type = "html",
            jurisdiction = "state",
            check_date = arkansas_html_check_date,
            # restructuring the data means pulling out the data portion of the json
            pull_func = arkansas_html_pull,
            # restructuring the data means pulling out the data portion of the json
            restruct_func = function(x, ...) arkansas_html_restruct(x, ...),
            # Rename the columns to appropriate database names
            extract_func = arkansas_html_extract){
            super$initialize(
                url = url, id = id, pull_func = pull_func, type = type,
                restruct_func = restruct_func, extract_func = extract_func,
                log = log, state = state, jurisdiction = jurisdiction,
                check_date = check_date)
        }
    )
)

if(sys.nframe() == 0){
    arkansas_html <- arkansas_html_scraper$new(log=T)
    arkansas_html$run_check_date()
    arkansas_html$raw_data
    arkansas_html$pull_raw()
    arkansas_html$raw_data
    arkansas_html$save_raw()
    arkansas_html$restruct_raw()
    arkansas_html$restruct_data
    arkansas_html$extract_from_raw()
    arkansas_html$extract_data
    arkansas_html$validate_extract()
    arkansas_html$save_extract()
}
