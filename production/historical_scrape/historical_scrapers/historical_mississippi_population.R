source("./R/generic_scraper.R")
source("./R/utilities.R")

historical_ms_pop_pull <- function(x, date, file = NULL){
    # Extract all urls 
    url_ <- x %>% 
        xml2::read_html() %>%
        rvest::html_nodes("a") %>%
        rvest::html_attr("href")
    
    # Extract all link texts
    link_ <- x %>%
        xml2::read_html() %>%
        rvest::html_nodes("a") %>%
        rvest::html_text()
    
    # Get url for latest pdf 
    tbl <- tibble(link = link_, url = url_) %>% 
        filter(stringr::str_detect(url_, "DailyInmatePopulation")) %>% 
        mutate(Date = lubridate::my(link, quiet = T))
    
    # Construct url
    stringr::str_c(
        "https://www.mdoc.ms.gov/", 
        tbl %>% 
            filter(lubridate::month(Date) == lubridate::month(date)) %>% 
            filter(lubridate::year(Date) == lubridate::year(date)) %>% 
            pull(url) %>% 
            # Hacky way to handle url redirect behavior 
            str_replace_all(., " ", "%20")
    )
}

get_ms_pop_table <- function(x){
    x %>% 
        as.data.frame() %>% 
        janitor::row_to_names(row_number = 1) %>% 
        pivot_longer(!c("Location", "Capacity"), 
                     names_to = "Date", 
                     values_to = "Residents.Population")
}

historical_ms_pop_restruct <- function(x, date = NULL){
    bind_rows(
        x %>% 
            tabulizer::extract_tables(pages = 1) %>% 
            get_ms_pop_table(), 
        
        x %>% 
            tabulizer::extract_tables(pages = 2) %>% 
            get_ms_pop_table()
    )
}
       
historical_ms_pop_extract <- function(x, date = NULL){
    drop_names <- c(
        "COMMUNITY WORK CENTERS", 
        "REGIONAL CORRECTIONAL FACILITIES", 
        "PRIVATE PRISONS", 
        "TRANSITIONAL HOUSING", 
        "TOTALS", 
        "RESTITUTION CENTERS",
        "REGIONAL CORRECTIONAL FACILITIE", 
        ""
    )
    
    x_ <- x %>% 
        filter(! Date == "Avg Population") %>% 
        mutate(Date = lubridate::dmy(Date)) %>% 
        select(
            Name = Location, 
            Date, 
            Residents.Population
        ) %>% 
        clean_scraped_df() %>% 
        filter(!is.na(Residents.Population)) 
    
    filtered <- x_ %>% 
        filter(!Name %in% drop_names) 
    
    out <- filtered %>% 
        filter(Date == date) %>% 
        select(-Date)
    
    # Check that total matches sum of facilities 
    total <- x_ %>% 
        filter(str_detect(Name, "(?i)total")) %>% 
        filter(Date == date) %>% 
        pull(Residents.Population)
    
    if (total != sum_na_rm(out$Residents.Population)){
        warning(str_c("Total population ", total, " different from facility sum ",
                   sum_na_rm(out$Residents.Population), ". Inspect raw file."))
    }
    
    out
}

#' Scraper class for historical Mississippi population data 
#' 
#' @name historical_ms_pop_scraper
#' @description The Mississippi DOC posts and archives daily population data in 
#' a wide format. This scraper pulls the relevant archived file based on the date. 
#' \describe{
#'   \item{Location}{}
#'   \item{Capacity}{}
#'   \item{Daily Inmate Population}{}
#' }

historical_ms_pop_scraper <- R6Class(
    "historical_ms_pop_scraper",
    inherit = generic_scraper,
    public = list(
        log = NULL,
        initialize = function(
            log,
            url = "https://www.mdoc.ms.gov/Admin-Finance/Pages/Daily-Inmate-Population.aspx",
            id = "historical_ms_pop",
            type = "pdf",
            state = "MS",
            jurisdiction = "state",
            check_date = NULL,
            pull_func = historical_ms_pop_pull,
            restruct_func = historical_ms_pop_restruct,
            extract_func = historical_ms_pop_extract){
            super$initialize(
                url = url, id = id, pull_func = pull_func, type = type,
                restruct_func = restruct_func, extract_func = extract_func,
                log = log, state = state, jurisdiction  = jurisdiction, 
                check_date = check_date)
        }
    )
)

if(sys.nframe() == 0){
    historical_ms_pop <- historical_ms_pop_scraper$new(log=TRUE)
    historical_ms_pop$reset_date("DATE")
    historical_ms_pop$raw_data
    historical_ms_pop$pull_raw(date = historical_ms_pop$date, .dated_pull = TRUE)
    historical_ms_pop$raw_data
    historical_ms_pop$save_raw()
    historical_ms_pop$restruct_raw()
    historical_ms_pop$restruct_data
    historical_ms_pop$extract_from_raw(date = historical_ms_pop$date)
    historical_ms_pop$extract_data
    historical_ms_pop$validate_extract()
    historical_ms_pop$save_extract()
}
    