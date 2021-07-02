source("./R/generic_scraper.R")
source("./R/utilities.R")

mississippi_pop_pull <- function(x, date = "2021-06-30"){
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

mississippi_pop_restruct <- function(x){
    bind_rows(
        x %>% 
            tabulizer::extract_tables(pages = 1) %>% 
            get_ms_pop_table(), 
        
        x %>% 
            tabulizer::extract_tables(pages = 2) %>% 
            get_ms_pop_table()
    )
}

mississippi_pop_extract <- function(x){
    drop_names <- c(
        "COMMUNITY WORK CENTERS", 
        "REGIONAL CORRECTIONAL FACILITIES", 
        "PRIVATE PRISONS", 
        "TRANSITIONAL HOUSING", 
        "TOTALS", 
        "RESTITUTION CENTERS",
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
        filter(Date == max(filtered$Date)) %>% 
        select(-Date)
    
    # Check that total matches sum of facilities 
    total <- x_ %>% 
        filter(str_detect(Name, "(?i)total")) %>% 
        filter(Date == max(filtered$Date)) %>% 
        pull(Residents.Population)
    
    if (total != sum_na_rm(out$Residents.Population)){
        stop(str_c("Total population ", total, " different from expected ", 
                   sum_na_rm(out$Residents.Population), ". Inspect raw file."))
    }
    
    out
}

#' Scraper class for Mississippi population data 
#' 
#' @name mississippi_population_scraper
#' @description The Mississippi DOC posts daily population data in a wide format. 
#' This scraper pulls the latest date available. 
#' \describe{
#'   \item{Location}{}
#'   \item{Capacity}{}
#'   \item{Daily Inmate Population}{}
#' }

mississippi_population_scraper <- R6Class(
    "mississippi_population_scraper",
    inherit = generic_scraper,
    public = list(
        log = NULL,
        initialize = function(
            log,
            url = "https://www.mdoc.ms.gov/Admin-Finance/Pages/Daily-Inmate-Population.aspx",
            id = "mississippi_population",
            type = "pdf",
            state = "MS",
            jurisdiction = "state",
            check_date = NULL,
            pull_func = mississippi_pop_pull,
            restruct_func = mississippi_pop_restruct,
            extract_func = mississippi_pop_extract){
            super$initialize(
                url = url, id = id, pull_func = pull_func, type = type,
                restruct_func = restruct_func, extract_func = extract_func,
                log = log, state = state, jurisdiction  = jurisdiction,
                check_date = check_date)
        }
    )
)

if(sys.nframe() == 0){
    mississippi_population <- mississippi_population_scraper$new(log=TRUE)
    mississippi_population$run_check_date()
    mississippi_population$raw_data
    mississippi_population$pull_raw()
    mississippi_population$raw_data
    mississippi_population$save_raw()
    mississippi_population$restruct_raw()
    mississippi_population$restruct_data
    mississippi_population$extract_from_raw()
    mississippi_population$extract_data
    mississippi_population$validate_extract()
    mississippi_population$save_extract()
}
