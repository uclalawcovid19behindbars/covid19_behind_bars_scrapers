source("./R/generic_scraper.R")
source("./R/utilities.R")

historical_iowa_pop_pull <- function(x, file, date = NULL){
    file %>% 
        xml2::read_html()
}

historical_iowa_pop_restruct <- function(x, date = NULL){
    x %>%
        rvest::html_nodes("table") %>%
        .[[1]] %>%
        rvest::html_table(fill = TRUE)
}

historical_iowa_pop_extract <- function(x, date = NULL){
    expected_names <- c(
        "Institution", "Current Count", "Capacity", "Medical/Segregation")
    
    check_names(x, expected_names)
    
    x %>% 
        select(Name = Institution, 
               Residents.Population = `Current Count`) %>% 
        filter(!Name %in% c("INSTITUTIONAL TOTALS", "% overcrowded by")) %>% 
        # Rename and aggregate facility names to match COVID data 
        mutate(Name = case_when(
            # Report Live-Out separately, rename facility to denote this 
            Name == "Minimum Live-Out" & lag(Name) == "Mitchellville" ~ "Mitchellville Minimum Live-Out", 
            # Aggregate Oakdale with psychiatric hospital (IMCC in COVID data)
            Name == "Forensic Psychiatric Hospital" & lag(Name) == "Oakdale" ~ "Oakdale", 
            # Aggregate Newton medium and minimum  into Newton 
            Name == "Minimum" & lag(Name) == "Newton-Medium" ~ "Newton", 
            Name == "Newton-Medium" ~ "Newton", 
            TRUE ~ Name)) %>% 
        clean_scraped_df() %>% 
        # Sum population across aggregated names 
        group_by(Name) %>% 
        summarise(Residents.Population = sum(Residents.Population)) %>% 
        ungroup() 
}

#' Scraper class for Iowa population data
#' 
#' @name historical_iowa_population_scraper
#' @description html table with minimal recoding and cleaning. Some name aggregating 
#' to match how COVID data is reported. 
#' \describe{
#'   \item{Institution}{The faciilty name}
#'   \item{Current Count}{Current population}
#'   \item{Capacity}{Facility capacity}
#'   \item{Medical/Segregation}{Population in medical care/segregation}
#' }

historical_iowa_pop_scraper <- R6Class(
    "historical_iowa_pop_scraper",
    inherit = generic_scraper,
    public = list(
        log = NULL,
        initialize = function(
            log,
            url = "https://doc.iowa.gov/daily-statistics",
            id = "historical_iowa_pop",
            state = "IA",
            type = "html",
            jurisdiction = "state",
            check_date = NULL,
            pull_func = historical_iowa_pop_pull,
            restruct_func = historical_iowa_pop_restruct,
            extract_func = historical_iowa_pop_extract){
            super$initialize(
                url = url, id = id, pull_func = pull_func, type = type,
                restruct_func = restruct_func, extract_func = extract_func,
                log = log, state = state, jurisdiction = jurisdiction,
                check_date = check_date)
        })
)

if(sys.nframe() == 0){
    historical_iowa_pop <- historical_iowa_pop_scraper$new(log=TRUE)
    historical_iowa_pop$reset_date("DATE")
    historical_iowa_pop$raw_data
    historical_iowa_pop$pull_raw(file, .dated_pull = TRUE)
    historical_iowa_pop$raw_data
    historical_iowa_pop$save_raw()
    historical_iowa_pop$restruct_raw()
    historical_iowa_pop$restruct_data
    historical_iowa_pop$extract_from_raw()
    historical_iowa_pop$extract_data
    historical_iowa_pop$validate_extract()
    historical_iowa_pop$save_extract()
}
