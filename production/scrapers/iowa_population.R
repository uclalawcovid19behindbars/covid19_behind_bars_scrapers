source("./R/generic_scraper.R")
source("./R/utilities.R")

iowa_population_pull <- function(x){
    xml2::read_html(x)
}

iowa_population_restruct <- function(x){
    x %>%
        rvest::html_nodes("table") %>%
        .[[1]] %>%
        rvest::html_table(fill = TRUE)
}

iowa_population_extract <- function(x){
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
            # Report Psychiatric separately, rename facility to denote this 
            Name == "Forensic Psychiatric Hospital" & lag(Name == "Oakdale") ~ "Oakdale Forensic Psychiatric Hospital", 
            # Aggregate Newton medium and minimum  
            Name == "Minimum" & lag(Name) == "Newton-Medium" ~ "Newton-Medium", 
            TRUE ~ Name)) %>% 
        clean_scraped_df() %>% 
        # Sum population across aggregated names 
        group_by(Name) %>% 
        summarise(Residents.Population = sum(Residents.Population)) %>% 
        ungroup() 
}

#' Scraper class for Iowa population data
#' 
#' @name iowa_population_scraper
#' @description html table with minimal recoding and cleaning. Some name aggregating 
#' to match how COVID data is reported. 
#' \describe{
#'   \item{Institution}{The faciilty name}
#'   \item{Current Count}{Current population}
#'   \item{Capacity}{Facility capacity}
#'   \item{Medical/Segregation}{Population in medical care/segregation}
#' }

iowa_population_scraper <- R6Class(
    "iowa_population_scraper",
    inherit = generic_scraper,
    public = list(
        log = NULL,
        initialize = function(
            log,
            url = "https://doc.iowa.gov/daily-statistics",
            id = "iowa",
            state = "IA",
            type = "html",
            jurisdiction = "state",
            pull_func = iowa_population_pull,
            restruct_func = iowa_population_restruct,
            extract_func = iowa_population_extract){
            super$initialize(
                url = url, id = id, pull_func = pull_func, type = type,
                restruct_func = restruct_func, extract_func = extract_func,
                log = log, state = state, jurisdiction = jurisdiction)
        })
)

if(sys.nframe() == 0){
    iowa_population <- iowa_population_scraper$new(log = FALSE)
    iowa_population$raw_data
    iowa_population$pull_raw()
    iowa_population$raw_data
    iowa_population$restruct_raw()
    iowa_population$restruct_data
    iowa_population$extract_from_raw()
    iowa_population$extract_data
    iowa_population$validate_extract()
    iowa_population$save_extract()
}
