source("./R/generic_scraper.R")
source("./R/utilities.R")

nyc_jails_pull <- function(x){
    xml2::read_html(x)
}

nyc_jails_restruct <- function(x){
    tabs <- x %>%
        rvest::html_nodes("table")
    
    if(str_detect(rvest::html_text(tabs[[1]]), "(?i)deaths")){
        warning("URL MAY HAVE CHANGED SCTRUCTURE INSPECT")
    }
    
    lapply(tabs, rvest::html_table)
}

nyc_jails_extract <- function(x){
    conf_df <- x[[1]] %>%
        mutate(Name = "New York City Jails") %>%
        clean_scraped_df() %>%
        mutate(Residents.Confirmed = `Incarcerated Population` + Parolees) %>%
        rename(Staff.Confirmed = Staff) %>%
        select(-`Incarcerated Population`, -Parolees)
    
    death_df <- x[[2]] %>%
        mutate(Name = "New York City Jails") %>%
        clean_scraped_df() %>%
        mutate(Residents.Deaths = `Incarcerated Population` + Parolees) %>%
        rename(Staff.Deaths = Staff) %>%
        select(-`Incarcerated Population`, -Parolees)
    
    full_join(conf_df, death_df, by = "Name")
}

#' Scraper class for general nyc_jails COVID data
#' 
#' @name nyc_jails_scraper
#' @description This will be a description of nyc_jails data and what the scraper
#' does
#' \describe{
#'   \item{Facility_Name}{The faciilty name.}
#' }

nyc_jails_scraper <- R6Class(
    "nyc_jails_scraper",
    inherit = generic_scraper,
    public = list(
        log = NULL,
        initialize = function(
            log,
            url = "https://doccs.ny.gov/doccs-covid-19-report",
            id = "nyc_jails",
            type = "html",
            state = "NY",
            jurisdiction = "county",
            # pull the JSON data directly from the API
            pull_func = nyc_jails_pull,
            # restructuring the data means pulling out the data portion of the json
            restruct_func = nyc_jails_restruct,
            # Rename the columns to appropriate database names
            extract_func = nyc_jails_extract){
            super$initialize(
                url = url, id = id, pull_func = pull_func, type = type,
                restruct_func = restruct_func, extract_func = extract_func,
                log = log, state = state, jurisdiction = jurisdiction)
        }
    )
)

if(sys.nframe() == 0){
    nyc_jails <- nyc_jails_scraper$new(log=TRUE)
    nyc_jails$raw_data
    nyc_jails$pull_raw()
    nyc_jails$raw_data
    nyc_jails$save_raw()
    nyc_jails$restruct_raw()
    nyc_jails$restruct_data
    nyc_jails$extract_from_raw()
    nyc_jails$extract_data
    nyc_jails$validate_extract()
    nyc_jails$save_extract()
}

