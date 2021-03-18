source("./R/generic_scraper.R")
source("./R/utilities.R")

kansas_vaccine_pull <- function(x){
    xml2::read_html(x)
}

kansas_vaccine_restruct <- function(x){
    x %>%
        rvest::html_node("table") %>%
        rvest::html_table() %>%
        as_tibble()
}

kansas_vaccine_extract <- function(x){
    x %>%
        select(
            Name = Facility, 
            Residents.Initiated = `Total Residents Vaccinated`
        ) %>% 
        mutate(Name = clean_fac_col_txt(Name), 
               Residents.Initiated = stringr::str_replace(Residents.Initiated, '\\*', '')) %>%
        clean_scraped_df() 
}

#' Scraper class for Kansas vaccine data
#' 
#' @name kansas_vaccine_scraper
#' @description Kansas's DOC posts facility-level data on the total number of 
#' residents vaccinated and the number of residents vaccinated in a given week. 
#' Footnotes say: "KDOC vaccine allocations currently require two dosages about 
#' a month apart. Data reflected here are number of residents, not the number of 
#' vaccines administered." and "The vaccines allocated to KDOC are for ages 18 
#' and up. This limits the number of eligible residents at KJCC."      
#' 
#' \describe{
#'   \item{Facility}{The faciilty name}
#'   \item{Residents Vaccinated Week of: ...}{}
#'   \item{Total Residents Vaccinated}{}
#' }

kansas_vaccine_scraper <- R6Class(
    "kansas_vaccine_scraper",
    inherit = generic_scraper,
    public = list(
        log = NULL,
        initialize = function(
            log,
            url = "https://www.doc.ks.gov/kdoc-coronavirus-updates/covid-19-vaccinations",
            id = "kansas_vaccine",
            type = "html",
            state = "KS",
            jurisdiction = "state",
            # pull the JSON data directly from the API
            pull_func = kansas_vaccine_pull,
            # restructuring the data means pulling out the data portion of the json
            restruct_func = kansas_vaccine_restruct,
            # Rename the columns to appropriate database names
            extract_func = kansas_vaccine_extract){
            super$initialize(
                url = url, id = id, pull_func = pull_func, type = type,
                restruct_func = restruct_func, extract_func = extract_func,
                log = log, state = state, jurisdiction = jurisdiction)
        }
    )
)

if(sys.nframe() == 0){
    kansas_vaccine <- kansas_vaccine_scraper$new(log=TRUE)
    kansas_vaccine$raw_data
    kansas_vaccine$pull_raw()
    kansas_vaccine$raw_data
    kansas_vaccine$save_raw()
    kansas_vaccine$restruct_raw()
    kansas_vaccine$restruct_data
    kansas_vaccine$extract_from_raw()
    kansas_vaccine$extract_data
    kansas_vaccine$validate_extract()
    kansas_vaccine$save_extract()
}

