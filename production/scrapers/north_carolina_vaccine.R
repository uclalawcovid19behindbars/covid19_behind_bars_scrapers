source("./R/generic_scraper.R")
source("./R/utilities.R")

north_carolina_vaccine_pull <- function(x){
    x %>%
        xml2::read_html()
}

north_carolina_vaccine_restruct <- function(x){
    txt <- x %>%
        rvest::html_nodes("section") %>%
        rvest::html_text() %>% 
        .[which(str_detect(., "COVID-19 Vaccination Info"))] %>% 
        str_squish() %>% 
        str_split("\n") %>% 
        unlist() 
    
    tibble(
        Staff.Initiated = stringr::str_extract(
            txt, pattern = "(?<=Info).*(?=Partially Vaccinated Staff)"), 
        Staff.Completed = stringr::str_extract(
            txt, pattern = "(?<=Partially Vaccinated Staff).*(?=Fully Vaccinated Staff)"), 
        Residents.Initiated = stringr::str_extract(
            txt, pattern = "(?<=Fully Vaccinated Staff).*(?=Partially Vaccinated Offenders)"), 
        Residents.Completed = stringr::str_extract(
            txt, pattern = "(?<=Partially Vaccinated Offenders).*(?=Fully Vaccinated Offenders)")
    )
}

north_carolina_vaccine_extract <- function(x){
    x %>%
        mutate(Name = "STATEWIDE") %>% 
        clean_scraped_df()
}

#' Scraper class for North Carolina vaccine data 
#' 
#' @name north_carolina_vaccine_scraper
#' @description In early March, NC began reporting statewide totals for vaccine data 
#' directly on their main COVID-19 dashboard. They also report vaccination data by 
#' race/ethnicity, not scraped. They include the following note about staff vaccinations
#' "Numbers represent Adult Correction staff who have been vaccinated by NCDPS and 
#' who have self-reported if they were vaccinated by an outside provider. Staff 
#' vaccination is voluntary. Vaccination information is protected through HIPAA." 
#' 
#' \describe{
#'   \item{Partially Vaccinated}{Individuals who have received the initial shot and 
#'   are waiting the required time before receiving the second dose.}
#'   \item{Fully Vaccinated}{Individuals who have received all required doses of the vaccine.}
#' }

north_carolina_vaccine_scraper <- R6Class(
    "north_carolina_vaccine_scraper",
    inherit = generic_scraper,
    public = list(
        log = NULL,
        initialize = function(
            log,
            url = "https://www.ncdps.gov/our-organization/adult-correction/prisons/prisons-info-covid-19",
            id = "north_carolina_vaccine",
            type = "html",
            state = "NC",
            jurisdiction = "state",
            # pull the JSON data directly from the API
            pull_func = north_carolina_vaccine_pull,
            # restructuring the data means pulling out the data portion of the json
            restruct_func = north_carolina_vaccine_restruct,
            # Rename the columns to appropriate database names
            extract_func = north_carolina_vaccine_extract){
            super$initialize(
                url = url, id = id, pull_func = pull_func, type = type,
                restruct_func = restruct_func, extract_func = extract_func,
                log = log, state = state, jurisdiction = jurisdiction)
        }
    )
)

if(sys.nframe() == 0){
    north_carolina_vaccine <- north_carolina_vaccine_scraper$new(log=TRUE)
    north_carolina_vaccine$raw_data
    north_carolina_vaccine$pull_raw()
    north_carolina_vaccine$raw_data
    north_carolina_vaccine$save_raw()
    north_carolina_vaccine$restruct_raw()
    north_carolina_vaccine$restruct_data
    north_carolina_vaccine$extract_from_raw()
    north_carolina_vaccine$extract_data
    north_carolina_vaccine$validate_extract()
    north_carolina_vaccine$save_extract()
}
