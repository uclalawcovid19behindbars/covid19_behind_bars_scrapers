source("./R/generic_scraper.R")
source("./R/utilities.R")

georgia_youth_pull <- function(x){
    xml2::read_html(x)
}

georgia_youth_restruct <- function(x){
    x %>%
        rvest::html_nodes('table') %>%
        .[[1]] %>%
        rvest::html_table()
}

georgia_youth_extract <- function(x){
    check_names(x,
                c("DJJ Location",
                  "Staff\nConfirmed",
                  "Staff\nRecovered",
                  "Youth\nConfirmed",
                  "Youth\nRecovered"))
    
    clean <- x %>%
        select(Name = "DJJ Location", 
               Residents.Confirmed = `Youth\nConfirmed`,
               Residents.Recovered = `Youth\nRecovered`,
               Staff.Confirmed = `Staff\nConfirmed`,
               Staff.Recovered = `Staff\nRecovered`
               ) %>%
        mutate(Name = str_c(toupper(Name), " YOUTH"))
    
    out <- clean %>%
        clean_scraped_df() %>%
        as_tibble()
    
    return(out)
}

#' Scraper class for general Georgia Youth COVID data
#' 
#' @name georgia_youth_scraper
#' @description GA data self contained within html table. 
#' \describe{
#'   \item{Location}{The facilty name}
#'   \item{Staff.Confirmed}{Staff Confirmed}
#'   \item{Staff.Recovered}{Staff Recovered}
#'   \item{Residents.Confirmed}{Residents Confirmed}
#'   \item{Residents.Recovered}{Residents Recovered}
#' }

georgia_youth_scraper <- R6Class(
    "georgia_youth_scraper",
    inherit = generic_scraper,
    public = list(
        log = NULL,
        initialize = function(
            log,
            url = "https://djj.georgia.gov/news-cloned/2021-04-14/covid-19-case-update",
            id = "georgia_youth",
            type = "html",
            state = "GA",
            jurisdiction = "state",
            # pull the JSON data directly from the API
            pull_func = georgia_youth_pull,
            # restructuring the data means pulling out the data portion of the json
            restruct_func = georgia_youth_restruct,
            # Rename the columns to appropriate database names
            extract_func = georgia_youth_extract){
            super$initialize(
                url = url, id = id, pull_func = pull_func, type = type,
                restruct_func = restruct_func, extract_func = extract_func,
                log = log, state = state, jurisdiction = jurisdiction)
        }
    )
)

if(sys.nframe() == 0){
    georgia_youth <- georgia_youth_scraper$new(log=TRUE)
    georgia_youth$raw_data
    georgia_youth$pull_raw()
    georgia_youth$raw_data
    georgia_youth$save_raw()
    georgia_youth$restruct_raw()
    georgia_youth$restruct_data
    georgia_youth$extract_from_raw()
    georgia_youth$extract_data
    georgia_youth$validate_extract()
    georgia_youth$save_extract()
}

