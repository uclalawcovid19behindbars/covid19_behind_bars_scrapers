source("./R/generic_scraper.R")
source("./R/utilities.R")

south_carolina_youth_pull <- function(x){
    xml2::read_html(x)
}

south_carolina_youth_restruct <- function(x){
    x %>%
        rvest::html_nodes('table') %>%
        .[[1]] %>%
        rvest::html_table()
}

south_carolina_youth_extract <- function(x){
    check_names(x, 
                c("IYC Facility", 
                  "Confirmed COVID-19 Cases *", 
                  "Confirmed COVID-19 Cases *"))
    
    clean <- x %>%
        janitor::row_to_names(row_number = 1) %>%
        select(Name = "", 
               Residents.Confirmed = `Youth`,
               Staff.Confirmed = `Staff`) %>%
        mutate(Name = toupper(Name))
    
    out <- clean %>%
        clean_scraped_df() %>%
        as_tibble()
    
    return(out)
}

#' Scraper class for general Illinois COVID data
#' 
#' @name south_carolina_youth_scraper
#' @description IL data self contained within html table. Death data may have
#' have been reported in the past.
#' \describe{
#'   \item{Location}{The facilty name}
#'   \item{Staff.Confirmed}{Staff Confirmed}
#'   \item{Staff.Recovered}{Staff Recovered}
#'   \item{Residents.Confirmed}{Residents Confirmed}
#'   \item{Residents.Recovered}{Residents Recovered}
#' }

south_carolina_youth_scraper <- R6Class(
    "south_carolina_youth_scraper",
    inherit = generic_scraper,
    public = list(
        log = NULL,
        initialize = function(
            log,
            url = "https://www2.south_carolina.gov/idjj/Pages/COVID19.aspx",
            id = "south_carolina_youth",
            type = "html",
            state = "IL",
            jurisdiction = "state",
            # pull the JSON data directly from the API
            pull_func = south_carolina_youth_pull,
            # restructuring the data means pulling out the data portion of the json
            restruct_func = south_carolina_youth_restruct,
            # Rename the columns to appropriate database names
            extract_func = south_carolina_youth_extract){
            super$initialize(
                url = url, id = id, pull_func = pull_func, type = type,
                restruct_func = restruct_func, extract_func = extract_func,
                log = log, state = state, jurisdiction = jurisdiction)
        }
    )
)

if(sys.nframe() == 0){
    south_carolina_youth <- south_carolina_youth_scraper$new(log=TRUE)
    south_carolina_youth$raw_data
    south_carolina_youth$pull_raw()
    south_carolina_youth$raw_data
    south_carolina_youth$save_raw()
    south_carolina_youth$restruct_raw()
    south_carolina_youth$restruct_data
    south_carolina_youth$extract_from_raw()
    south_carolina_youth$extract_data
    south_carolina_youth$validate_extract()
    south_carolina_youth$save_extract()
}
