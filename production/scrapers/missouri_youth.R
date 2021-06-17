source("./R/generic_scraper.R")
source("./R/utilities.R")

missouri_youth_pull <- function(x){
    xml2::read_html(x)
}

missouri_youth_restruct <- function(x){
    x %>%
        rvest::html_nodes('table') %>%
        .[[1]] %>%
        rvest::html_table()
}

missouri_youth_extract <- function(x){
    check_names(x,
                c("Residential Center",
                  "City",
                  "Staff Cases",
                  "Youth Cases",
                  "Total Cases",
                  "Active Cases" # ambiguous whether this is staff, active, or combined
                  ))
    
    clean <- x %>%
        select(Name = `Residential Center`,
               Residents.Confirmed = `Youth Cases`,
               Staff.Confirmed = `Staff Cases`,
        ) %>%
        filter(Name != "TOTAL") %>% 
        mutate(Name = str_c(toupper(Name), " YOUTH"))
    
    out <- clean %>%
        clean_scraped_df() %>%
        as_tibble()
    
    return(out)
}

#' Scraper class for general Missouri Youth COVID data
#' 
#' @name missouri_youth_scraper
#' @description Missouri data self contained within html table. 
#' \describe{
#'   \item{Location}{The facilty name}
#'   \item{Staff.Confirmed}{Staff Confirmed}
#'   \item{Staff.Recovered}{Staff Recovered}
#'   \item{Residents.Confirmed}{Residents Confirmed}
#'   \item{Residents.Recovered}{Residents Recovered}
#' }

missouri_youth_scraper <- R6Class(
    "missouri_youth_scraper",
    inherit = generic_scraper,
    public = list(
        log = NULL,
        initialize = function(
            log,
            url = "https://dss.mo.gov/covid-19/dys-res-centers/index.htm",
            id = "missouri_youth",
            type = "html",
            state = "MO",
            jurisdiction = "state",
            # pull the JSON data directly from the API
            pull_func = missouri_youth_pull,
            # restructuring the data means pulling out the data portion of the json
            restruct_func = missouri_youth_restruct,
            # Rename the columns to appropriate database names
            extract_func = missouri_youth_extract){
            super$initialize(
                url = url, id = id, pull_func = pull_func, type = type,
                restruct_func = restruct_func, extract_func = extract_func,
                log = log, state = state, jurisdiction = jurisdiction)
        }
    )
)

if(sys.nframe() == 0){
    missouri_youth <- missouri_youth_scraper$new(log=TRUE)
    missouri_youth$raw_data
    missouri_youth$pull_raw()
    missouri_youth$raw_data
    missouri_youth$save_raw()
    missouri_youth$restruct_raw()
    missouri_youth$restruct_data
    missouri_youth$extract_from_raw()
    missouri_youth$extract_data
    missouri_youth$validate_extract()
    missouri_youth$save_extract()
}

