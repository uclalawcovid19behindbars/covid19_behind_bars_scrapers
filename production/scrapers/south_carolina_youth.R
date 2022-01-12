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
                c("Location", 
                  "Youth", 
                  "Staff"))
    
    clean <- x %>%
        select(Name = "Location", 
               Residents.Confirmed = `Youth`,
               Staff.Confirmed = `Staff`) %>%
        filter(Name != "TOTAL") %>%
        mutate(Name = str_c(toupper(Name), " YOUTH"))
    
    out <- clean %>%
        clean_scraped_df() %>%
        as_tibble() 
    
    return(out)
}

#' Scraper class for general South Carolina Youth COVID data
#' 
#' @name south_carolina_youth_scraper
#' @description SC data self contained within html table.
#' \describe{
#'   \item{Location}{The facilty name}
#'   \item{Staff.Confirmed}{Staff Confirmed}
#'   \item{Residents.Confirmed}{Residents Confirmed}
#' }

south_carolina_youth_scraper <- R6Class(
    "south_carolina_youth_scraper",
    inherit = generic_scraper,
    public = list(
        log = NULL,
        initialize = function(
            log,
            url = "https://djj.sc.gov/Confirmed_COVID-19_Cases",
            id = "south_carolina_youth",
            type = "html",
            state = "SC",
            jurisdiction = "state",
            check_date = NULL,
            # pull the JSON data directly from the API
            pull_func = south_carolina_youth_pull,
            # restructuring the data means pulling out the data portion of the json
            restruct_func = south_carolina_youth_restruct,
            # Rename the columns to appropriate database names
            extract_func = south_carolina_youth_extract){
            super$initialize(
                url = url, id = id, pull_func = pull_func, type = type,
                restruct_func = restruct_func, extract_func = extract_func,
                log = log, state = state, jurisdiction = jurisdiction,
                check_date = check_date)
        }
    )
)

if(sys.nframe() == 0){
    south_carolina_youth <- south_carolina_youth_scraper$new(log=TRUE)
    south_carolina_youth$run_check_date()
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

