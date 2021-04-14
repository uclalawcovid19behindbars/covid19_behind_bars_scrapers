source("./R/generic_scraper.R")
source("./R/utilities.R")

louisiana_youth_pull <- function(x){
    xml2::read_html(x)
}

louisiana_youth_restruct <- function(x){
    x %>%
        rvest::html_node('.wp-block-table') %>%
        rvest::html_table()
}

louisiana_youth_extract <- function(x){
    la <- x %>%
        janitor::row_to_names(row_number = 1) 
    
    check_names(la, 
                c("Secure Care Facility", 
                  "# of Youth Positive", 
                  "# of Youth Recovered"))
    
    la_cln <- la %>%
        select(Name = `Secure Care Facility`, 
               Residents.Confirmed = `# of Youth Positive`,
               Residents.Recovered = `# of Youth Recovered`) %>%
        mutate(Name = str_c(toupper(Name), " YOUTH"))
    
    out <- la_cln %>%
        clean_scraped_df() %>%
        as_tibble() %>%
        filter(Name != "TOTAL YOUTH") %>%
    
    return(out)
}

#' Scraper class for general Illinois COVID data
#' 
#' @name louisiana_youth_scraper
#' @description IL data self contained within html table. Death data may have
#' have been reported in the past.
#' \describe{
#'   \item{Location}{The facilty name}
#'   \item{Staff.Confirmed}{Staff Confirmed}
#'   \item{Staff.Recovered}{Staff Recovered}
#'   \item{Residents.Confirmed}{Residents Confirmed}
#'   \item{Residents.Recovered}{Residents Recovered}
#' }

louisiana_youth_scraper <- R6Class(
    "louisiana_youth_scraper",
    inherit = generic_scraper,
    public = list(
        log = NULL,
        initialize = function(
            log,
            url = "https://ojj.la.gov/ojj-covid-19-information/",
            id = "louisiana_youth",
            type = "html",
            state = "IL",
            jurisdiction = "state",
            # pull the JSON data directly from the API
            pull_func = louisiana_youth_pull,
            # restructuring the data means pulling out the data portion of the json
            restruct_func = louisiana_youth_restruct,
            # Rename the columns to appropriate database names
            extract_func = louisiana_youth_extract){
            super$initialize(
                url = url, id = id, pull_func = pull_func, type = type,
                restruct_func = restruct_func, extract_func = extract_func,
                log = log, state = state, jurisdiction = jurisdiction)
        }
    )
)

if(sys.nframe() == 0){
    louisiana_youth <- louisiana_youth_scraper$new(log=TRUE)
    louisiana_youth$raw_data
    louisiana_youth$pull_raw()
    louisiana_youth$raw_data
    louisiana_youth$save_raw()
    louisiana_youth$restruct_raw()
    louisiana_youth$restruct_data
    louisiana_youth$extract_from_raw()
    louisiana_youth$extract_data
    louisiana_youth$validate_extract()
    louisiana_youth$save_extract()
}

