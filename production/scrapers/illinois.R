source("./R/generic_scraper.R")
source("./R/utilities.R")

illinois_pull <- function(x){
    xml2::read_html(x)
}

illinois_restruct <- function(x){
    x %>%
        rvest::html_nodes('table') %>%
        .[[1]] %>%
        rvest::html_table()
}

illinois_extract <- function(x){
    exp_names <- c(
        Name = "Locations",
        Staff.Confirmed = "Staff Confirmed",
        Staff.Recovered = "Staff Recovered",
        Drop.Staff.Active = "Staff Current",
        Residents.Confirmed = "OffendersConfirmed",
        Residents.Recovered = "OffendersRecovered",
        Residents.Active = "Offenders Current"
    )
    
    il <- x
    check_names(il, exp_names)
    
    names(il) <- names(exp_names)
    
    il$Name <- toupper(il$Name)
    
    il %>%
        filter(!str_detect(Name, "(?i)total")) %>%
        select(-starts_with("Drop")) %>%
        clean_scraped_df() %>%
        as_tibble()
}

#' Scraper class for general Illinois COVID data
#' 
#' @name illinois_scraper
#' @description IL data self contained within html table. Death data may have
#' have been reported in the past.
#' \describe{
#'   \item{Location}{The facilty name}
#'   \item{Staff.Confirmed}{Staff Confirmed}
#'   \item{Staff.Recovered}{Staff Recovered}
#'   \item{Residents.Confirmed}{Residents Confirmed}
#'   \item{Residents.Recovered}{Residents Recovered}
#' }

illinois_scraper <- R6Class(
    "illinois_scraper",
    inherit = generic_scraper,
    public = list(
        log = NULL,
        initialize = function(
            log,
            url = "https://www2.illinois.gov/idoc/facilities/Pages/Covid19Response.aspx",
            id = "illinois",
            type = "html",
            state = "IL",
            jurisdiction = "state",
            # pull the JSON data directly from the API
            pull_func = illinois_pull,
            # restructuring the data means pulling out the data portion of the json
            restruct_func = illinois_restruct,
            # Rename the columns to appropriate database names
            extract_func = illinois_extract){
            super$initialize(
                url = url, id = id, pull_func = pull_func, type = type,
                restruct_func = restruct_func, extract_func = extract_func,
                log = log, state = state, jurisdiction = jurisdiction)
        }
    )
)

if(sys.nframe() == 0){
    illinois <- illinois_scraper$new(log=TRUE)
    illinois$raw_data
    illinois$pull_raw()
    illinois$raw_data
    illinois$save_raw()
    illinois$restruct_raw()
    illinois$restruct_data
    illinois$extract_from_raw()
    illinois$extract_data
    illinois$validate_extract()
    illinois$save_extract()
}

