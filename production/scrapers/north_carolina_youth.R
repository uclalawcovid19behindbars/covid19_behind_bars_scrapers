source("./R/generic_scraper.R")
source("./R/utilities.R")

north_carolina_youth_pull <- function(x){
    xml2::read_html(x)
}

north_carolina_youth_restruct <- function(x){
    x %>%
        rvest::html_nodes('table') %>%
        .[[1]] %>%
        rvest::html_table()
}

north_carolina_youth_extract <- function(x){
    check_names(x,
                c("",
                  "Cumulative Tests Performed",
                  "Positive Tests",
                  "Presumed Recovered \n\t\t\t\tPer CDC/DHHS guidelines"
                  ))
    
    clean <- x %>%
        select(Name = "", 
               Residents.Tadmin = `Cumulative Tests Performed`,
               Residents.Confirmed = `Positive Tests`, # not sure what to do with this one
        ) %>%
        filter(Name != "TOTAL") %>% 
        mutate(Name = str_c(toupper(Name), " YOUTH")) 
    
    out <- clean %>%
        clean_scraped_df() %>%
        as_tibble()
    
    return(out)
}

#' Scraper class for general North Carolina Youth COVID data
#' 
#' @name north_carolina_youth_scraper
#' @description NC data self contained within html table. 
#' \describe{
#'   \item{Name}{The facilty name}
#'   \item{Residents.Tadmin}{Number of tests administered to youth}
#'   \item{Residents.Confirmed}{Number of total positive tests at a facility}
#'   \item{Residents.Recovered}{Youth Recovered}
#' }

north_carolina_youth_scraper <- R6Class(
    "north_carolina_youth_scraper",
    inherit = generic_scraper,
    public = list(
        log = NULL,
        initialize = function(
            log,
            url = "https://www.ncdps.gov/our-organization/juvenile-justice/juvenile-justice-info-covid-19#data",
            id = "north_carolina_youth",
            type = "html",
            state = "NC",
            jurisdiction = "state",
            pull_func = north_carolina_youth_pull,
            restruct_func = north_carolina_youth_restruct,
            extract_func = north_carolina_youth_extract){
            super$initialize(
                url = url, id = id, pull_func = pull_func, type = type,
                restruct_func = restruct_func, extract_func = extract_func,
                log = log, state = state, jurisdiction = jurisdiction)
        }
    )
)

if(sys.nframe() == 0){
    north_carolina_youth <- north_carolina_youth_scraper$new(log=TRUE)
    north_carolina_youth$raw_data
    north_carolina_youth$pull_raw()
    north_carolina_youth$raw_data
    north_carolina_youth$save_raw()
    north_carolina_youth$restruct_raw()
    north_carolina_youth$restruct_data
    north_carolina_youth$extract_from_raw()
    north_carolina_youth$extract_data
    north_carolina_youth$validate_extract()
    north_carolina_youth$save_extract()
}

