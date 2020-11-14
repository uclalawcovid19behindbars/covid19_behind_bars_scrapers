source("./R/generic_scraper.R")
source("./R/utilities.R")

florida_url_restruct <- function(x){
    fl <- x %>%
        rvest::html_node('table') %>%
        rvest::html_table()
    
    expected <- c(
        "Facility", "Security Quarantine", "Medical Quarantine", 
        "Medical Isolation", "Pending Tests", "Negative Tests", 
        "Positive Tests", "Positive Tests")
    received <- unname(unlist(fl[1,]))
    
    basic_check(received, expected)
    
    names(fl)[1] <- "Name"
    names(fl)[2] <- "Quar1"
    names(fl)[3] <- "Quar2"
    names(fl)[4] <- "Quar3"
    names(fl)[5] <- "Residents.Pending"
    names(fl)[6] <- "Residents.Negative"
    names(fl)[7] <- "Residents.Confirmed"
    names(fl)[8] <- "Staff.Confirmed"
        
    state_death <- x %>%
        rvest::html_nodes('.smallTable') %>%
        .[[3]] %>%
        rvest::html_table()
    
    if(!any(grepl("Inmate Deaths", names(state_death)))){
        warning(str_c(
            "Attempted to grab state deaths but column may not be correct."))
    }
    
    bind_rows(
        fl,
        tibble(
            Name = "State-Wide",
            Residents.Deaths = state_death[,2]))
}

florida_url_extract <- function(x){
    x %>%
        mutate(Name = str_remove_all(Name, "Operated by.*")) %>%
        filter(Name!= "Facility" & Name!= "Totals") %>%
        clean_scraped_df() %>%
        mutate(Residents.Quarantine = Quar1 + Quar2 + Quar3) %>%
        select(-Quar1, -Quar2, -Quar3) %>%
        as_tibble()
}

#' Scraper class for general Florida COVID data
#' 
#' @name florida_url_scraper
#' @description Florida has an html table for reporting results at the facilty
#' level which has been consistent but they have recently stopped reporting
#' facility level deaths. The death data is now reported weekly in a table
#' further down on the web page.
#' \describe{
#'   \item{Facility}{The faciilty name.}
#'   \item{Resident Security Quarantine}{}
#'   \item{Resident Medical Quarantine}{}
#'   \item{Resident Medical Isolation}{}
#'   \item{Resident Pending Tests}{}
#'   \item{Resident Negative Tests}{}
#'   \item{Resident Positive Tests}{}
#'   \item{Staff Positive Tests}{}
#' }

florida_url_scraper <- R6Class(
    "florida_url_scraper",
    inherit = generic_scraper,
    public = list(
        log = NULL,
        initialize = function(
            log,
            url = "http://www.dc.state.fl.us/comm/covid-19.html",
            id = "florida_url",
            type = "html",
            state = "FL",
            jurisdiction = "state",
            # pull the JSON data directly from the API
            pull_func = xml2::read_html,
            restruct_func = florida_url_restruct,
            # Rename the columns to appropriate database names
            extract_func = florida_url_extract){
            super$initialize(
                url = url, id = id, pull_func = pull_func, type = type,
                restruct_func = restruct_func, extract_func = extract_func,
                log = log, state = state, jurisdiction = jurisdiction)
        }
    )
)

if(sys.nframe() == 0){
    florida_url <- florida_url_scraper$new(log=FALSE)
    florida_url$raw_data
    florida_url$pull_raw()
    florida_url$raw_data
    florida_url$restruct_raw()
    florida_url$restruct_data
    florida_url$extract_from_raw()
    florida_url$extract_data
    florida_url$validate_extract()
    florida_url$save_extract()
}

