source("./R/generic_scraper.R")
source("./R/utilities.R")

ice_pull <- function(x){
    xml2::read_html(x)
}

ice_restruct <- function(x){
    
    raw_date <- x %>%
        rvest::html_nodes(".field-item.even.item-1") %>%
        rvest::html_nodes(".timestamp") %>%
        rvest::html_text() 
    
    update_date <- raw_date%>%
        str_extract("\\d+/\\d+/\\d+") %>%
        lubridate::mdy()
    
    if(as.numeric(Sys.Date() - update_date) > 7){
        warning("Data may be outdated, last update was marked as: ", raw_date)
    }
    
    list(
        totals = x %>%
            rvest::html_nodes("table") %>%
            .[[1]] %>%
            rvest::html_nodes("td") %>%
            sapply(function(z){
                
                out <- z %>% 
                    rvest::html_nodes("div") %>%
                    .[[2]] %>%
                    rvest::html_text() %>%
                    string_to_clean_numeric()
                
                names(out) <- z %>% 
                    rvest::html_node("div") %>%
                    rvest::html_text()
                
                out
            }),
    
        facility = x %>%
            rvest::html_nodes("table") %>%
            .[[2]] %>%
            rvest::html_table() %>%
            as_tibble())
        
}

ice_extract <- function(x){
    
    if(!str_detect(names(x$totals)[1], "(?i)population")){
        stop("Expected population data but this may have changed. Please inspect.")
    }

    if(!str_detect(names(x$totals)[3], "(?i)test")){
        stop("Expected testing data but this may have changed. Please inspect.")
    }
    
    name_compare <- c(
        Name = "Custody/AOR/Facility",
        `Residents.Active` = "Confirmedcases currently under isolation or monitoring",
        `Residents.Deaths` = "Detainee deaths3",
        `Residents.Confirmed` = "Total confirmed COVID-19 cases4"
    )
    
    check_names(x$facility, name_compare)

    out_df <- x$facility
    names(out_df) <- names(name_compare)
    
    out_df %>%
        filter(!str_detect(Name, "(?i)total")) %>%
        filter(!str_detect(Name, "(?i)field office")) %>%
        clean_scraped_df() %>%
        bind_rows(
            tibble(
                Name = "ICE Totals",
                Residents.Population = x$totals[1],
                Residents.Tested = x$totals[3]
            )
        )
    
}

#' Scraper class for ice COVID data
#' 
#' @name ice_scraper
#' @description This scraper pulls html data from the ice page which reports on
#' the variables listed below. Unlike all other scrapers their are total column
#' values that we want to keep which do not corresponds to a state but rather
#' ICE as a whole. In addition we have found that facility names frequently
#' change and require updates to the facility spellings sheet.
#' \describe{
#'   \item{Facility}{The facility name}
#'   \item{Confirmed cases currently under isolation}{Residents with active infections}
#'   \item{Detainee deaths}{Resident deaths}
#'   \item{Total confirmed COVID-19}{Residents cconfirmed cumulative}
#'   \item{Detained Population}{Current Resident Population}
#'   \item{Population Tested}{Tested}
#' }

ice_scraper <- R6Class(
    "ice_scraper",
    inherit = generic_scraper,
    public = list(
        log = NULL,
        initialize = function(
            log,
            url = "https://www.ice.gov/coronavirus",
            id = "ice",
            type = "html",
            state = "Federal",
            jurisdiction = "immigration",
            pull_func = ice_pull,
            restruct_func = ice_restruct,
            extract_func = ice_extract){
            super$initialize(
                url = url, id = id, pull_func = pull_func, type = type,
                restruct_func = restruct_func, extract_func = extract_func,
                log = log, state = state, jurisdiction = jurisdiction)
        }
    )
)

if(sys.nframe() == 0){
    ice <- ice_scraper$new(log=TRUE)
    ice$perma_save()
    ice$raw_data
    ice$pull_raw()
    ice$raw_data
    ice$save_raw()
    ice$restruct_raw()
    ice$restruct_data
    ice$extract_from_raw()
    ice$extract_data
    ice$validate_extract()
    ice$save_extract()
}
