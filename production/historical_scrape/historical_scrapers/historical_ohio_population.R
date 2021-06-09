source("./R/generic_scraper.R")
source("./R/utilities.R")

historical_oh_pop_pull <- function(x, date, file = NULL){
    
    date <- lubridate::ymd(date)
    
    if(date > lubridate::ymd("2021-06-09")){
        stop("Historical scraper should not be run past 2021-06-09 to not overlap with regular scraper.")
    }
    
    # Extract all urls 
    url_ <- x %>% 
        xml2::read_html() %>%
        rvest::html_nodes("a") %>%
        rvest::html_attr("href")
    
    # Extract all link texts
    link_ <- x %>%
        xml2::read_html() %>%
        rvest::html_nodes("a") %>%
        rvest::html_text()
    
    # Get url for date 
    tibble(link = link_, url = url_) %>% 
        filter(stringr::str_detect(url_, "Portals")) %>% 
        mutate(Date = lubridate::mdy(link)) %>% 
        mutate(url = str_c("https://drc.ohio.gov", url)) %>% 
        # Manually fix bad dates 
        mutate(Date = case_when(Date == "0202-04-15" ~ as.Date("2021-04-15"), 
                                Date == "0202-04-08" ~ as.Date("2021-04-08"), 
                                TRUE ~ Date)) %>% 
        filter(Date == date) %>% 
        pull(url)
}

historical_oh_pop_restruct <- function(x, date = NULL){
    list(pg1 = x %>% 
             tabulizer::extract_tables(pages = 1, area = list(c(129, 49, 753, 564))), 
         pg2 = x %>% 
             tabulizer::extract_tables(pages = 2, area = list(c(45, 50, 345, 565)))
    )
}

historical_oh_pop_extract <- function(x, date = NULL){
    
    exp <- c("", "Institution", "Count", "", "Institution", "Count") 
    if (!all(exp == x$pg1[[1]][1,])){
        warning(
            "Column names not as expected. See if raw file structure changed." 
        )
    }
    
    if (!str_detect(x$pg2[[2]][nrow(x$pg2[[2]]),4], "(?i)total population")){
        warning(
            "Total column on second page not as expected. See if raw file structure changed." 
        )
    }
    
    total <- x$pg2[[2]][nrow(x$pg2[[2]]),5] %>% string_to_clean_numeric()
    
    out <- tibble(
        Name = c(
            x$pg1[[1]][,1], 
            x$pg1[[1]][,4], 
            x$pg2[[2]][,1], 
            x$pg2[[2]][,3]), 
        Residents.Population = c(
            x$pg1[[1]][,3], 
            x$pg1[[1]][,6], 
            x$pg2[[2]][,2], 
            x$pg2[[2]][,5])
    ) %>% 
        filter(!str_detect(Name, "(?i)total")) %>% 
        filter(!str_detect(Residents.Population, "(?i)count")) %>% 
        filter(Name != "") %>% 
        clean_scraped_df() 
    
    if (sum_na_rm(out$Residents.Population) != total) {
        warning(
            "Total doesn't match sum of facilities. See if raw file structure changed." 
        )
    }
    
    out
}

#' Scraper class for historical Ohio population data 
#' 
#' @name historical_oh_pop_scraper
#' @description Ohio posts weekly population reports in PDF form. The reports 
#' also include (not scraped) population by gender and security level.  
#' \describe{
#'   \item{Institition}{Name}
#'   \item{Count}{Population}
#' }

historical_oh_pop_scraper <- R6Class(
    "historical_ohio_pop_scraper",
    inherit = generic_scraper,
    public = list(
        log = NULL,
        initialize = function(
            log,
            url = "https://drc.ohio.gov/reports/population",
            id = "historical_oh_pop",
            type = "pdf",
            state = "OH",
            jurisdiction = "state",
            pull_func = historical_oh_pop_pull,
            restruct_func = historical_oh_pop_restruct,
            extract_func = historical_oh_pop_extract){
            super$initialize(
                url = url, id = id, pull_func = pull_func, type = type,
                restruct_func = restruct_func, extract_func = extract_func,
                log = log, state = state, jurisdiction  = jurisdiction)
        }
    )
)

if(sys.nframe() == 0){
    historical_oh_pop <- historical_oh_pop_scraper$new(log=TRUE)
    historical_oh_pop$reset_date("DATE")
    historical_oh_pop$raw_data
    historical_oh_pop$pull_raw(date = historical_oh_pop$date, .dated_pull = TRUE)
    historical_oh_pop$raw_data
    historical_oh_pop$save_raw()
    historical_oh_pop$restruct_raw()
    historical_oh_pop$restruct_data
    historical_oh_pop$extract_from_raw()
    historical_oh_pop$extract_data
    historical_oh_pop$validate_extract()
    historical_oh_pop$save_extract()
}