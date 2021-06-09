source("./R/generic_scraper.R")
source("./R/utilities.R")

ohio_population_pull <- function(x, exp_date = Sys.Date()){
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
    
    # Get url for latest pdf 
    tbl <- tibble(link = link_, url = url_) %>% 
        filter(stringr::str_detect(url_, "Portals")) %>% 
        mutate(Date = lubridate::mdy(link))
    
    latest_date <- max(tbl$Date)
    error_on_date(latest_date, exp_date)
    
    stringr::str_c(
        "https://drc.ohio.gov", 
        tbl %>% 
            filter(Date == latest_date) %>% 
            pull(url)
    )
}

ohio_population_restruct <- function(x){
    list(pg1 = x %>% 
             tabulizer::extract_tables(pages = 1, area = list(c(129, 49, 753, 564))), 
         pg2 = x %>% 
            tabulizer::extract_tables(pages = 2, area = list(c(45, 50, 345, 565)))
    )
}

ohio_population_extract <- function(x){
    
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
    
    # Hard-coding 
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

#' Scraper class for Ohio population data 
#' 
#' @name ohio_population_scraper
#' @description Ohio posts weekly population reports in PDF form. The reports 
#' also include (not scraped) population by gender and security leve, 
#' \describe{
#'   \item{Institition}{Name}
#'   \item{Count}{Population}
#' }

ohio_population_scraper <- R6Class(
    "ohio_population_scraper",
    inherit = generic_scraper,
    public = list(
        log = NULL,
        initialize = function(
            log,
            url = "https://drc.ohio.gov/reports/population",
            id = "ohio_population",
            type = "pdf",
            state = "OH",
            jurisdiction = "state",
            pull_func = ohio_population_pull,
            restruct_func = ohio_population_restruct,
            extract_func = ohio_population_extract){
            super$initialize(
                url = url, id = id, pull_func = pull_func, type = type,
                restruct_func = restruct_func, extract_func = extract_func,
                log = log, state = state, jurisdiction  = jurisdiction)
        }
    )
)

if(sys.nframe() == 0){
    ohio_population <- ohio_population_scraper$new(log=TRUE)
    ohio_population$raw_data
    ohio_population$pull_raw()
    ohio_population$raw_data
    ohio_population$save_raw()
    ohio_population$restruct_raw()
    ohio_population$restruct_data
    ohio_population$extract_from_raw()
    ohio_population$extract_data
    ohio_population$validate_extract()
    ohio_population$save_extract()
}

