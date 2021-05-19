source("./R/generic_scraper.R")
source("./R/utilities.R")

historical_pa_pop_pull <- function(x, file, date = NULL){
    
    if(date > lubridate::ymd("2020-10-19")){
        stop("Historical scraper should not be run past 2020-10-19 to not overlap with regular scraper.")
    }
    
    file %>% 
        xml2::read_html()
}

historical_pa_pop_restruct <- function(x, date = NULL){
    
    if (date >= lubridate::ymd("2020-05-30")) {
        table_idx <- 1
    } else {
        table_idx <- 2
    }
    
    x %>%
        rvest::html_nodes("table") %>%
        .[[table_idx]] %>% 
        rvest::html_table(header = TRUE) %>%
        as_tibble() 
}

historical_pa_pop_extract <- function(x, date = NULL){
    
    df_ <- x
    
    if (ncol(df_) == 5) {
        exp_names <- c(
            Name = "INSTITUTION",
            Residents.Population = "TODAY'S POPULATION",
            Drop.Delta.Day = "INCREASE/ DECREASE FROM YESTERDAY",
            Drop.Delta.Week = "INCREASE/ DECREASE FROM LAST WEEK",
            Drop.Delta.Month = "INCREASE/ DECREASE FROM LAST MONTH"
        )
    } else if (ncol(df_) == 7) {
        exp_names <- c(
            Name = "INSTITUTION",
            Residents.Population = "TODAY'S POPULATION",
            Drop.Residents.Release = "REPRIEVE RELEASES",
            Drop.Pop.After.Release = "TODAY'S POPULATION AFTER REPRIEVE RELEASES",
            Drop.Delta.Day = "INCREASE/ DECREASE FROM YESTERDAY",
            Drop.Delta.Week = "INCREASE/ DECREASE FROM LAST WEEK",
            Drop.Delta.Month = "INCREASE/ DECREASE FROM LAST MONTH"
        )
    } else {
        stop("Number of columns does match expected.")
    }

    check_names(df_, exp_names)
    names(df_) <- names(exp_names)
    
    bad_names <- c(
        "INSTITUTION", "SCI TOTAL", "CCC/F East", "CCC/F Central",
        "CCC/F West", "CCC/F TOTAL")
    
    df_ %>%
        select(-starts_with("Drop")) %>%
        filter(!(Name %in% bad_names)) %>%
        clean_scraped_df()
}

#' Scraper class for Pennsylvania historical population data 
#' 
#' @name historical_pa_pop_scraper
#' @description This scraper uses the wayback archives to pull population data 
#' reported on Pennsylvania's COVID dashboard since April 2020. Pennsylvania's DOC
#' also posts and archives monthly population reports in PDF form 
#' https://www.cor.pa.gov/About%20Us/Statistics/Pages/Monthly-Population-Reports.aspx
#' but the wayback archives provide more coverage. 
#' 
#' \describe{
#'   \item{INSTITUTION}{The facility name.}
#'   \item{TODAY'S POPULATION}{Residnet population}
#'   \item{REPRIEVE RELEASES}{Residents released}
#'   \item{TODAY'S POPULATION AFTER REPRIEVE RELEASES}{New res population}
#'   \item{INCREASE/DECREASE FROM YESTERDAY}{Resident change from yesterday}
#'   \item{INCREASE/DECREASE FROM LAST WEEK}{Resident change from last week}
#'   \item{INCREASE/DECREASE FROM LAST MONTH}{Resident change from last month}
#' }

historical_pa_pop_scraper <- R6Class(
    "historical_pa_pop_scraper",
    inherit = generic_scraper,
    public = list(
        log = NULL,
        initialize = function(
            log,
            url = "https://www.cor.pa.gov/Pages/COVID-19.aspx",
            id = "historical_pa_pop",
            type = "html",
            state = "PA",
            jurisdiction = "state",
            # pull the JSON data directly from the API
            pull_func = historical_pa_pop_pull,
            # restructuring the data means pulling out the data portion of the json
            restruct_func = historical_pa_pop_restruct,
            # Rename the columns to appropriate database names
            extract_func = historical_pa_pop_extract){
            super$initialize(
                url = url, id = id, pull_func = pull_func, type = type,
                restruct_func = restruct_func, extract_func = extract_func,
                log = log, state = state, jurisdiction = jurisdiction)
        }
    )
)

if(sys.nframe() == 0){
    historical_pa_pop <- historical_pennsylvania_pop_scraper$new(log=TRUE)
    historical_pa_pop$reset_date("DATE")
    historical_pa_pop$raw_data
    historical_pa_pop$pull_raw(file, .dated_pull = TRUE)
    historical_pa_pop$raw_data
    historical_pa_pop$save_raw()
    historical_pa_pop$restruct_raw()
    historical_pa_pop$restruct_data
    historical_pa_pop$extract_from_raw()
    historical_pa_pop$extract_data
    historical_pa_pop$validate_extract()
    historical_pa_pop$save_extract()
}
