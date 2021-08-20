source("./R/generic_scraper.R")
source("./R/utilities.R")

historical_utah_statewide_pull <- function(x){
    page_html <- xml2::read_html(x)
    return(page_html)
}

historical_utah_statewide_restruct <- function(x, date = NULL){
    staff_confirmed <- rvest::html_nodes(x, 
                                         xpath = "/html/body/section/article/div[2]/div[3]/p[22]/span/em/strong/span") %>%
        rvest::html_text(.) 
    if(!str_detect(staff_confirmed, "(?i)total confirmed staff cases")){
        warning("Field does mot match expected text")
    }
    
    staff_recovered <- rvest::html_nodes(x,
                                         xpath = "/html/body/section/article/div[2]/div[3]/p[23]/strong/span") %>%
        rvest::html_text()
    if(!str_detect(staff_recovered, "(?i)total recovered staff cases")){
        warning("Field does mot match expected text")
    }
    restruct_out <- list(staff_confirmed = staff_confirmed,
                         staff_recovered = staff_recovered)
    return(restruct_out)
}

historical_utah_statewide_extract <- function(x, date){
    statewide_staff_confirmed <- x$staff_confirmed %>%
        str_split("(?i)Total Confirmed Staff Cases: ") %>%
        unlist() %>%
        .[2] %>%
        as.numeric() 
    statewide_staff_recovered <- x$staff_recovered %>%
        str_split("(?i)Total Recovered Staff Cases: ") %>%
        unlist() %>%
        .[2] %>%
        as.numeric() 
    
    staff_rows <- tibble(Name = "STATEWIDE",
                         Staff.Confirmed = statewide_staff_confirmed,
                         Staff.Recovered = statewide_staff_recovered)
    
    extract_out <- staff_rows %>%
        clean_scraped_df() %>%
        as_tibble()
    
    return(extract_out)
}

#' Scraper class for general Utah COVID data
#' 
#' @name historical_utah_statewide_scraper
#' @description The staff numbers are for the whole system
#' \describe{
#'   \item{Location}{The facility name.}
#'   \item{Staff Confirmed}{Staff cumulative cases}
#'   \item{Staff Recovered}{Staff recovered}
#' }

historical_utah_statewide_scraper <- R6Class(
    "historical_utah_statewide_scraper",
    inherit = generic_scraper,
    public = list(
        log = NULL,
        initialize = function(
            log,
            url = "https://corrections.utah.gov/index.php/home/alerts-2/1237-udc-coronavirus-updates",
            id = "historical_utah_statewide",
            type = "html",
            state = "UT",
            jurisdiction = "state",
            check_date = NULL,
            pull_func = historical_utah_statewide_pull,
            restruct_func = historical_utah_statewide_restruct,
            extract_func = historical_utah_statewide_extract){
            super$initialize(
                url = url, id = id, pull_func = pull_func, type = type,
                restruct_func = restruct_func, extract_func = extract_func,
                log = log, state = state, jurisdiction = jurisdiction)
        }
    )
)

if(sys.nframe() == 0){
    historical_utah_statewide <- historical_utah_statewide_scraper$new(log=TRUE)
    historical_utah_statewide$reset_date("DATE")
    historical_utah_statewide$pull_raw(date = scraper$date, file = NULL, .dated_pull = TRUE)
    historical_utah_statewide$raw_data
    historical_utah_statewide$save_raw()
    historical_utah_statewide$restruct_raw(date = historical_utah_statewide$date)
    historical_utah_statewide$restruct_data
    historical_utah_statewide$extract_from_raw(date = historical_utah_statewide$date)
    historical_utah_statewide$extract_data
    historical_utah_statewide$validate_extract()
    historical_utah_statewide$save_extract()
}