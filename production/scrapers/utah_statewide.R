source("./R/generic_scraper.R")
source("./R/utilities.R")

utah_statewide_check_date <- function(x, date = Sys.Date()){
    z <- xml2::read_html(x)
    
    z %>%
        rvest::html_nodes("p") %>% 
        rvest::html_text() %>% 
        {.[str_detect(., "(?i)updated")]} %>% 
        first() %>% 
        str_split("updated") %>% 
        unlist() %>% 
        {.[str_detect(., "21")]} %>% 
        lubridate::mdy() %>%
        error_on_date(date)
}

utah_statewide_pull <- function(x){
    page_html <- xml2::read_html(x)
    return(page_html)
}

utah_statewide_restruct <- function(x){
    staff_confirmed <- x %>%
        rvest::html_nodes("p") %>%
        rvest::html_text() %>% 
        {.[which(str_detect(., "(?i)total confirmed staff cases"))]} 
    
    if(!str_detect(staff_confirmed, "(?i)total confirmed staff cases")){
        warning("Field does mot match expected text (staff confirmed)")
    }
    staff_recovered <- x %>%
        rvest::html_nodes("p") %>%
        rvest::html_text() %>% 
        {.[which(str_detect(., "(?i)total recovered staff cases"))]} 
    
    # if(!str_detect(staff_recovered, "(?i)total recovered staff cases")){
    #     warning("Field does mot match expected text (staff recovered)")
    # }
    restruct_out <- list(staff_confirmed = staff_confirmed,
                         staff_recovered = staff_recovered)
    return(restruct_out)
}

utah_statewide_extract <- function(x){
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
    statewide_staff_recovered <- ifelse(length(statewide_staff_recovered) == 0,
                                        NA_integer_, statewide_staff_recovered)
    
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
#' @name utah_statewide_scraper
#' @description The staff numbers are for the whole system
#' \describe{
#'   \item{Location}{The facility name.}
#'   \item{Staff Confirmed}{Staff cumulative cases}
#'   \item{Staff Recovered}{Staff recovered}
#' }

utah_statewide_scraper <- R6Class(
    "utah_statewide_scraper",
    inherit = generic_scraper,
    public = list(
        log = NULL,
        initialize = function(
            log,
            url = "https://corrections.utah.gov/index.php/home/alerts-2/1237-udc-coronavirus-updates",
            id = "utah_statewide",
            type = "html",
            state = "UT",
            jurisdiction = "state",
            check_date = utah_statewide_check_date,
            # pull the JSON data directly from the API
            pull_func = utah_statewide_pull,
            # restructuring the data means pulling out the data portion of the json
            restruct_func = utah_statewide_restruct,
            # Rename the columns to appropriate database names
            extract_func = utah_statewide_extract){
            super$initialize(
                url = url, id = id, pull_func = pull_func, type = type,
                restruct_func = restruct_func, extract_func = extract_func,
                log = log, state = state, jurisdiction = jurisdiction,
                check_date = check_date)
        }
    )
)

if(sys.nframe() == 0){
    utah_statewide <- utah_statewide_scraper$new(log=TRUE)
    utah_statewide$run_check_date()
    utah_statewide$pull_raw()
    utah_statewide$raw_data
    utah_statewide$save_raw()
    utah_statewide$restruct_raw()
    utah_statewide$restruct_data
    utah_statewide$extract_from_raw()
    utah_statewide$extract_data
    utah_statewide$validate_extract()
    utah_statewide$save_extract()
}