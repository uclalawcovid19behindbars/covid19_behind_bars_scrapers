source("./R/generic_scraper.R")
source("./R/utilities.R")

michigan_vaccine_date_check <- function(x, date = Sys.Date()){
    base_html <- rvest::read_html(x)
    
    base_html %>% 
        rvest::html_nodes("caption") %>% 
        rvest::html_text() %>% 
        {.[str_detect(., "(?i)correction")]} %>% 
        stringr::str_split("(?i)through") %>% 
        unlist() %>% 
        {.[str_detect(., "21")]} %>% 
        lubridate::mdy() %>% 
        error_on_date(date)
}

michigan_vaccine_pull <- function(x){
    xml2::read_html(x)
}

michigan_vaccine_restruct <- function(x){
    captions <- x %>% 
        rvest::html_nodes("table") %>% 
        rvest::html_nodes("caption") %>%
        rvest::html_text() 
    
    prison_idx <- which(str_detect(captions, "(?i)detained"))
    
    x %>% 
        rvest::html_nodes("table") %>% 
        rvest::html_table() %>% 
        .[[prison_idx]] 
}

michigan_vaccine_extract <- function(x){
    
    exp_names <- c(
        Name = "name", 
        Residents.Population = "total_population",
        Refusals.Drop = "number_refusals",
        First.Drop = "cumulative_1st_doses", 
        Second.Drop = "cumulative_2nd_doses", 
        Single.Drop = "single_dose_series_administered", 
        Age65.Drop = "age_group_65", 
        Age50.Drop = "age_group_50_64", 
        Age16.Drop = "age_group_16_49", 
        Vulnerable.Drop = "number_of_doses_given_to_medically_vulnerable"
    )
    
    df_ <- janitor::clean_names(x)
    check_names(df_, exp_names, detect = TRUE)
    names(df_) <- names(exp_names)
    
    df_ %>% 
        mutate(Name = clean_fac_col_txt(Name, to_upper = T)) %>% 
        mutate(across(where(is.numeric), ~ ifelse(is.na(.), 0, .))) %>%
        mutate(Residents.Initiated = First.Drop + Single.Drop, 
               Residents.Completed = Second.Drop + Single.Drop, 
               Residents.Vadmin = First.Drop + Second.Drop + Single.Drop) %>% 
        select(-ends_with("Drop")) %>% 
        filter(!str_detect(Name, "(?i)total")) %>% 
        clean_scraped_df()
}

#' Scraper class for Michigan vaccine data
#' 
#' @name michigan_vaccine_scraper
#' @description Michigan reports vaccine data for correctional facilities on the
#' state's DHS website. 
#' \describe{
#'   \item{Name}{}
#'   \item{Total population}{} 
#'   \item{# refusals}{}
#'   \item{Cumulative 1st doses}{}
#'   \item{Cumulative 2nd doses}{}
#'   \item{Second dose series administered}{}
#'   \item{Age group 65+}{}
#'   \item{Age group 50-64}{}
#'   \item{Age group 16-49}{}
#'   \item{# of doses given to medically vulnerable}{}
#' }

michigan_vaccine_scraper <- R6Class(
    "michigan_vaccine_scraper",
    inherit = generic_scraper,
    public = list(
        log = NULL,
        initialize = function(
            log,
            url = "https://www.michigan.gov/coronavirus/0,9753,7-406-98178_103214-547150--,00.html",
            id = "michigan_vaccine",
            type = "html",
            state = "MI",
            jurisdiction = "state",
            check_date = michigan_vaccine_date_check,
            # pull the JSON data directly from the API
            pull_func = michigan_vaccine_pull,
            # restructuring the data means pulling out the data portion of the json
            restruct_func = michigan_vaccine_restruct,
            # Rename the columns to appropriate database names
            extract_func = michigan_vaccine_extract){
            super$initialize(
                url = url, id = id, pull_func = pull_func, type = type,
                restruct_func = restruct_func, extract_func = extract_func,
                log = log, state = state, jurisdiction = jurisdiction,
                 check_date = check_date)
        }
    )
)

if(sys.nframe() == 0){
    michigan_vaccine <- michigan_vaccine_scraper$new(log=TRUE)
    michigan_vaccine$run_check_date()
    michigan_vaccine$raw_data
    michigan_vaccine$pull_raw()
    michigan_vaccine$raw_data
    michigan_vaccine$save_raw()
    michigan_vaccine$restruct_raw()
    michigan_vaccine$restruct_data
    michigan_vaccine$extract_from_raw()
    michigan_vaccine$extract_data
    michigan_vaccine$validate_extract()
    michigan_vaccine$save_extract()
}

