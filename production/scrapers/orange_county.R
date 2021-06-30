source("./R/generic_scraper.R")
source("./R/utilities.R")

orange_county_check_date <- function(x, date = Sys.Date()){
    base_html <- xml2::read_html(x)
    date_txt <- rvest::html_nodes(base_html, xpath = "//*[@id=\"block-countyoc-content\"]/article/div/div/div/div/h3[1]") %>%
        rvest::html_text()
    
    date_txt %>%
        {.[str_detect(., "(?i)21")]} %>%
        str_split(., "(?i)Updated ") %>%
        unlist() %>%
        .[2] %>%
        lubridate::mdy() %>%
        error_on_date(expected_date = date)
}


orange_county_pull <- function(x){
    xml2::read_html(x)
}

orange_county_restruct <- function(x){
    x %>%
        rvest::html_node("table") %>%
        rvest::html_table() %>%
        as_tibble() %>%
        mutate(across(.fns =  function(x) str_remove(x, "\\([^)]*\\)")))
}

orange_county_extract <- function(x){
    exp_names <- c(
        Residents.Active = "# Current COVID-19 Positive",
        Residents.Recovered = "# Inmates Recovered",
        Residents.Confirmed = "# Inmates Positive",
        Residents.Negative = "# Inmates Negative",
        Residents.Pending = "# Results Pending",
        Residents.Tested = "# Unique Inmates Tested"
    )
    
    check_names(x, exp_names)
    df_ <- x
    names(df_) <- names(exp_names)
    
    df_ %>%
        select(-starts_with("Drop")) %>%
        mutate(Name = "Orange County Jails") %>%
        clean_scraped_df()
}

#' Scraper class for general orange_county COVID data
#' 
#' @name orange_county_scraper
#' @description OC data comes from an html table which reports data on
#' numerous statistics. Quarantine data for facilities is also provided.
#' \describe{
#'   \item{# Current COVID-19 Positive}{}
#'   \item{# Inmates Recovered}{}
#'   \item{# Inmates Positive}{}
#'   \item{# Inmates Negative}{}
#'   \item{# Results Pending}{}
#'   \item{# Tests Administered}{}
#'   \item{Quarantine/Isolation}{}
#' }

orange_county_scraper <- R6Class(
    "orange_county_scraper",
    inherit = generic_scraper,
    public = list(
        log = NULL,
        initialize = function(
            log,
            url = "https://ocsheriff.gov/about-ocsd/covid-19/covid-19-oc-jails",
            id = "orange_county",
            type = "html",
            state = "CA",
            jurisdiction = "county",
            check_date = orange_county_check_date,
            # pull the JSON data directly from the API
            pull_func = orange_county_pull,
            # restructuring the data means pulling out the data portion of the json
            restruct_func = orange_county_restruct,
            # Rename the columns to appropriate database names
            extract_func = orange_county_extract){
            super$initialize(
                url = url, id = id, pull_func = pull_func, type = type,
                restruct_func = restruct_func, extract_func = extract_func,
                log = log, state = state, jurisdiction = jurisdiction,
                check_date = check_date)
        }
    )
)

if(sys.nframe() == 0){
    orange_county <- orange_county_scraper$new(log=TRUE)
    orange_county$run_check_date()
    orange_county$raw_data
    orange_county$pull_raw()
    orange_county$raw_data
    orange_county$save_raw()
    orange_county$restruct_raw()
    orange_county$restruct_data
    orange_county$extract_from_raw()
    orange_county$extract_data
    orange_county$validate_extract()
    orange_county$save_extract()
}

