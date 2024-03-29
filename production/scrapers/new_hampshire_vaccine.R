source("./R/generic_scraper.R")
source("./R/utilities.R")

new_hampshire_vaccine_check_date <- function(x, date = Sys.Date()){
    base_html <- xml2::read_html(x)
    
    vaccine_section <- rvest::html_node(
        base_html,
        xpath = "//caption[contains(text(), 'Resident COVID-19 Vaccine Information')]/parent::table/parent::section")

    date_txt <- rvest::html_nodes(vaccine_section, "strong") %>%
        rvest::html_text() %>%
        {.[str_detect(., "(?i)table data as of")]} %>%
        lubridate::mdy() %>%

    return(error_on_date(expected_date = date, date_txt))
}

new_hampshire_vaccine_pull <- function(x){
    xml2::read_html(x)
}

new_hampshire_vaccine_restruct <- function(x){
    tab_set <- x %>%
        rvest::html_nodes(xpath="//table[@border=1]")
    
    captions <- tab_set %>%
        rvest::html_text() %>%
        clean_fac_col_txt()
    
    rez_vacc_idx <- which(
        stringr::str_detect(captions, "(?i)resident") & 
            stringr::str_detect(captions, "(?i)vaccine"))
    
    tab_set[[rez_vacc_idx]] %>%
        rvest::html_table(header = FALSE) %>%
        as_tibble()
}

new_hampshire_vaccine_extract <- function(x){
    if(ncol(x) != 5){
        stop("html is not as expected please inspect")
    }
    
    if(!any(str_detect(x$X3, "(?i)1 Dose"))){
        stop("html is not as expected please inspect")
    }
    
    if(!any(str_detect(x$X4, "(?i)Up to Date"))){
        stop("html is not as expected please inspect")
    }
    
    x %>%
        select(X1, X2, X3, X4) %>%
        rename(Name = X1, 
               Residents.Population = X2,
               Residents.Initiated = X3, 
               Residents.Completed = X4) %>% 
        filter(!str_detect(Name, "(?i)total")) %>%
        filter(!Name == "Facility") %>% 
        clean_scraped_df() 
}

#' Scraper class for general New Hampshire COVID data
#' 
#' @name new_hampshire_vaccine_scraper
#' @description NH has 3 tables of data and the third one is related to
#' vaccinations of residents. The table is broken down into phases which we
#' are not extracting and break down vaccines by facility and initiated and
#' completed.
#' \describe{
#'   \item{Facility_Name}{The facility name.}
#'   \item{1st Dose}{}
#'   \item{2nd Dose}{}
#'   \item{1 Dose Vaccine}{}
#' }

new_hampshire_vaccine_scraper <- R6Class(
    "new_hampshire_vaccine_scraper",
    inherit = generic_scraper,
    public = list(
        log = NULL,
        initialize = function(
            log,
            url = "https://www.covid19.nhdoc.nh.gov/covid-19-testing-information",
            id = "new_hampshire_vaccine",
            type = "html",
            state = "NH",
            jurisdiction = "state",
            check_date = new_hampshire_vaccine_check_date,
            pull_func = new_hampshire_vaccine_pull,
            restruct_func = new_hampshire_vaccine_restruct,
            extract_func = new_hampshire_vaccine_extract){
            super$initialize(
                url = url, id = id, pull_func = pull_func, type = type,
                restruct_func = restruct_func, extract_func = extract_func,
                log = log, state = state, jurisdiction = jurisdiction,
                check_date = check_date)
        }
    )
)

if(sys.nframe() == 0){
    new_hampshire_vaccine <- new_hampshire_vaccine_scraper$new(log=TRUE)
    new_hampshire_vaccine$run_check_date()
    new_hampshire_vaccine$raw_data
    new_hampshire_vaccine$pull_raw()
    new_hampshire_vaccine$raw_data
    new_hampshire_vaccine$save_raw()
    new_hampshire_vaccine$restruct_raw()
    new_hampshire_vaccine$restruct_data
    new_hampshire_vaccine$extract_from_raw()
    new_hampshire_vaccine$extract_data
    new_hampshire_vaccine$validate_extract()
    new_hampshire_vaccine$save_extract()
}

