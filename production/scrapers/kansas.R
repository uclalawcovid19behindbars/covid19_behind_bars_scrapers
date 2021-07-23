source("./R/generic_scraper.R")
source("./R/utilities.R")

kansas_date_check <- function(x, date = Sys.Date()){
    base_html <- xml2::read_html(x)
    
    base_html %>%
        rvest::html_nodes("p") %>%
        rvest::html_text() %>% 
        {.[str_detect(., "(?i)as of")]} %>% 
        str_extract("\\d{1,2}/\\d{1,2}/\\d{2,4}") %>% 
        min(na.rm = TRUE) %>% 
        lubridate::mdy() %>%
        error_on_date(date)
}

kansas_pull <- function(x){
    xml2::read_html(x)
}

kansas_restruct <- function(x){
    x %>%
        rvest::html_node("table") %>%
        rvest::html_table() %>%
        as_tibble()
}

kansas_extract <- function(x){
    df_ <- x
    
    exp_names <- c(
        Name = "Facility",
        Staff.Active = "CurrentStaff Cases",
        Residents.Active = "Current ResidentCases",
        Drop.Resident.At = "Current PositiveResidents Housedat thisFacility",
        Staff.Confirmed = "Cumulative Staff Cases",
        Residents.Confirmed = "Cumulative Resident Cases"
    )
    
    check_names(df_, exp_names)
    names(df_) <- names(exp_names)
    
    df_ %>%
        select(-starts_with("Drop")) %>%
        mutate(Name = clean_fac_col_txt(Name)) %>%
        clean_scraped_df() %>%
        filter(Name != "")
}

#' Scraper class for general kansas COVID data
#' 
#' @name kansas_scraper
#' @description This will be a description of kansas data and what the scraper
#' does
#' \describe{
#'   \item{Facility_Name}{The faciilty name.}
#' }

kansas_scraper <- R6Class(
    "kansas_scraper",
    inherit = generic_scraper,
    public = list(
        log = NULL,
        initialize = function(
            log,
            url = "https://www.doc.ks.gov/kdoc-coronavirus-updates/kdoc-covid-19-status",
            id = "kansas",
            type = "html",
            state = "KS",
            jurisdiction = "state",
            check_date = kansas_date_check,
            # pull the JSON data directly from the API
            pull_func = kansas_pull,
            # restructuring the data means pulling out the data portion of the json
            restruct_func = kansas_restruct,
            # Rename the columns to appropriate database names
            extract_func = kansas_extract){
            super$initialize(
                url = url, id = id, pull_func = pull_func, type = type,
                restruct_func = restruct_func, extract_func = extract_func,
                log = log, state = state, jurisdiction = jurisdiction,
                check_date = check_date)
        }
    )
)

if(sys.nframe() == 0){
    kansas <- kansas_scraper$new(log=TRUE)
    kansas$run_check_date()
    kansas$raw_data
    kansas$pull_raw()
    kansas$raw_data
    kansas$save_raw()
    kansas$restruct_raw()
    kansas$restruct_data
    kansas$extract_from_raw()
    kansas$extract_data
    kansas$validate_extract()
    kansas$save_extract()
}

