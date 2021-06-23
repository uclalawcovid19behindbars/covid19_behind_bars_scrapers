source("./R/generic_scraper.R")
source("./R/utilities.R")

new_jersey_check_date <- function(x, date = Sys.Date()){
    base_html <- xml2::read_html(x)
    date_txt <- rvest::html_nodes(base_html, 
                                  xpath="/html/body/div[3]/div[4]/div[1]/div/div[2]/p[1]") %>%
        rvest::html_text()
    
    date_txt %>%
        {.[str_detect(., "(?i)21")]} %>%
        str_extract("\\d{1,2}/\\d{1,2}/\\d{2,4}") %>%
        lubridate::mdy() %>%
        error_on_date(expected_date = date)
}

new_jersey_pull <- function(x){
    xml2::read_html(x)
}

new_jersey_restruct <- function(x){
    x %>%
        rvest::html_nodes("table") %>%
        lapply(rvest::html_table)
}

new_jersey_extract <- function(x){
    njtabs <- x[sapply(x, function(x) any(str_detect(names(x), "(?i)inmate")))]
    
    n4 <- c(
        "PRISONS AND ANCILLARY LOCATIONS", "EMPLOYEES",
        "INMATES", "INMATE DEATHS")
    
    n3 <- c("RESIDENTIAL COMMUNITY RELEASE PROGRAM", "INMATES", "INMATE DEATHS")
    
    bind_rows(lapply(njtabs, function(z){
        mini_tab <- z
        
        if(ncol(mini_tab) == 4){
            check_names(mini_tab, n4)
            
            names(mini_tab) <- c(
                "Name", "Staff.Confirmed", "Residents.Confirmed",
                "Residents.Deaths")
        }
        
        else if(ncol(mini_tab) == 3){
            check_names(mini_tab, n3)
            
            names(mini_tab) <- c(
                "Name", "Residents.Confirmed", "Residents.Deaths")
        }
        
        else{
            stop("Website structure has changed, examine code")
        }
        
        as_tibble(clean_scraped_df(mini_tab))})) %>%
        group_by(Name) %>%
        summarize_all(sum, na.rm = TRUE) %>%
        filter(Name != "Totals")
}

#' Scraper class for general new_jersey COVID data
#' 
#' @name new_jersey_scraper
#' @description NJ reports data in multiple tables depending on phases. Within
#' each table is the cumulative coiunt for that phase. We loop through all
#' tables and sum to get the sumn of all phases.
#' \describe{
#'   \item{Facility_Name}{The facility name}
#'   \item{Employees}{Staff.Confirmed}
#'   \item{Inmates}{Resident cumulative Confirmed}
#'   \item{Inmates deaths}{Resident cumulative deaths}
#' }

new_jersey_scraper <- R6Class(
    "new_jersey_scraper",
    inherit = generic_scraper,
    public = list(
        log = NULL,
        initialize = function(
            log,
            url = "https://www.state.nj.us/corrections/pages/COVID19Updates.shtml",
            id = "new_jersey",
            type = "html",
            state = "NJ",
            jurisdiction = "state",
            check_date = new_jersey_check_date,
            # pull the JSON data directly from the API
            pull_func = new_jersey_pull,
            # restructuring the data means pulling out the data portion of the json
            restruct_func = new_jersey_restruct,
            # Rename the columns to appropriate database names
            extract_func = new_jersey_extract){
            super$initialize(
                url = url, id = id, pull_func = pull_func, type = type,
                restruct_func = restruct_func, extract_func = extract_func,
                log = log, state = state, jurisdiction = jurisdiction,
                check_date = check_date)
        }
    )
)

if(sys.nframe() == 0){
    new_jersey <- new_jersey_scraper$new(log=TRUE)
    new_jersey$run_check_date()
    new_jersey$raw_data
    new_jersey$pull_raw()
    new_jersey$raw_data
    new_jersey$save_raw()
    new_jersey$restruct_raw()
    new_jersey$restruct_data
    new_jersey$extract_from_raw()
    new_jersey$extract_data
    new_jersey$validate_extract()
    new_jersey$save_extract()
}

