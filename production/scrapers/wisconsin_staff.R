source("./R/generic_scraper.R")
source("./R/utilities.R")

wisconsin_staff_check_date <- function(x, date = Sys.Date()){
    z <- xml2::read_html(x)
    
    z %>%
        rvest::html_node(xpath = "//em[contains(text(),'Updated')]") %>%
        rvest::html_text() %>%
        str_remove_all("Updated|\\)|\\(") %>%
        lubridate::mdy() %>%
        error_on_date(date)
}

wisconsin_staff_pull <- function(x){
   xml2::read_html(x)
}

wisconsin_staff_restruct <- function(x){
    df_ <- x %>%
        rvest::html_nodes("table") %>%
        .[[2]] %>% 
        rvest::html_table() 
    
    out <- df_[,1:2]
    
    out %>% 
        janitor::row_to_names(row_number = 1)
}

wisconsin_staff_extract <- function(x){
    exp_names <- c(
        Name = "Division of Adult Institutions",
        Staff.Confirmed = "Staff Confirmed"
    )
    
    check_names(x, exp_names)
    df_ <- x
    names(df_) <- names(exp_names)
    
    df_ %>%
        # remove adult totals
        filter(!str_detect(Name, "(?i)division of adult")) %>%
        # remove totals except for corrections which only has a total
        filter(!str_detect(Name, "(?i)total") |
                   str_detect(Name, "(?i)division of community")) %>%
        filter(!str_detect(Staff.Confirmed, "(?i)staff")) %>%
        clean_scraped_df() %>%
        as_tibble()
}

#' Scraper class for general wisconsin_staff COVID data
#' 
#' @name wisconsin_staff_scraper
#' @description WI comes from a tableau table which is downloaded as a pdf.
#' Currently the downloading of this pdf is done on a private server run by
#' marquezn at law.ucla.edu because of RSelenium requirements. This should be
#' changed when a dedicated server is built. 
#' \describe{
#'   \item{Facility_Name}{The facility name.}
#'   \item{Positive tests}{Cumulative positive test count for residents.}
#'   \item{Negative tests}{Cumulative negative test count for residents.}
#'   \item{Total tests}{Cumulative tests administered for residents.}
#'   \item{Released Positive Cases}{Cumulative residents released while positive.}
#'   \item{Active Positive Cases}{Residents with active cases.}
#'   \item{Inactive Positive Cases}{Residents recovered but the wording changed 10/9 from recovered to inactive.}
#' }

wisconsin_staff_scraper <- R6Class(
    "wisconsin_staff_scraper",
    inherit = generic_scraper,
    public = list(
        log = NULL,
        initialize = function(
            log,
            url = 
                "https://doc.wi.gov/Pages/COVID19%28Coronavirus%29/EmployeeConfirmedCases/COVID19EmployeeConfirmedCases.aspx",
            id = "wisconsin_staff",
            type = "html",
            state = "WI",
            jurisdiction = "state",
            check_date = wisconsin_staff_check_date,
            # pull the JSON data directly from the API
            pull_func = wisconsin_staff_pull,
            # restructuring the data means pulling out the data portion of the json
            restruct_func = wisconsin_staff_restruct,
            # Rename the columns to appropriate database names
            extract_func = wisconsin_staff_extract){
            super$initialize(
                url = url, id = id, pull_func = pull_func, type = type,
                restruct_func = restruct_func, extract_func = extract_func,
                log = log, state = state, jurisdiction = jurisdiction,
                check_date = check_date)
        }
    )
)

if(sys.nframe() == 0){
    wisconsin_staff <- wisconsin_staff_scraper$new(log=TRUE)
    wisconsin_staff$run_check_date()
    wisconsin_staff$raw_data
    wisconsin_staff$pull_raw()
    wisconsin_staff$raw_data
    wisconsin_staff$save_raw()
    wisconsin_staff$restruct_raw()
    wisconsin_staff$restruct_data
    wisconsin_staff$extract_from_raw()
    wisconsin_staff$extract_data
    wisconsin_staff$validate_extract()
    wisconsin_staff$save_extract()
}

