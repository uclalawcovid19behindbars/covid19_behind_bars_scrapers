source("./R/generic_scraper.R")
source("./R/utilities.R")

illinois_psychiatric_pull <- function(x){
    xml2::read_html(x)
    
}

illinois_psychiatric_restruct <- function(x){
    
    table1 <- x %>%
        rvest::html_nodes("table") %>%
        .[[3]] %>%
        rvest::html_table(fill = TRUE)
    
    table1_expected_names <- c("Facility (City - County)", "Total Staff at Facility", 
                               "Cumulative Confirmed Positive\r\nCOVID-19\r\nStaff",
                               "Staff Returned to Work", 
                               "Total Patients at\r\nFacility", 
                               "Cumulative Confirmed Positive\r\nCOVID-19 Patients",
                               "Recovered Patients")
    
    check_names(table1, table1_expected_names)
    
    colnames(table1) <- c("Name", "Drop.TotalStaff", "Staff.Confirmed", 
                          "Staff.Recovered", "Drop.TotalResidents", 
                          "Residents.Confirmed", "Residents.Recovered")
    
    table1 <- table1 %>%
        select(-starts_with("Drop"))
    
    
    table2 <- x %>%
        rvest::html_nodes("table") %>%
        .[[10]] %>%
        rvest::html_table(fill = TRUE)
    
    table2_expected_names <- c("Facility (City - County)", "Total Staff at Location",
                               "Staff Fatalities", "Total Patients at Facility",
                               "Patient Fatalities")
    
    check_names(table2, table2_expected_names)
    
    colnames(table2) <- c("Name", "Drop.TotalStaff", "Staff.Deaths", 
                          "Drop.TotalResidents", "Residents.Deaths")
    
    table2 <- table2 %>%
        select(-starts_with("Drop")) 
    
    
    merge(table1, table2, all = TRUE)
}

illinois_psychiatric_extract <- function(x){
    x 
}

#' Scraper class for general illinois_psychiatric COVID data
#' 
#' @name illinois_psychiatric_scraper
#' @description There are multiple html tables on this page however we are
#' interested in the table titled State Operated Psychiatric Hospitals. Row
#' names here indicate state psychiatric facilities and we want the columns
#' below
#' \describe{
#'   \item{Facility}{The facility name.}
#'   \item{Cumulative Confirmed Staff}{Staff.Confirmed}
#'   \item{Total patients at facility}{Residents.Population}
#'   \item{Cumulative Confirmed Patients}{Residents.Confirmed}
#'   \item{Recovered patients}{Residents.Recovered}
#' }

illinois_psychiatric_scraper <- R6Class(
    "illinois_psychiatric_scraper",
    inherit = generic_scraper,
    public = list(
        log = NULL,
        initialize = function(
            log,
            url = "https://www.dhs.state.il.us/page.aspx?item=123651",
            id = "illinois_psychiatric",
            type = "html",
            state = "IL",
            jurisdiction = "psychiatric",
            check_date = NULL,
            pull_func = illinois_psychiatric_pull,
            restruct_func = illinois_psychiatric_restruct,
            extract_func = illinois_psychiatric_extract){
            super$initialize(
                url = url, id = id, pull_func = pull_func, type = type,
                restruct_func = restruct_func, extract_func = extract_func,
                log = log, state = state, jurisdiction = jurisdiction,
                check_date = check_date)
        }
    )
)

if(sys.nframe() == 0){
    illinois_psychiatric <- illinois_psychiatric_scraper$new(log=TRUE)
    illinois_psychiatric$run_check_date()
    illinois_psychiatric$perma_save()
    illinois_psychiatric$raw_data
    illinois_psychiatric$pull_raw()
    illinois_psychiatric$raw_data
    illinois_psychiatric$save_raw()
    illinois_psychiatric$restruct_raw()
    illinois_psychiatric$restruct_data
    illinois_psychiatric$extract_from_raw()
    illinois_psychiatric$extract_data
    illinois_psychiatric$validate_extract()
    illinois_psychiatric$save_extract()
}

