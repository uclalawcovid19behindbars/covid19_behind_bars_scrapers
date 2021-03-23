source("./R/generic_scraper.R")
source("./R/utilities.R")

georgia_psychiatric_pull <- function(x){
    xml2::read_html(x)
}


georgia_psychiatric_restruct <- function(x){
    
    table1 <- x %>% 
        rvest::html_nodes("table") %>%
        .[[1]] %>% 
        rvest::html_table(header= FALSE) %>% 
        slice(-1)
    
    colnames(table1) <- table1[1, ]
    
    table1_expected_names <- c("", "Individuals", "Staff", "Total", "Individuals", "Staff", "Total")
    check_names(table1, table1_expected_names)
    
    colnames(table1) <- c(
        "Name", "Residents.Confirmed", "Staff.Confirmed", "Drop.Total.1",
        "Residents.Recovered", "Staff.Recovered", "Drop.Total.2")
    
    table1 <- table1 %>%
        select(-starts_with("Drop")) %>%
        filter(Name != "")
    
    
    table2 <- x %>% 
        rvest::html_nodes("table") %>%
        .[[2]] %>% rvest::html_table(header= TRUE)
    
    table2_expected_names <- c(
        "DBHDD Facilities", "Individuals", "Individuals", "Individuals", "Staff", "Staff", "Staff", "Total")
    check_names(table2, table2_expected_names)
    
    colnames(table2) <- c(
        "Drop.Name", "Residents.Deaths", "Drop.ResDeathDuplicate.1", "Drop.ResDeathDuplicate.2", 
        "Staff.Deaths", "Drop.StaffDeathDuplicate.1", "Drop.StaffDeathDuplicate.2", "Drop.Total")
    
    table2 <- table2 %>%
        select(-starts_with("Drop")) 
    
    
    cbind(table1, table2)  
}


georgia_psychiatric_extract <- function(x){
    x %>% 
        filter(!str_detect(Name, "(?i)total")) %>%
        clean_scraped_df() 
}

#' Scraper class for general georgia_psychiatric COVID data
#' 
#' @name georgia_psychiatric_scraper
#' @description Cases, Recoveries and deaths for staff and residents are to be
#' extracted from two html tables. We can ignore the total columns and should
#' alwayd be sure to filter out rows that include the total so we dont double
#' count.
#' 
#' \describe{
#'   \item{DBHDD Facilities}{The facility name.}
#'   \item{Individuals tested positive}{Residents.Confirmed}
#'   \item{Staff tested positive}{Staff.Confirmed}
#'   \item{Individuals recovered}{Residents.Recovered}
#'   \item{Staff recovered}{Staff.Recovered}
#'   \item{Individuals deaths}{Residents.Deaths}
#'   \item{Staff deaths}{Staff.Deaths}
#' }

georgia_psychiatric_scraper <- R6Class(
    "georgia_psychiatric_scraper",
    inherit = generic_scraper,
    public = list(
        log = NULL,
        initialize = function(
            log,
            url = "https://dbhdd.georgia.gov/confirmed-covid-19-cases",
            id = "georgia_psychiatric",
            type = "html",
            state = "GA",
            jurisdiction = "psychiatric",
            # pull the JSON data directly from the API
            pull_func = georgia_psychiatric_pull,
            # restructuring the data means pulling out the data portion of the json
            restruct_func = georgia_psychiatric_restruct,
            # Rename the columns to appropriate database names
            extract_func = georgia_psychiatric_extract){
            super$initialize(
                url = url, id = id, pull_func = pull_func, type = type,
                restruct_func = restruct_func, extract_func = extract_func,
                log = log, state = state, jurisdiction = jurisdiction)
        }
    )
)

if(sys.nframe() == 0){
    georgia_psychiatric <- georgia_psychiatric_scraper$new(log=TRUE)
    georgia_psychiatric$perma_save()
    georgia_psychiatric$raw_data
    georgia_psychiatric$pull_raw()
    georgia_psychiatric$raw_data
    georgia_psychiatric$save_raw()
    georgia_psychiatric$restruct_raw()
    georgia_psychiatric$restruct_data
    georgia_psychiatric$extract_from_raw()
    georgia_psychiatric$extract_data
    georgia_psychiatric$validate_extract()
    georgia_psychiatric$save_extract()
}

