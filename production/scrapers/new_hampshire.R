source("./R/generic_scraper.R")
source("./R/utilities.R")

new_hampshire_pull <- function(x){
    xml2::read_html(x)
}

new_hampshire_restruct <- function(x){
    tab_set <- x %>%
        rvest::html_nodes(xpath="//table[@border=1]")
    
    captions <- tab_set %>%
        rvest::html_text() %>%
        clean_fac_col_txt()
    
    staff_idx <- which(stringr::str_detect(captions, "(?i)staff testing"))
    rez_idx <- which(stringr::str_detect(captions, "(?i)residents testing"))
    
    list(
        staff = tab_set[[staff_idx]] %>%
            rvest::html_table(header = TRUE) %>%
            as_tibble(),
        resident = tab_set[[rez_idx]] %>%
            rvest::html_table(header = TRUE) %>%
            as_tibble()
    )
}

new_hampshire_extract <- function(x){
    staff_df <- x$staff
    rez_df <- x$resident
    
    names(staff_df) <- clean_fac_col_txt(names(staff_df))
    names(rez_df) <- clean_fac_col_txt(names(rez_df))
    
    rez_exp <- c(
        Name = "Facility",
        Residents.Tested = "Residents COVID-19 Tests Administered",
        Residents.Active = "Active Residents Positive",
        Residents.Confirmed =
            "Total Residents who have tested positive since March 2020",
        Residents.Deaths = "COVID-19 Deaths"
        )
    
    staff_exp <- c(
        Name = "Worksite",
        Staff.Confirmed = "Staff Positive - Total",
        Drop.Staff.Active = "Staff Positive - Active")
    
    check_names(staff_df, staff_exp)
    check_names(rez_df, rez_exp)
    
    names(staff_df) <- names(staff_exp)
    names(rez_df) <- names(rez_exp)
    
    full_join(
        staff_df %>%
            mutate(Name = clean_fac_col_txt(Name)) %>%
            mutate(Name = ifelse(Name == "SPU / RTU", "SPU & RTU", Name)) %>%
            filter(!str_detect(Name, "(?i)total|current|other|request")),
        rez_df %>%
            mutate(Name = clean_fac_col_txt(Name)) %>%
            mutate(Name = ifelse(Name == "SPU / RTU", "SPU & RTU", Name)) %>%
            filter(!str_detect(Name, "(?i)total|current|other|request")),
        by = "Name") %>%
        select(-starts_with("Drop")) %>%
        clean_scraped_df()
}

#' Scraper class for general New Hampshire COVID data
#' 
#' @name new_hampshire_scraper
#' @description NH has two tables one for residents and one for staff. The info
#' provided is minimal but fairly easy to scrape. Both number tested and 
#' confirmed for staff and residents appear to be individuals confirmed
#' and tested rather than tests administered and tests that were positive.
#' \describe{
#'   \item{Name}{The facility name.}
#'   \item{Num Staff Positive}{The staff number testing positive}
#'   \item{Num Residents tested}{Not the number of tests administered}
#'   \item{Num Residents Positive}{Number of residents confirmed}
#'   \item{NHDOC Staff}{State wide staff population}
#' }

new_hampshire_scraper <- R6Class(
    "new_hampshire_scraper",
    inherit = generic_scraper,
    public = list(
        log = NULL,
        initialize = function(
            log,
            url = "https://www.covid19.nhdoc.nh.gov/covid-19-testing-information",
            id = "new_hampshire",
            type = "html",
            state = "NH",
            jurisdiction = "state",
            # pull the JSON data directly from the API
            pull_func = new_hampshire_pull,
            # restructuring the data means pulling out the data portion of the json
            restruct_func = new_hampshire_restruct,
            # Rename the columns to appropriate database names
            extract_func = new_hampshire_extract){
            super$initialize(
                url = url, id = id, pull_func = pull_func, type = type,
                restruct_func = restruct_func, extract_func = extract_func,
                log = log, state = state, jurisdiction = jurisdiction)
        }
    )
)

if(sys.nframe() == 0){
    new_hampshire <- new_hampshire_scraper$new(log=TRUE)
    new_hampshire$raw_data
    new_hampshire$pull_raw()
    new_hampshire$raw_data
    new_hampshire$save_raw()
    new_hampshire$restruct_raw()
    new_hampshire$restruct_data
    new_hampshire$extract_from_raw()
    new_hampshire$extract_data
    new_hampshire$validate_extract()
    new_hampshire$save_extract()
}

