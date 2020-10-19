source("./R/generic_scraper.R")
source("./R/utilities.R")

new_hampshire_pull <- function(x){
    xml2::read_html(x)
}

new_hampshire_restruct <- function(x){
    tab_set <- x %>%
        rvest::html_nodes(xpath="//table[@border=1]")
    
    list(
        staff = tab_set[[1]] %>%
            rvest::html_table(header = TRUE) %>%
            as_tibble(),
        resident = tab_set[[2]] %>%
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
        Residents.Tested = "# Residents Tested",
        Residents.Confirmed = "# Residents Positive")
    staff_exp <- c(Name = "Worksite", Staff.Confirmed = "# Staff Positive")
    
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
#'   \item{# Staff Positive}{The staff number testing positive}
#'   \item{# Residents tested}{Not the number of tests administered}
#'   \item{# Residents Positive}{Number of residents confirmed}
#'   \item{NHDOC Staff}{State wide staff population}
#' }

new_hampshire_scraper <- R6Class(
    "new_hampshire_scraper",
    inherit = generic_scraper,
    public = list(
        log = NULL,
        initialize = function(
            log,
            url = "https://www.nh.gov/nhdoc/covid/index.html",
            id = "new_hampshire",
            type = "html",
            state = "NH",
            # pull the JSON data directly from the API
            pull_func = new_hampshire_pull,
            # restructuring the data means pulling out the data portion of the json
            restruct_func = new_hampshire_restruct,
            # Rename the columns to appropriate database names
            extract_func = new_hampshire_extract){
            super$initialize(
                url = url, id = id, pull_func = pull_func, type = type,
                restruct_func = restruct_func, extract_func = extract_func,
                log = log, state = state)
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

