source("./R/generic_scraper.R")
source("./R/utilities.R")

new_jersey_statewide_pull <- function(x){
    xml2::read_html(x)
}

new_jersey_statewide_restruct <- function(x){
    titles <- x %>% 
        rvest::html_nodes(".col-md-12") %>% 
        rvest::html_nodes(".card_title_DOC2") %>% 
        rvest::html_text()
    
    res_idx <- which(stringr::str_detect(titles, "(?i)incarcerated"))
    staff_idx <- which(stringr::str_detect(titles, "(?i)staff"))
    
    tables <- x %>% 
        rvest::html_nodes(".col-md-12") %>% 
        rvest::html_nodes(".card_body_doc") %>% 
        rvest::html_table()
    
    res_table <- tables[[res_idx]]
    staff_table <- tables[[staff_idx]]
    
    rbind(
        res_table %>% 
            as_tibble() %>% 
            mutate(X1 = clean_fac_col_txt(stringr::str_c(X1, " Residents"))), 
        
        staff_table %>% 
            as_tibble() %>% 
            mutate(X1 = clean_fac_col_txt(stringr::str_c(X1, " Staff")))
    ) %>% 
        t() %>% 
        janitor::row_to_names(row_number = 1) %>% 
        as_tibble()
}

new_jersey_statewide_extract <- function(x){
    
    exp_names <- c(
        "Test Completed Residents", 
        "Cumulative Positives Residents", 
        "Vaccines Doses Distributed Residents", 
        "Deaths Residents", 
        "Test Completed Staff", 
        "Cumulative Positives Staff", 
        "Vaccines Doses Distributed Staff"
    )
    
    check_names(x, exp_names)
    
    names(x) <- c(
        "Residents.Tadmin", 
        "Residents.Confirmed.Drop", # Collecting facility-level in main scraper 
        "Residents.Vadmin", 
        "Residents.Deaths.Drop", # Collecting facility-level in main scraper 
        "Staff.Tadmin.Drop", # We don't collect Staff.Tadmin 
        "Staff.Confirmed.Drop", # Collecting facility-level in main scraper 
        "Staff.Vadmin"
    )
    
    x %>% 
        mutate(Name = "STATEWIDE") %>% 
        select(-ends_with(".Drop")) %>% 
        clean_scraped_df()
}

#' Scraper class for general new_jersey COVID data
#' 
#' @name new_jersey_statewide_scraper
#' @description In addition to the main NJ dashboard, they started reporting a 
#' new dashboard with statewide totals for  tests completed and vaccine doses 
#' distributed for incarcerated people and staff. 
#' \describe{
#'   \item{Incarcerated Population Test Completed}{}
#'   \item{Incarcerated Population Cumulative Positives}{}
#'   \item{Incarcerated Population Vaccines Doses Distributed}{}
#'   \item{Incarcerated Population Deaths}{}
#'   \item{Staff Population Test Completed}{}
#'   \item{Staff Population Cumulative Positives}{}
#'   \item{Staff Population Vaccines Doses Distributed}{}
#' }

new_jersey_statewide_scraper <- R6Class(
    "new_jersey_statewide_scraper",
    inherit = generic_scraper,
    public = list(
        log = NULL,
        initialize = function(
            log,
            url = "https://www.state.nj.us/corrections/pages/COVID_Rev2.html",
            id = "new_jersey_statewide",
            type = "html",
            state = "NJ",
            jurisdiction = "state",
            # pull the JSON data directly from the API
            pull_func = new_jersey_statewide_pull,
            # restructuring the data means pulling out the data portion of the json
            restruct_func = new_jersey_statewide_restruct,
            # Rename the columns to appropriate database names
            extract_func = new_jersey_statewide_extract){
            super$initialize(
                url = url, id = id, pull_func = pull_func, type = type,
                restruct_func = restruct_func, extract_func = extract_func,
                log = log, state = state, jurisdiction = jurisdiction)
        }
    )
)

if(sys.nframe() == 0){
    new_jersey_statewide <- new_jersey_statewide_scraper$new(log=TRUE)
    new_jersey_statewide$raw_data
    new_jersey_statewide$pull_raw()
    new_jersey_statewide$raw_data
    new_jersey_statewide$save_raw()
    new_jersey_statewide$restruct_raw()
    new_jersey_statewide$restruct_data
    new_jersey_statewide$extract_from_raw()
    new_jersey_statewide$extract_data
    new_jersey_statewide$validate_extract()
    new_jersey_statewide$save_extract()
}
