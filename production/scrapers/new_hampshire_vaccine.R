source("./R/generic_scraper.R")
source("./R/utilities.R")

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
    
    if(ncol(x) != 7){
        stop("html is not as expected please inspect")
    }
    
    long_df <- x[,1:3] %>%
        bind_rows(rename(x[,4:7], X1 = X4, X2 = X5, X3 = X6)) %>%
        as_tibble()
    
    if(!any(str_detect(long_df$X1, "(?i)phase"))){
        stop("html is not as expected please inspect")
    }
    
    if(!any(str_detect(long_df$X2, "(?i)1st Dose"))){
        stop("html is not as expected please inspect")
    }
    
    if(!any(str_detect(long_df$X3, "(?i)2nd Dose"))){
        stop("html is not as expected please inspect")
    }
    
    if(!any(str_detect(long_df$X7, "(?i)1 Dose"))){
        stop("html is not as expected please inspect")
    }
    
    long_df %>%
        rename(Name = X1, 
               First.Dose = X2, 
               Second.Dose = X3, 
               One.Dose = X7) %>% 
        mutate(across(ends_with("Dose"), ~ na_if(., "n/a"))) %>% 
        filter(!str_detect(Name, "(?i)total")) %>%
        filter(!str_detect(Name, "(?i)phase")) %>% 
        clean_scraped_df() %>%
        group_by(Name) %>%
        summarize_all(sum_na_rm) %>%
        mutate(Residents.Vadmin = First.Dose + Second.Dose + One.Dose, 
               Residents.Initiated = First.Dose + One.Dose, 
               Residents.Completed = Second.Dose + One.Dose) %>% 
        filter(!Name == "") %>% 
        select(-ends_with("Dose"))
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
            check_date = NULL,
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

