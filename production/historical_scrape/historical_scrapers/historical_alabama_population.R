source("./R/generic_scraper.R")
source("./R/utilities.R")

historical_al_pop_restruct <- function(x, date){
    # October is on page 6, others on page 4 
    p <- ifelse(lubridate::month(date) == 10, 6, 4)
    
    x %>%
        magick::image_read_pdf(pages = p) %>%
        magick::image_crop("1270x1950+400+200") %>%
        ExtractTable() 
}

historical_al_pop_extract <- function(x, date = NULL){
    x_ <- x %>% 
        as.data.frame() %>% 
        janitor::row_to_names(row_number = 1) %>% 
        janitor::clean_names(replace = c("[:digit:]" = ""))
        
    check_names(x_, c(
        "facility", 
        "designed_capacity", 
        "current_beds", 
        "month_end_population", 
        "difference", 
        "occupancy_rate"
    ))
    
    out <- x_ %>% 
        select(Name = facility, 
               Residents.Population = month_end_population) %>% 
        mutate(Name = clean_fac_col_txt(Name, to_upper = TRUE), 
               Name = str_replace_all(Name, "[:digit:]", "")) %>% 
        filter(!str_detect(Name, "TOTAL")) %>% 
        clean_scraped_df() %>% 
        mutate(Name = ifelse(str_detect(Name, "TUTWILER"), "TUTWILER", Name)) %>% 
        group_by(Name) %>% 
        summarise(Residents.Population = sum_na_rm(Residents.Population)) %>% 
        ungroup()
    
    exp_fac <- 28
    if (!nrow(out) == exp_fac){
        warning(stringr::str_c("Total number of facilities ", nrow(out), 
                               " does not match expected ", exp_fac))
    }

    out 
}

#' Scraper class for Alabama historical population data 
#' 
#' @name historical_al_pop_scraper
#' @description Alabama's DOC posts monthly statistical reports in PDF form and 
#' archives these reports. They are long (16+ pages) and provide a lot of 
#' detailed information that isn't scraped. These reports are published 2-3 
#' months ex-post.  
#' 
#' \describe{
#'   \item{Facility}{}
#'   \item{Designed Capacity}{}
#'   \item{Current Beds}{}
#'   \item{Month End Population{}
#'   \item{Difference{}
#'   \item{Occupancy Rate{}
#' }

historical_al_pop_scraper <- R6Class(
    "historical_alabama_pop_scraper",
    inherit = generic_scraper,
    public = list(
        log = NULL,
        initialize = function(
            log,
            url = "http://www.doc.state.al.us/StatReports",
            id = "historical_al_pop",
            type = "pdf",
            state = "AL",
            jurisdiction = "state",
            pull_func = function(x, file, date = NULL){file},
            restruct_func = historical_al_pop_restruct,
            extract_func = historical_al_pop_extract){
            super$initialize(
                url = url, id = id, pull_func = pull_func, type = type,
                restruct_func = restruct_func, extract_func = extract_func,
                log = log, state = state, jurisdiction  = jurisdiction)
        }
    )
)

if(sys.nframe() == 0){
    historical_al_pop <- historical_al_pop_scraper$new(log=TRUE)
    historical_al_pop$reset_date("DATE")
    historical_al_pop$raw_data
    historical_al_pop$pull_raw(file, .dated_pull = TRUE)
    historical_al_pop$raw_data
    historical_al_pop$save_raw()
    historical_al_pop$restruct_raw(date = historical_al_pop$date)
    historical_al_pop$restruct_data
    historical_al_pop$extract_from_raw()
    historical_al_pop$extract_data
    historical_al_pop$validate_extract()
    historical_al_pop$save_extract()
}

