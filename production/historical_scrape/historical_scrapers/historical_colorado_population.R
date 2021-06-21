source("./R/generic_scraper.R")
source("./R/utilities.R")

historical_co_pop_pull <- function(x, file, date = NULL){
    # Can't access files directly since they're hosted on Google Drive 
    # Download locally first 
    stringr::str_c("results/local/", file)
}

historical_co_pop_restruct <- function(x, date){
    
    pdf_date <- x %>% 
        magick::image_read_pdf(pages = 1) %>% 
        magick::image_crop("400x80+1100+200") %>% 
        magick::image_ocr() %>% 
        lubridate::mdy()
    
    if (date != pdf_date){
        stop(stringr::str_c("Scraper date ", date, " does not match PDF date ", pdf_date))
    }
    
    magick::image_read_pdf(x, pages = 1) %>%
        magick::image_crop("2400x1550+100+250") %>%
        ExtractTable()
}

historical_co_pop_extract <- function(x, date = NULL){
    
    df_ <-  x[[1]] %>% 
        janitor::row_to_names(row_number = 1) %>% 
        janitor::clean_names(replace = c("[:digit:]" = "")) %>% 
        select(
            Name = facility_security_level, 
            Residents.Population = facility_population) %>% 
        mutate(Name = clean_fac_col_txt(Name)) %>% 
        clean_scraped_df() %>% 
        # Aggregate infirmaries with facilities to match COVID reporting aggregation 
        mutate(Name = case_when(
            str_detect(Name, "Colorado Territorial Corr Fac") ~ "Colorado Territorial Corr Fac", 
            str_detect(Name, "Denver Rec Diag Ctr") ~ "Denver Rec Diag Ctr", 
            TRUE ~ Name)) %>% 
        group_by(Name) %>% 
        summarise(Residents.Population = sum(Residents.Population)) %>% 
        ungroup() 
    
    total <- df_ %>%
        filter(Name == "State Private Prison Total") %>%
        pull(Residents.Population)
    
    out <- df_%>% 
        filter(!stringr::str_detect(Name, "Subtotal|Total")) 
    
    if (sum(out$Residents.Population) != total) {
        stop(stringr::str_c("Total population does not match. Inspect for changed names."))
    }
    
    exp_fac <- ifelse(
        date > as.Date("2020-07-01"), 23, 22)
    
    if (nrow(out) != exp_fac){
        stop(stringr::str_c("Total number of facilities ", nrow(out), 
                            " does not match expected ", exp_fac))
    }
    
    out 
}

#' Scraper class for Colorado historical population data 
#' 
#' @name historical_co_pop_scraper
#' @description Colorado's DOC reports facility-level population data through 
#' monthly PDFs as of the last day of each month. The reports also include data 
#' broken out by various other classifications. 
#' 
#' \describe{
#'   \item{Facility (Security Level)}{Facility name}
#'   \item{Punitive Beds}{}
#'   \item{RTP}{Residential Treatment Program}
#'   \item{Operational Capacity}{}
#'   \item{Vacant Beds}{}
#'   \item{On Grounds Population}{}
#'   \item{Off-Grounds Population}{}
#'   \item{Facility Population}{Residents.Population}
#' }

historical_co_pop_scraper <- R6Class(
    "historical_colorado_pop_scraper",
    inherit = generic_scraper,
    public = list(
        log = NULL,
        initialize = function(
            log,
            url = "https://cdoc.colorado.gov/about/data-and-reports/statistics",
            id = "historical_co_pop",
            type = "pdf",
            state = "CO",
            jurisdiction = "state",
            check_date = NULL,
            pull_func = historical_co_pop_pull,
            restruct_func = historical_co_pop_restruct,
            extract_func = historical_co_pop_extract){
            super$initialize(
                url = url, id = id, pull_func = pull_func, type = type,
                restruct_func = restruct_func, extract_func = extract_func,
                log = log, state = state, jurisdiction  = jurisdiction,
                check_date = check_date)
        }
    )
)

if(sys.nframe() == 0){
    historical_co_pop <- historical_co_pop_scraper$new(log=TRUE)
    historical_co_pop$reset_date("DATE")
    historical_co_pop$raw_data
    historical_co_pop$pull_raw(file, .dated_pull = TRUE)
    historical_co_pop$raw_data
    historical_co_pop$save_raw()
    historical_co_pop$restruct_raw(date = historical_co_pop$date)
    historical_co_pop$restruct_data
    historical_co_pop$extract_from_raw(date = historical_co_pop$date)
    historical_co_pop$extract_data
    historical_co_pop$validate_extract()
    historical_co_pop$save_extract()
}
