source("./R/generic_scraper.R")
source("./R/utilities.R")

historical_ky_pop_pull <- function(x, date, file = NULL){
    
    date_mmddyy <- format(date, "%m-%d-%y")
    year4 <- format.Date(date, "%Y")
    month2 <- format.Date(date, "%m")

    stringr::str_c(
        "https://corrections.ky.gov/About/researchandstats/Documents/Daily%20Population/", 
        year4, "/", month2, "/", date_mmddyy, ".pdf"
    )
}
    
historical_ky_pop_restruct <- function(x, date){
    
    pdf_date <- magick::image_read_pdf(x) %>% 
        magick::image_crop("900x100+800+240") %>% 
        magick::image_ocr() %>% 
        lubridate::mdy()
    
    error_on_date(date, pdf_date, days = 0)
    
    magick::image_read_pdf(x) %>% 
        magick::image_crop("2000x1800+0+300") %>% 
        ExtractTable()
}
    
historical_ky_pop_extract <- function(x, date){
    
    df_ <- x %>% 
        .[[1]] %>% 
        as.data.frame() %>% 
        janitor::row_to_names(row_number = 2)
    
    check_names(df_, c(
        "Location", "In Count", "Hospital", "Court", "Other",
        "Total", "Operational Capacity"))
    
    cleaned <- df_ %>% 
        select(Name = `Location:`, 
               Residents.Population = `In Count`) %>% 
        mutate(Name = clean_fac_col_txt(Name, to_upper = TRUE)) %>% 
        filter(Name != "") %>% 
        clean_scraped_df() 
    
    # The table lists sub-facilities under the same name 
    out <- cleaned %>% 
        filter(!stringr::str_detect(Name, "TOTAL|ASSESSMENT|ROSS")) %>% 
        group_by(Name) %>% 
        # Pull the max to avoid double-counting 
        summarise(Residents.Population = max(Residents.Population)) %>% 
        ungroup()
    
    exp_total <- cleaned %>% 
        filter(stringr::str_detect(Name, "TOTAL")) %>% 
        pull(Residents.Population) 
    
    total <- sum(out$Residents.Population)
    if (exp_total != total){
        stop(str_c("Total population ", total, " different from expected ", exp_total, 
                   ". Inspect raw file."))
    }
    exp_fac <- ifelse(date > "2020-7-16", 14, 13)
    if (nrow(out) != exp_fac){
        stop(str_c("Number of facilities ", nrow(out), 
                   " different from expected ", exp_fac))
    }
    out 
}

#' Scraper class for historical Kentucky population data 
#' 
#' @name historical_kentucky_pop_scraper
#' @description KY's DOC posts daily count sheets with facility-level population 
#' data. These are posted most weekdays and archived in PDF form back to 2011. 
#' Data on community service programs is reported and not scraped. 
#' \describe{
#'   \item{Location}{Facility name}
#'   \item{In Count}{Incarcerated population}
#'   \item{Out to Hospital}{}
#'   \item{Out to Court}{}
#'   \item{Total}{}
#'   \item{Operational Capacity}{}
#' }

historical_ky_pop_scraper <- R6Class(
    "historical_kentucky_pop_scraper",
    inherit = generic_scraper,
    public = list(
        log = NULL,
        initialize = function(
            log,
            url = "https://corrections.ky.gov/About/researchandstats/Pages/dailycount.aspx",
            id = "historical_ky_pop",
            type = "pdf",
            state = "KY",
            jurisdiction = "state",
            pull_func = historical_ky_pop_pull,
            restruct_func = historical_ky_pop_restruct,
            extract_func = historical_ky_pop_extract){
            super$initialize(
                url = url, id = id, pull_func = pull_func, type = type,
                restruct_func = restruct_func, extract_func = extract_func,
                log = log, state = state, jurisdiction  = jurisdiction)
        }
    )
)

if(sys.nframe() == 0){
    historical_ky_pop <- historical_ky_pop_scraper$new(log=TRUE)
    historical_ky_pop$reset_date("DATE")
    historical_ky_pop$raw_data
    historical_ky_pop$pull_raw(date = historical_ky_pop$date, .dated_pull = TRUE)
    historical_ky_pop$raw_data
    historical_ky_pop$save_raw()
    historical_ky_pop$restruct_raw(date = historical_ky_pop$date)
    historical_ky_pop$restruct_data
    historical_ky_pop$extract_from_raw(date = historical_ky_pop$date)
    historical_ky_pop$extract_data
    historical_ky_pop$validate_extract()
    historical_ky_pop$save_extract()
}
