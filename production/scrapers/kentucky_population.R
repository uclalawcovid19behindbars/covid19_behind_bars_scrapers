source("./R/generic_scraper.R")
source("./R/utilities.R")

kentucky_population_pull <- function(x, date = Sys.Date(), days = 7){
    
    # See if there is a daily count sheet for today, if not check the previous day, etc. 
    found_url <- FALSE 
    
    while (!found_url) {
        date_mmddyy <- format(date, "%m-%d-%y")
        url <- get_src_by_attr(x, "a", attr = "href", attr_regex = stringr::str_c(
            "(?i)", date_mmddyy, ".pdf"))
        
        found_url <- ifelse(length(url) == 0, FALSE, TRUE)
        date <- date - 1
        
        # This is also checked in the restruct function 
        # Added here to avoid accidentally entering an infinite while loop if the page changes 
        if ((Sys.Date() - date) > days){
            stop(str_c("Date is more than ", days, " different from expected"))
        }
    }
    return(url)
}

kentucky_population_restruct <- function(x, date = Sys.Date()){
    #browser()
    pdf_date <- magick::image_read_pdf(x) %>% 
        magick::image_crop("900x100+800+240") %>% 
        magick::image_ocr() %>% 
        lubridate::mdy()
    
    error_on_date(date, pdf_date)
    
    magick::image_read_pdf(x) %>% 
        magick::image_crop("2000x1800+0+300") %>% 
        ExtractTable()
}

kentucky_population_extract <- function(x, date = Sys.Date()){
    
    df_ <- x %>% 
        .[[1]] %>% 
        as.data.frame() %>% 
        janitor::row_to_names(row_number = 1) %>% 
        janitor::clean_names()
    
    check_names(df_, c(
        "location", "in_count", "hospital", "court", "other",
        "total", "operational_capacity"))
    
    cleaned <- df_ %>% 
        select(Name = `location`, 
               Residents.Population = `in_count`) %>% 
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

#' Scraper class for Kentucky population data 
#' 
#' @name kentucky_population_scraper
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

kentucky_population_scraper <- R6Class(
    "kentucky_population_scraper",
    inherit = generic_scraper,
    public = list(
        log = NULL,
        initialize = function(
            log,
            url = "https://corrections.ky.gov/About/researchandstats/Pages/dailycount.aspx",
            id = "kentucky_population",
            type = "pdf",
            state = "KY",
            jurisdiction = "state",
            check_date = NULL,
            pull_func = kentucky_population_pull,
            restruct_func = kentucky_population_restruct,
            extract_func = kentucky_population_extract){
            super$initialize(
                url = url, id = id, pull_func = pull_func, type = type,
                restruct_func = restruct_func, extract_func = extract_func,
                log = log, state = state, jurisdiction  = jurisdiction,
                check_date = check_date)
        }
    )
)

if(sys.nframe() == 0){
    kentucky_population <- kentucky_population_scraper$new(log=TRUE)
    kentucky_population$run_check_date()
    kentucky_population$raw_data
    kentucky_population$pull_raw()
    kentucky_population$raw_data
    kentucky_population$save_raw()
    kentucky_population$restruct_raw()
    kentucky_population$restruct_data
    kentucky_population$extract_from_raw()
    kentucky_population$extract_data
    kentucky_population$validate_extract()
    kentucky_population$save_extract()
}

