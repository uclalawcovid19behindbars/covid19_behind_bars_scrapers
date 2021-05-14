source("./R/generic_scraper.R")
source("./R/utilities.R")

historical_tn_pop_pull <- function(x, date, file = NULL){
    
    date <- as.Date(date, format = "%Y-%m-%d")
    
    month <- format.Date(date, "%B")
    year4 <- year4 <- format.Date(date, "%Y")
    
    stringr::str_c("https://www.tn.gov/content/dam/tn/correction/documents/Bed", 
                   month, year4, ".pdf")
}

historical_tn_pop_restruct <- function(x, date = NULL){
    
    x %>% 
        magick::image_read_pdf(pages = 2) %>% 
        magick::image_crop("2400x1400+50+250") %>% 
        magick::image_convert(type = 'Grayscale') %>% 
        ExtractTable() %>% 
        .[[1]]
}

historical_tn_extract <- function(x, date = NULL){
    x_ <- x %>%  
        rownames_to_column() %>% 
        mutate(rowname = as.numeric(rowname)) %>% 
        arrange(rowname) %>% 
        janitor::row_to_names(row_number = 3) 
    
    check_names(x_, c(
        "2", "INSTITUTION", "Total Active Beds", "# Beds", "As % of Total Active Beds", 
        "# Inmates", "As % of Total Active Beds", "As % of Operating Capacity"))
    
    x_ %>% 
        select(Name = INSTITUTION, 
               Residents.Population = `# Inmates`) %>% 
        mutate(Name = clean_fac_col_txt(Name)) %>% 
        mutate(Name = str_remove(Name, fixed("^"))) %>% 
        filter(!str_detect(Name, "(?i)total")) %>% 
        clean_scraped_df()
}

#' Scraper class for historical Tennessee population data 
#' 
#' @name historical_tn_pop_scraper
#' @description Tennessee posts daily population reports in PDF form with 
#' assigned counts and operating capacities as of the last day of each month. 
#' 
#' \describe{
#'   \item{Total Active Beds}{}
#'   \item{Established Operating Capacity}{}
#'   \item{Assigned Count # Inmates}{}
#' }

historical_tn_pop_scraper <- R6Class(
    "historical_tn_pop_scraper",
    inherit = generic_scraper,
    public = list(
        log = NULL,
        initialize = function(
            log,
            url = "https://www.tn.gov/correction/statistics-and-information/bed-space---capacity-reports.html",
            id = "historical_tn_pop",
            type = "pdf",
            state = "TN",
            jurisdiction = "state",
            pull_func = historical_tn_pop_pull,
            restruct_func = historical_tn_pop_restruct,
            extract_func = historical_tn_extract){
            super$initialize(
                url = url, id = id, pull_func = pull_func, type = type,
                restruct_func = restruct_func, extract_func = extract_func,
                log = log, state = state, jurisdiction  = jurisdiction)
        }
    )
)

if(sys.nframe() == 0){
    historical_tn_pop <- historical_tn_pop_scraper$new(log=TRUE)
    historical_tn_pop$reset_date("DATE")
    historical_tn_pop$raw_data
    historical_tn_pop$pull_raw(date = historical_tn_pop$date, .dated_pull = TRUE)
    historical_tn_pop$raw_data
    historical_tn_pop$save_raw()
    historical_tn_pop$restruct_raw()
    historical_tn_pop$restruct_data
    historical_tn_pop$extract_from_raw()
    historical_tn_pop$extract_data
    historical_tn_pop$validate_extract()
    historical_tn_pop$save_extract()
}
