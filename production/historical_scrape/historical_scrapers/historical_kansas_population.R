source("./R/generic_scraper.R")
source("./R/utilities.R")

historical_ks_pop_pull <- function(x, date, file = NULL){
    date <- as.Date(date, format = "%Y-%m-%d")
    year4 <- format.Date(date, "%Y")
    month2 <- format.Date(date, "%m")
    day <- format.Date(date, "%d")
    
    str_c("https://www.doc.ks.gov/publications/pop/POP%20", 
          month2, "-", day, "-", year4, ".pdf")
}

historical_ks_pop_restruct <- function(x, date = NULL){
    pdf_date <- x %>% 
        magick::image_read_pdf(pages = 1) %>% 
        magick::image_crop("400x100") %>% 
        magick::image_ocr() %>% 
        lubridate::mdy()
    
    error_on_date(pdf_date, date)
    
    magick::image_read_pdf(x, pages = 1) %>% 
        ExtractTable()
}

historical_ks_pop_extract <- function(x, date = NULL){
    
    # Check table dimensions 
    if (!all(dim(x[[1]]) == c(38, 17))){
        warning("Dimensions of table not as expected. Inspect raw file.")
    }
    
    x_ <- x[[1]] %>% 
        select(seq(0, 16) %>% as.character()) %>% 
        rownames_to_column() %>% 
        mutate(rowname = as.numeric(rowname)) %>% 
        arrange(rowname) %>% 
        select(-rowname)
    
    # Check relevant column positions 
    if (!all(c(x_[1,1] == "NAME OF FACILITY", 
              x_[1,6] == "CURRENT", 
              x_[3,7] == "TOT"))){
        warning("Column names not as expected. Inspect raw file.")
    }
    
    tab <- x_[c(1, 7)] 
    names(tab) <- c("Name", "Residents.Population")
    
    # Hacky hard coded prison names 
    # We've only ever scraped COVID data from 9 facilities in KS, so this should work 
    out <- tab %>% 
        filter(str_detect(Name, str_c(
            "(?i)", 
            "lansing|", 
            "hutchinson|", 
            "dorado|", 
            "topeka|", 
            "norton|", 
            "ellsworth|", 
            "winfield|", 
            "wichita|", 
            "larned"))) %>% 
        mutate(Name = str_remove_all(Name, "- TOTAL")) %>% 
        clean_scraped_df() 
    
    if (nrow(out) != 9){
        warning("Number of facilities ", nrow(out), " doesn't match expected 9.")
    }
    
    exp_total <- tab %>%
        filter(str_detect(clean_fac_col_txt(Name), "(?i)subtotal kdoc")) %>%
        pull(Residents.Population) %>%
        first() %>%
        string_to_clean_numeric()

    if (exp_total != sum_na_rm(out$Residents.Population)){
        warning("Total ", exp_total, " doesn't match expected ", sum_na_rm(out$Residents.Population))
    }
    
    return(out)
}

#' Scraper class for Kansas population data 
#' 
#' @name historical_kansas_pop_scraper
#' @description The Kansas DOC posts daily adult population reports on Monday 
#' through Friday and archives historical reports. In addition to the data scraped
#' here, the reports also include operating capacity, male and female population, 
#' parole populations, and population by security level and classification by facility. 
#' \describe{
#'   \item{KDOC Facilities}{}
#'   \item{Operating Capacity}{}
#'   \item{Current Inmate Population}{}
#' }

historical_ks_pop_scraper <- R6Class(
    "historical_kansas_pop_scraper",
    inherit = generic_scraper,
    public = list(
        log = NULL,
        initialize = function(
            log,
            url = "https://www.doc.ks.gov/publications/population-report/",
            id = "historical_ks_pop",
            type = "pdf",
            state = "KS",
            jurisdiction = "state",
            check_date = NULL,
            pull_func = historical_ks_pop_pull,
            restruct_func = historical_ks_pop_restruct,
            extract_func = historical_ks_pop_extract){
            super$initialize(
                url = url, id = id, pull_func = pull_func, type = type,
                restruct_func = restruct_func, extract_func = extract_func,
                log = log, state = state, jurisdiction  = jurisdiction,
                check_date = check_date)
        }
    )
)

if(sys.nframe() == 0){
    historical_ks_pop <- historical_ks_pop_scraper$new(log=TRUE)
    historical_ks_pop$reset_date("DATE")
    historical_ks_pop$raw_data
    historical_ks_pop$pull_raw(date = historical_ks_pop$date, .dated_pull = TRUE)
    historical_ks_pop$raw_data
    historical_ks_pop$save_raw()
    historical_ks_pop$restruct_raw(date = historical_ks_pop$date)
    historical_ks_pop$restruct_data
    historical_ks_pop$extract_from_raw(date = historical_ks_pop$date)
    historical_ks_pop$extract_data
    historical_ks_pop$validate_extract()
    historical_ks_pop$save_extract()
}
