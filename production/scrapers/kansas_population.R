source("./R/generic_scraper.R")
source("./R/utilities.R")

kansas_check_date <- function(x, date = Sys.Date()){
    url <- get_src_by_attr(x, "a", attr = "href", attr_regex = "POP%") %>% 
        first()
    
    url %>% 
        magick::image_read_pdf(pages = 1) %>% 
        magick::image_crop("400x100") %>% 
        magick::image_ocr() %>% 
        lubridate::mdy() %>% 
        error_on_date(date)
}

kansas_population_pull <- function(x, date = Sys.Date(), days = 7){
    get_src_by_attr(x, "a", attr = "href", attr_regex = "POP") %>% 
        first()
}

kansas_population_restruct <- function(x){
    magick::image_read_pdf(x, pages = 1) %>% 
        ExtractTable()
}

kansas_population_extract <- function(x){
    
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
#' @name kansas_population_scraper
#' @description The Kansas DOC posts daily adult population reports on Monday 
#' through Friday and archives historical reports. In addition to the data scraped
#' here, the reports also include operating capacity, male and female population, 
#' parole populations, and population by security level and classification by facility. 
#' \describe{
#'   \item{KDOC Facilities}{}
#'   \item{Operating Capacity}{}
#'   \item{Current Inmate Population}{}
#' }

kansas_population_scraper <- R6Class(
    "kansas_population_scraper",
    inherit = generic_scraper,
    public = list(
        log = NULL,
        initialize = function(
            log,
            url = "https://www.doc.ks.gov/publications/population-report/",
            id = "kansas_population",
            type = "pdf",
            state = "KS",
            jurisdiction = "state",
            check_date = kansas_check_date,
            pull_func = kansas_population_pull,
            restruct_func = kansas_population_restruct,
            extract_func = kansas_population_extract){
            super$initialize(
                url = url, id = id, pull_func = pull_func, type = type,
                restruct_func = restruct_func, extract_func = extract_func,
                log = log, state = state, jurisdiction  = jurisdiction,
                check_date = check_date)
        }
    )
)

if(sys.nframe() == 0){
    kansas_population <- kansas_population_scraper$new(log=TRUE)
    kansas_population$run_check_date()
    kansas_population$raw_data
    kansas_population$pull_raw()
    kansas_population$raw_data
    kansas_population$save_raw()
    kansas_population$restruct_raw()
    kansas_population$restruct_data
    kansas_population$extract_from_raw()
    kansas_population$extract_data
    kansas_population$validate_extract()
    kansas_population$save_extract()
}
