source("./R/generic_scraper.R")
source("./R/utilities.R")

historical_or_pop_pull <- function(x){
    get_src_by_attr(x, "a", attr = "href", attr_regex = "inmate-profile.pdf")
}

historical_or_pop_restruct <- function(x, date = NULL){
    
    pdf_date <- x %>% 
        magick::image_read_pdf(pages = 1) %>% 
        magick::image_crop("1600x200+500x400") %>% 
        magick::image_ocr() %>% 
        sub(".*Profile for", "", .) %>% 
        lubridate::mdy()
    
    error_on_date(date, pdf_date)
    
    x <- x %>% 
        tabulizer::extract_tables()
    
    x7_ <- x[[7]] %>% 
        janitor::row_to_names(row_number = 2) %>% 
        as.data.frame()
    
    joined <- x[[1]] %>% 
        cbind(x[[2]]) %>% 
        as.data.frame() %>% 
        janitor::row_to_names(row_number = 2) %>% 
        left_join(x7_, by = c("Demographic", "Value")) 
    
    if (joined %>% filter(TOTAL.x != TOTAL.y) %>% nrow() > 0){
        stop(stringr::str_c(
            "Totals on page 2 and 7 don't match. ", 
            "Check if raw file structure changed.")
        )
    }
    
    if (joined[1,1] != "TOTAL" | joined[1,2] != "INMATE POPULATION"){
        stop(stringr::str_c(
            "First row on pages 1, 2, 7 should be total population. ", 
            "Check if raw file structure changed.")
        )
    }
    
    joined
}

historical_or_pop_extract <- function(x){
    total <- x %>% 
        filter(Demographic == "TOTAL") %>% 
        pull(TOTAL.x) %>% 
        string_to_clean_numeric()
    
    out <- x %>% 
        filter(Demographic == "TOTAL") %>% 
        t() %>% 
        as.data.frame() %>% 
        rownames_to_column() %>% 
        janitor::row_to_names(row_number = 2) %>% 
        rename(
            "Name" = "Value", 
            "Residents.Population" = "INMATE POPULATION"
        ) %>% 
        filter(!str_detect(Name, "(?i)total")) %>% 
        clean_scraped_df()
    
    if (sum_na_rm(out$Residents.Population) != total){
        warning("Sum of populations ", sum_na_rm(out$Residents.Population), 
                " does not match expected total ", total)
    }
    
    out
}

#' Scraper class for historical Oregon population data
#' 
#' @name historical_oregon_pop_scraper
#' @description Oregon population data comes from population profile PDFs. The 
#' data is spread across multiple tables and multiple pages and includes detailed
#' demographic breakdowns (not scraped) including gender, age, race, custody level, 
#' time to release, sentencing guidelines, life/death sentence, crime type, 
#' offense group, security threat group, dangerous offender, sex offender, 
#' drug offender, education need, mental health need, substance abuse need, 
#' developmental disability. It seems like this is updated monthly on the first 
#' of the month. 
#' 
#' \describe{
#'   \item{Demographic}{}
#'   \item{Value}{}
#'   \item{Facility [...]}
#' }

historical_or_population_scraper <- R6Class(
    "historical_oregon_pop_scraper",
    inherit = generic_scraper,
    public = list(
        log = NULL,
        initialize = function(
            log,
            url = "https://www.oregon.gov/doc/research-and-requests/pages/research-and-statistics.aspx",
            id = "historical_or_population",
            type = "pdf",
            state = "OR",
            jurisdiction = "state",
            # pull the JSON data directly from the API
            pull_func = historical_or_pop_pull,
            # restructuring the data means pulling out the data portion of the json
            restruct_func = historical_or_pop_restruct,
            # Rename the columns to appropriate database names
            extract_func = historical_or_pop_extract){
            super$initialize(
                url = url, id = id, pull_func = pull_func, type = type,
                restruct_func = restruct_func, extract_func = extract_func,
                log = log, state = state, jurisdiction = jurisdiction)
        }
    )
)

if(sys.nframe() == 0){
    historical_or_pop <- historical_or_population_scraper$new(log=TRUE)
    historical_or_pop$reset_date("2021-05-01")
    historical_or_pop$raw_data
    historical_or_pop$pull_raw(.dated_pull = TRUE)
    historical_or_pop$raw_data
    historical_or_pop$save_raw()
    historical_or_pop$restruct_raw(date = historical_or_pop$date)
    historical_or_pop$restruct_data
    historical_or_pop$extract_from_raw()
    historical_or_pop$extract_data
    historical_or_pop$validate_extract()
    historical_or_pop$save_extract()
}

