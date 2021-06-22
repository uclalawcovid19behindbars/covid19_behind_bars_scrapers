source("./R/generic_scraper.R")
source("./R/utilities.R")

# File name is inconsistent, specify in config file 
historical_in_pop_pull <- function(x, file, date = NULL){
    file
}

get_in_pop_table <- function(x, idx, check_names = TRUE){
    col_name_mat <- matrix(c(
        "Facility Name", "0", "Name",
        "Beds", "1", "Drop.Capacity",
        "Offenders", "2", "Residents.Population", 
        "% Capacity", "3", "Drop.Capacity.Pct"
    ), ncol = 3, nrow = 4, byrow = TRUE)
    
    colnames(col_name_mat) <- c("check", "raw", "clean")
    col_name_df <- as_tibble(col_name_mat)
    
    if(check_names){
        check_names_extractable(x[[idx]], col_name_df)
        rename_extractable(x[[idx]], col_name_df) %>%
            as_tibble() 
    } else{
        names(x[[idx]]) <- col_name_df$clean
        x[[idx]] %>% 
            as_tibble()
    }
}

historical_in_pop_restruct <- function(x, date = NULL){
    # Check date
    date_pdf <- magick::image_read_pdf(x, pages = 9) %>%
        magick::image_crop("1300x100+1100+200") %>%
        magick::image_ocr() %>%
        str_match("(?<=Snapshot date:)(.*)") %>%
        lubridate::mdy() %>%
        head(1)

    if (date != date_pdf) {
        warning(str_c("Extracted date ", date_pdf, " does not match expected date ", date))
    }
    
    c(magick::image_read_pdf(x, pages = 9) %>% 
          magick::image_crop("1500x2300+400+300") %>%
          # magick::image_crop("1150x2000+300x200") %>% 
          ExtractTable(),  
      magick::image_read_pdf(x, pages = 10) %>% 
          magick::image_crop("1500x2400+400+400") %>%
          # magick::image_crop("1120x2400+300x200") %>% 
          ExtractTable()
    )
}

historical_in_pop_extract <- function(x, date = NULL){
    # 6 tables should have headers 
    idx <- which(sapply(x, function(z){
        any(str_detect(z[1,1], "(?i)facility"))}))
    
    # 2 tables don't have headers but should have the same as the others 
    idx_other <- which(sapply(x, function(z){
        any(str_detect(z[1,1], c("(?i)women", "(?i)reception")))}))
    
    if(length(idx) != 6 | length(idx_other) != 2){
        warning("Number of tables does not match expected.")
    }
    
    bind_rows(
        lapply(idx, function(z){
            get_in_pop_table(x, z)}) %>% 
            bind_rows() , 
        lapply(idx_other, function(z){
            get_in_pop_table(x, z, check_names = FALSE)}) %>% 
            bind_rows()) %>% 
        filter(Name != "Facility Name") %>% 
        filter(!str_detect(Name, "(?i)total")) %>% 
        clean_scraped_df() %>% 
        select(-starts_with("Drop"))
}

#' Scraper class for Indiana population data
#' 
#' @name historical_indiana_pop_scraper
#' @description The Indiana DOC posts extremely long and detailed monthly population
#' reports around the first of every month. This includes monthly admissions, 
#' population by county of commit, facility breakdowns, offense classification
#' breakdowns, etc. There also seems to be data from county jails and community 
#' corrections. 
#' \describe{
#'   \item{Facility Name}{}
#'   \item{Beds}{Capacity}
#'   \item{Offenders}{Residents.Population}
#'   \item{% Capacity}{}
#' }

historical_in_pop_scraper <- R6Class(
    "historical_indiana_pop_scraper",
    inherit = generic_scraper,
    public = list(
        log = NULL,
        initialize = function(
            log,
            url = "https://www.in.gov/idoc/data-and-statistics/statistical-data/total-population-summary-reports/",
            id = "historical_in_pop",
            type = "pdf",
            state = "IN",
            jurisdiction = "state",
            check_date = NULL,
            pull_func = historical_in_pop_pull,
            restruct_func = historical_in_pop_restruct,
            extract_func = historical_in_pop_extract){
            super$initialize(
                url = url, id = id, pull_func = pull_func, type = type,
                restruct_func = restruct_func, extract_func = extract_func,
                log = log, state = state, jurisdiction  = jurisdiction, 
                check_date = check_date)
        }
    )
)

if(sys.nframe() == 0){
    historical_in_pop <- historical_in_pop_scraper$new(log=TRUE)
    historical_in_pop$reset_date("DATE")
    historical_in_pop$raw_data
    historical_in_pop$pull_raw(file, .dated_pull = TRUE)
    historical_in_pop$raw_data
    historical_in_pop$save_raw()
    historical_in_pop$restruct_raw(date = historical_in_pop$date)
    historical_in_pop$restruct_data
    historical_in_pop$extract_from_raw()
    historical_in_pop$extract_data
    historical_in_pop$validate_extract()
    historical_in_pop$save_extract()
}
