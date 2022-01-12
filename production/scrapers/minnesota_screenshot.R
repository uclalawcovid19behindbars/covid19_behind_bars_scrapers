source("./R/generic_scraper.R")
source("./R/utilities.R")

minnesota_screenshot_date_check <- function(x, date = Sys.Date()){
    
    base_html <- minnesota_screenshot_pull()
    
    base_html %>% 
        rvest::html_elements("p") %>% 
        rvest::html_text() %>% 
        {.[str_detect(., "(?i)as of")]} %>% 
        str_extract("\\d{1,2}/\\d{1,2}/\\d{2,4}") %>% 
        lubridate::mdy() %>% 
        error_on_date(date)
}

minnesota_screenshot_pull <- function(x){
    screenshot_path <- "/tmp/sel_dl/mn.png"
    mn_img <- magick::image_read(screenshot_path) %>% 
        magick::image_convert(type = 'Grayscale')
}

minnesota_screenshot_restruct <- function(mn_img){
    # Run through OCR
    table_extract <- ExtractTable(mn_img)
    
    df_res <- table_extract %>%
        .[[2]] %>%
        filter(!is.na(.[,1])) %>%
        as.data.frame() %>% 
        filter(!str_detect(.[,1], "(?i)total"))
    names(df_res) <- df_res[1,]
    
    ## get rid of the first row (column names)
    df_res <- df_res %>%
        .[2:nrow(.), ] 
    
    exp_res <- c(
        Name = "Primary",
        Residents.Active = "Currently Positive",
        Residents.Tadmin = "Total Tests Administered",
        Residents.Negative = "Confirmed Negative (some duplicate test results)",
        Residents.Confirmed = "Confirmed Positive",
        Residents.Pending = "Tests Pending",
        Residents.Hospital.Drop = "Hospitalized",
        Residents.Recovered = "Recovered (includes presumed positive)",
        Residents.Deaths = "Total Deceased"
    )
    
    exp_staff <- c(
        Name = "Primary",
        Staff.Confirmed = "Confirmed Positive Test",
        Presumed.Drop = "Presumed Positive",
        Staff.Recovered = "Returned to Work"
    )
    
    df_staff <- table_extract %>%
        .[[1]] %>%
        filter(!is.na(.[,1])) %>%
        as.data.frame() %>% 
        filter(!str_detect(.[,1], "(?i)total"))
    names(df_staff) <- df_staff[1,]
    
    ## get rid of the first row (column names)
    df_staff <- df_staff %>%
        .[2:nrow(.), ] 

    basic_check(names(df_res), exp_res)
    basic_check(names(df_staff), exp_staff)
    
    names(df_res) <- names(exp_res)
    names(df_staff) <- names(exp_staff)
    
    restruct_out <- clean_scraped_df(df_res) %>%
        mutate(Name = str_replace(Name, "(?i)st\\.* cloud", "Saint Cloud")) %>% 
        full_join(
            clean_scraped_df(df_staff) %>%
                mutate(Name = str_replace(Name, "(?i)st\\.* cloud", "Saint Cloud")),
            by = "Name") %>%
        as_tibble() 
    return(restruct_out)
}

minnesota_screenshot_extract <- function(restruct){
    extract <- restruct %>%
        mutate(Staff.Confirmed = Staff.Confirmed + Presumed.Drop) %>%
        select(!contains("Drop"))
    return(extract)
}

#' Scraper class for general Minnesota COVID data
#' 
#' @name minnesota_screenshot_scraper
#' @description MN data comes from an iframe that is loaded from smart sheets.
#' The main data sources come from two primary tables, one for staff and
#' another for residents.
#' \describe{
#'   \item{Primary}{Facility Name}
#'   \item{Resident Total Tests Administered}{}
#'   \item{Resident Confirmed Negative}{}
#'   \item{Resident Confirmed Positive}{}
#'   \item{Resident Tests Pending}{}
#'   \item{Resident Hospitalized}{}
#'   \item{Resident Recovered}{}
#'   \item{Resident Deceased}{}
#'   \item{Staff Confirmed Positive Test}{}
#'   \item{Staff Hospitalized}{}
#'   \item{Staff Presumed Positive}{}
#'   \item{Staff Returned to Work}{}
#' }

minnesota_screenshot_scraper <- R6Class(
    "minnesota_screenshot_scraper",
    inherit = generic_scraper,
    public = list(
        log = NULL,
        initialize = function(
            log,
            url = "https://mn.gov/doc/about/covid-19-updates/",
            id = "minnesota_screenshot",
            type = "img",
            state = "MN",
            jurisdiction = "state",
            check_date = NULL,
            # pull the JSON data directly from the API
            pull_func = minnesota_screenshot_pull,
            # restructuring the data means pulling out the data portion of the json
            restruct_func = minnesota_screenshot_restruct,
            # Rename the columns to appropriate database names
            extract_func = minnesota_screenshot_extract){
            super$initialize(
                url = url, id = id, pull_func = pull_func, type = type,
                restruct_func = restruct_func, extract_func = extract_func,
                log = log, state = state, jurisdiction = jurisdiction,
                check_date = check_date)
        }
    )
)

if(sys.nframe() == 0){
    minnesota_screenshot <- minnesota_screenshot_scraper$new(log=TRUE)
    minnesota_screenshot$run_check_date()
    minnesota_screenshot$raw_data
    minnesota_screenshot$pull_raw()
    minnesota_screenshot$raw_data
    minnesota_screenshot$save_raw()
    minnesota_screenshot$restruct_raw()
    minnesota_screenshot$restruct_data
    minnesota_screenshot$extract_from_raw()
    minnesota_screenshot$extract_data
    minnesota_screenshot$validate_extract()
    minnesota_screenshot$save_extract()
}

