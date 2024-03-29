source("./R/generic_scraper.R")
source("./R/utilities.R")

ohio_check_date <- function(url, date = Sys.Date()){
    src <-  get_src_by_attr(url, "a", attr = "href", attr_regex = "(?i)covid")
    img <- magick::image_read_pdf(src, pages = 1) 
    
    date_box <- magick::image_crop(img, "500x240+900+160") %>% 
        magick::image_ocr()
    
    date_box %>%
        {.[str_detect(., "(?i)20")]} %>% # look for year 20xx
        str_extract("\\d{1,2}/\\d{1,2}/\\d{2,4}") %>%
        lubridate::mdy() %>%
        error_on_date(expected_date = date)
}

ohio_pull <- function(x){
    get_src_by_attr(x, "a", attr = "href", attr_regex = "(?i)covid")
}

ohio_restruct <- function(x){
    oh_pgs <- magick::image_read_pdf(x)
    
    if(length(oh_pgs) == 1){
        restruct_results <- list(oh_pgs %>%
            magick::image_crop("2550x1350+0+518") %>%
            ExtractTable())
    }
    
    else{
        tmp_files <- sapply(oh_pgs, function(li){
            f <- tempfile(fileext = ".png")
            li %>% 
                magick::image_write(f, format = "png")
            f
        })
        
        restruct_results <- lapply(tmp_files, ExtractTable)
    }
    
    restruct_results
}

ohio_extract <- function(x){
    
    col_name_mat <- matrix(c(
        "Institution", "0", "Name",
        # "# of Staff who have Reported Positive Tests", "1", "Staff.Confirmed",
        "# of Staff Currently Positive", "1", "Staff.Active",
        "# of COVID- 19 Related Staff Deaths", "2", "Staff.Deaths",
        # "# of Staff who have Recovered", "4", "Staff.Recovered",
        "Housing Type (cell, open bay, combo)", "3", "Housing.Type",
        #"# of Inmates in Quarantine", "6", "Residents.Quarantine",
        "# of Inmates in Isolation", "4", "Residents.Isolation",
        "# of inmates currently Positive for COVID-19", "5", "Residents.Active",
        "# of Confirmed COVID-19 Related Inmate Deaths", "6", "Residents.Confirmed.Deaths",
        "# of Inmates who have Pending Results",  "7", "Residents.Pending"
    ), ncol = 3, nrow = 8, byrow = TRUE)
    
    colnames(col_name_mat) <- c("check", "raw", "clean")
    col_name_df <- as_tibble(col_name_mat)
    
    bind_rows(lapply(x, function(li){
        df_ <- as_tibble(li[[1]])
        check_names_extractable(df_, col_name_df)
        renamed_df <- rename_extractable(df_, col_name_df) %>%
            select(-Housing.Type) %>%
            filter(Name != "Institution" & Name != "Totals") %>%
            filter(!str_detect(Name, "(?i)total"))})) %>%
        clean_scraped_df() %>% 
        mutate(Residents.Deaths = Residents.Confirmed.Deaths ) %>% 
        select(
            -Residents.Pending, -Residents.Isolation,
            -Residents.Confirmed.Deaths)
}

#' Scraper class for general Ohio COVID data
#' 
#' @name ohio_scraper
#' @description Data come from a pdf which is updated periodically. The link
#' to the pdf itself does not change only the data within the pdf. We should be
#' periodically checking to see if alternative sources are available as the data
#' collected are sometimes fickle. Cumulative positive cases come from the sum of 
#' Active + Recovered + Deaths. We're summing confirmed and probable deaths for 
#' Residents.Deaths, but we're NOT including probable deaths in the Residents.Confirmed
#' total. 
#' \describe{
#'   \item{Institution}{}
#'   \item{Housing Type (cell, open bay, combo)}{}
#'   \item{# of Staff who have Reported Positive Tests}{}
#'   \item{# of COVID- 19 Related Staff Deaths}{}
#'   \item{# of Staff who have Recovered}{}
#'   \item{Units in Quarantine}{}
#'   \item{# of Inmates in Quarantine}{}
#'   \item{# of Inmates in Isolation}{}
#'   \item{# of inmates currently Positive for COVID-19}{}
#'   \item{# of Probable COVID-19 Related Inmate Deaths}{}
#'   \item{# of Confirmed COVID-19 Related Inmate Deaths}{}
#'   \item{# of Inmates who have Pending Results}{}
#'   \item{# of current Inmates who have Recovered}{}
#' }

ohio_scraper <- R6Class(
    "ohio_scraper",
    inherit = generic_scraper,
    public = list(
        log = NULL,
        initialize = function(
            log,
            url = "https://drc.ohio.gov/Organization/Research/Reports/COVID-19-Information",
            id = "ohio",
            type = "pdf",
            state = "OH",
            jurisdiction = "state",
            check_date = ohio_check_date,
            pull_func = ohio_pull,
            restruct_func = ohio_restruct,
            extract_func = ohio_extract){
            super$initialize(
                url = url, id = id, pull_func = pull_func, type = type,
                restruct_func = restruct_func, extract_func = extract_func,
                log = log, state = state, jurisdiction = jurisdiction,
                check_date = check_date)
        })
)

if(sys.nframe() == 0){
    ohio <- ohio_scraper$new(log=TRUE)
    ohio$run_check_date()
    ohio$raw_data
    ohio$pull_raw()
    ohio$save_raw()
    ohio$raw_data
    ohio$restruct_raw()
    ohio$restruct_data
    ohio$extract_from_raw()
    ohio$extract_data
    ohio$validate_extract()
    ohio$save_extract()
}