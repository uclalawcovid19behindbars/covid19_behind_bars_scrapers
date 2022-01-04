source("./R/generic_scraper.R")
source("./R/utilities.R")

mississippi_date_check <- function(url, date = Sys.Date()){
    pdf <- get_src_by_attr(url, "a", attr="href", attr_regex = "(?i)cases") %>%
        first()
    
    pdf_read <- magick::image_read_pdf(pdf, pages = 1)
    h_ <- magick::image_info(pdf_read)$height
    height_offset <- h_ - (1/8*h_)
    
    date_string <- pdf_read %>%
        magick::image_crop(str_c(height_offset, "x200+400+", height_offset)) %>%
        magick::image_ocr() %>%
        {.[str_detect(., "(?i)last update")]} %>%
        str_split("Last Update: ") %>%
        unlist() %>%
        .[2] %>% 
        str_remove("\n")
        
    date_string %>%
        lubridate::mdy_h() %>% 
        lubridate::floor_date(unit = "days") %>%
        lubridate::as_date() %>%
        error_on_date(date)
}

mississippi_pull <- function(x){
    get_src_by_attr(x, "a", attr="href", attr_regex = "(?i)cases") %>%
        first()
}

mississippi_restruct <- function(x){
    ms_pgs <- magick::image_read_pdf(x[[1]])
    ExtractTable(ms_pgs)
}

mississippi_extract <- function(x){
    col_name_mat <- matrix(c(
        "State Institutions", "X0", "Name",
        "Cumulative Positives", "X1", "Residents.Confirmed",
        "Current Active", "X2", "Residents.Active"
        ), ncol = 3, nrow = 3, byrow = TRUE)
    
    colnames(col_name_mat) <- c("check", "raw", "clean")
    col_name_df <- as_tibble(col_name_mat)
    
    df_ <- as.data.frame(x)
    
    check_names_extractable(df_, col_name_df)
    rename_extractable(df_, col_name_df) %>%
        filter(!(Name %in% c("State Institutions",
                             "Regionals",
                             "Other",
                             "Private Institutions",
                             "TOTALS"))) %>%
        clean_scraped_df() %>%
        mutate(Residents.Recovered = Residents.Confirmed - Residents.Active) %>%
        as_tibble()
}

#' Scraper class for general mississippi COVID data
#' 
#' @name mississippi_scraper
#' @description MS provides data for a number of facilities through a pdf
#' however minimal data is provided for each facility.
#' \describe{
#'   \item{State Institutions}{The facility name}
#'   \item{Total Positives}{Confirmed residents}
#'   \item{Active}{Residents currently infected}
#' }

mississippi_scraper <- R6Class(
    "mississippi_scraper",
    inherit = generic_scraper,
    public = list(
        log = NULL,
        initialize = function(
            log,
            url = "https://www.mdoc.ms.gov/Pages/COVID-19-Information-and-Updates.aspx",
            id = "mississippi",
            type = "pdf",
            state = "MS",
            jurisdiction = "state",
            check_date = mississippi_date_check,
            # pull the JSON data directly from the API
            pull_func = mississippi_pull,
            # restructuring the data means pulling out the data portion of the json
            restruct_func = mississippi_restruct,
            # Rename the columns to appropriate database names
            extract_func = mississippi_extract){
            super$initialize(
                url = url, id = id, pull_func = pull_func, type = type,
                restruct_func = restruct_func, extract_func = extract_func,
                log = log, state = state, jurisdiction = jurisdiction,
                check_date = check_date)
        }
    )
)

if(sys.nframe() == 0){
    mississippi <- mississippi_scraper$new(log=TRUE)
    mississippi$run_check_date()
    mississippi$raw_data
    mississippi$pull_raw()
    mississippi$raw_data
    mississippi$save_raw()
    mississippi$restruct_raw()
    mississippi$restruct_data
    mississippi$extract_from_raw()
    mississippi$extract_data
    mississippi$validate_extract()
    mississippi$save_extract()
}

