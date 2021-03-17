source("./R/generic_scraper.R")
source("./R/utilities.R")

mississippi_pull <- function(x){
    get_src_by_attr(x, "a", attr="href", attr_regex = "(?i)cases")
}

mississippi_restruct <- function(x, exp_date = Sys.Date()){
    ms_pgs <- magick::image_read_pdf(x)
    
    date <- magick::image_crop(ms_pgs, "1000x200+400+2700") %>% 
        magick::image_ocr() %>% 
        lubridate::mdy()
    
    error_on_date(date, exp_date)
    
    ExtractTable(ms_pgs)
}

mississippi_extract <- function(x){
    col_name_mat <- matrix(c(
        "State Institutions", "X0", "Name",
        "Total Yearly Positives", "X1", "Residents.Confirmed",
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
            # pull the JSON data directly from the API
            pull_func = mississippi_pull,
            # restructuring the data means pulling out the data portion of the json
            restruct_func = mississippi_restruct,
            # Rename the columns to appropriate database names
            extract_func = mississippi_extract){
            super$initialize(
                url = url, id = id, pull_func = pull_func, type = type,
                restruct_func = restruct_func, extract_func = extract_func,
                log = log, state = state, jurisdiction = jurisdiction)
        }
    )
)

if(sys.nframe() == 0){
    mississippi <- mississippi_scraper$new(log=TRUE)
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

