source("./R/generic_scraper.R")
source("./R/utilities.R")

maryland_youth_date_check <- function(x, date = Sys.Date()){
    pdf <- get_src_by_attr(x, "a", attr = "href", attr_regex = "(?i)daily-report") %>%
        .[[1]]
    
    magick::image_read_pdf(pdf) %>% 
        magick::image_crop("1000x200+700+500") %>% 
        magick::image_ocr() %>% 
        str_split("(?i)date") %>% 
        unlist() %>%
        {.[str_detect(., "21")]} %>% 
        lubridate::mdy() %>% 
        error_on_date(date)
}

maryland_youth_pull <- function(x){
    get_src_by_attr(x, "a", attr = "href", attr_regex = "(?i)daily-report") %>%
        .[[1]]
}

maryland_youth_restruct <- function(x){
    md_tab <- magick::image_read_pdf(x)
    
    restruct_results <- md_tab %>%
                         # magick::image_crop("2550x2350+0+518") %>%
                         magick::image_crop("1900x16750+200+1600") %>%
                         ExtractTable()
    restruct_results
}

maryland_youth_extract <- function(x){
    col_name_mat <- matrix(c(
        "Facilities Summary", "X0", "Name",
        "# of Tests on Youth", "X1", "Residents.Tadmin",
        "COVID+ Youth", "X2", "Residents.Confirmed",
        "Recovered Youth", "X3", "Residents.Recovered",
        "COVID+ Staff", "X4", "Staff.Confirmed",
        "Recovered Staff", "X5", "Staff.Recovered"
    ), ncol = 3, nrow = 6, byrow = TRUE)
    
    colnames(col_name_mat) <- c("check", "raw", "clean")
    col_name_df <- as_tibble(col_name_mat)
    
    # Drop first row (second row is column names)
    df_ <- as.data.frame(x) %>% 
        slice(-1)
    
    # check_names_extractable(df_, col_name_df)
    
    out <- rename_extractable(df_, col_name_df) %>%
        as_tibble() %>%
        filter(!str_detect(Name, "(?i)total")) %>%
        clean_scraped_df() %>%
        mutate(Name = str_c(toupper(Name), " YOUTH"))
}

#' Scraper class for general maryland youth COVID data
#' 
#' @name maryland_youth_scraper
#' @description The MD  Dept of Juvenile Services provides data for a number of facilities through a PDF 
#' \describe{
#'   \item{DJS Facility Name}{The facility name}
#'   \item{# of Tests on Youth}{Cumulative tests administered to youth}
#'   \item{COVID+ Youth}{Cumulative resident cases}
#'   \item{Recovered Youth}{Cumulative residents recovered}
#'   \item{COVID+ Staff}{Cumulative staff cases}
#'   \item{Recovered Staff}{Cumulative staff recovered}
#' }

maryland_youth_scraper <- R6Class(
    "maryland_youth_scraper",
    inherit = generic_scraper,
    public = list(
        log = NULL,
        initialize = function(
            log,
            url = "https://djs.maryland.gov/Pages/COVID-19.aspx",
            id = "maryland_youth",
            type = "pdf",
            state = "MD",
            jurisdiction = "state",
            check_date = maryland_youth_date_check,
            # pull the JSON data directly from the API
            pull_func = maryland_youth_pull,
            # restructuring the data means pulling out the data portion of the json
            restruct_func = maryland_youth_restruct,
            # Rename the columns to appropriate database names
            extract_func = maryland_youth_extract){
            super$initialize(
                url = url, id = id, pull_func = pull_func, type = type,
                restruct_func = restruct_func, extract_func = extract_func,
                log = log, state = state, jurisdiction = jurisdiction,
                check_date = check_date)
        }
    )
)

if(sys.nframe() == 0){
    maryland_youth <- maryland_youth_scraper$new(log=TRUE)
    maryland_youth$run_check_date()
    maryland_youth$raw_data
    maryland_youth$pull_raw()
    maryland_youth$raw_data
    maryland_youth$save_raw()
    maryland_youth$restruct_raw()
    maryland_youth$restruct_data
    maryland_youth$extract_from_raw()
    maryland_youth$extract_data
    maryland_youth$validate_extract()
    maryland_youth$save_extract()
}

