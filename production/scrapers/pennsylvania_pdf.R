source("./R/generic_scraper.R")
source("./R/utilities.R")

pennsylvania_pdf_pull <- function(x){
    get_src_by_attr(x, "a", attr = "href", attr_regex = "(?i)daily-count")
}

pennsylvania_pdf_restruct <- function(x){
    pa_pgs <- magick::image_read_pdf(x)
    # ExtractTable(pa_pgs)
    if(length(pa_pgs) == 1){
        restruct_results <- list(pa_pgs %>%
                                     # magick::image_crop("2550x1350+0+518") %>%
                                     magick::image_crop("2550x2350+0+518") %>%
                                     ExtractTable())
    }
    
    else{
        tmp_files <- sapply(pa_pgs, function(li){
            f <- tempfile(fileext = ".png")
            li %>% 
                magick::image_write(f, format = "png")
            f
        })
        
        restruct_results <- lapply(tmp_files, ExtractTable)
    }
    
    restruct_results
}

pennsylvania_pdf_extract <- function(x){
    col_name_mat <- matrix(c(
        "SCI", "X0", "Name",
        "Inmate Active Positive Cases", "X1", "Residents.Active",
        "Asymptomatic Inmate Positive", "X2", "Drop.Residents.Asymp",
        "Inmate Deaths to date", "X3", "Residents.Deaths",
        "Employee Active Positive Cases", "X4", "Drop.Staff.Active",
        "Employee Deaths to date", "X5", "Staff.Deaths"
    ), ncol = 3, nrow = 6, byrow = TRUE)
    
    colnames(col_name_mat) <- c("check", "raw", "clean")
    col_name_df <- as_tibble(col_name_mat)
    df_ <- as.data.frame(x) 
    check_names_extractable(df_, col_name_df)
    
    # check_names_extractable(df_, col_name_df)
    rename_extractable(df_, col_name_df) %>%
        filter(!str_detect(Name, "(?i)SCI")) %>%
        as_tibble() %>%
        select(-starts_with("Drop")) %>% 
        filter(!str_detect(Name, "(?i)total")) %>%
        clean_scraped_df() 
        
}

#' Scraper class for general pennsylvania_pdf COVID data
#' 
#' @name pennsylvania_pdf_scraper
#' @description PA provides data for a number of facilities through a pdf
#' however minimal data is provided for each facility.
#' \describe{
#'   \item{SCI}{The facility name}
#'   \item{Inmate Active Positive Cases}{Confirmed active cases among residents}
#'   \item{Inmate Deaths to Date}{Cumulative resident deaths}
#'   \item{Employee Deaths to Date}{Cumulative staff deaths}

#' }

pennsylvania_pdf_scraper <- R6Class(
    "pennsylvania_pdf_scraper",
    inherit = generic_scraper,
    public = list(
        log = NULL,
        initialize = function(
            log,
            url = "https://www.cor.pa.gov/Pages/COVID-19.aspx",
            id = "pennsylvania_pdf",
            type = "pdf",
            state = "PA",
            jurisdiction = "state",
            # pull the JSON data directly from the API
            pull_func = pennsylvania_pdf_pull,
            # restructuring the data means pulling out the data portion of the json
            restruct_func = pennsylvania_pdf_restruct,
            # Rename the columns to appropriate database names
            extract_func = pennsylvania_pdf_extract){
            super$initialize(
                url = url, id = id, pull_func = pull_func, type = type,
                restruct_func = restruct_func, extract_func = extract_func,
                log = log, state = state, jurisdiction = jurisdiction)
        }
    )
)

if(sys.nframe() == 0){
    pennsylvania_pdf <- pennsylvania_pdf_scraper$new(log=TRUE)
    pennsylvania_pdf$raw_data
    pennsylvania_pdf$pull_raw()
    pennsylvania_pdf$raw_data
    pennsylvania_pdf$save_raw()
    pennsylvania_pdf$restruct_raw()
    pennsylvania_pdf$restruct_data
    pennsylvania_pdf$extract_from_raw()
    pennsylvania_pdf$extract_data
    pennsylvania_pdf$validate_extract()
    pennsylvania_pdf$save_extract()
}

