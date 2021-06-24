source("./R/generic_scraper.R")
source("./R/utilities.R")

south_dakota_population_pull <- function(x){
    get_src_by_attr(x, "a", attr = "href", 
                    attr_regex = "(?i)documents/AdultPopulation.*.pdf")
}

south_dakota_population_restruct <- function(x, exp_date = Sys.Date()){
    
    pdf_date <- x %>% 
        magick::image_read_pdf() %>% 
        magick::image_crop("800x200+50+100") %>% 
        magick::image_ocr() %>% 
        lubridate::mdy()
    
    error_on_date(pdf_date, exp_date)
    
    x %>% 
        magick::image_read_pdf() %>% 
        ExtractTable() 
}

south_dakota_population_extract <- function(x){
    
    col_name_mat <- matrix(c(
        "Adult Corrections: End of Month Population:", "0", "Name",
        "State Males", "1", "State.Males.Drop",
        "State Females", "2", "State.Females.Drop", 
        "State Total", "3", "State.Total.Drop", 
        "Federal Males", "4", "Fedearl.Males.Drop", 
        "Federal Females", "5", "Federal.Females.Drop", 
        "Total Inmates", "6", "Residents.Population"
    ), ncol = 3, nrow = 7, byrow = TRUE)
    
    colnames(col_name_mat) <- c("check", "raw", "clean")
    col_name_df <- as_tibble(col_name_mat)
    
    check_names_extractable(x[[1]], col_name_df)
    
    rename_extractable(x[[1]], col_name_df) %>% 
        select(!ends_with(".Drop")) %>% 
        filter(!str_detect(Name, "Population")) %>% 
        clean_scraped_df()
}

#' Scraper class for South Dakota population data 
#' 
#' @name south_dakota_population_scraper
#' @description South Dakota's DOC posts end-of-month population PDFs with 
#' facility-level population data disaggregated by gender and federal vs. state. 
#' Rows for Community (inmates assigned to work release, community service work, 
#' and the community based Intensive Methamphetamine Treatment program) and 
#' Other (includes temporary absences (out to court, medical, furlough, or 
#' extended confinement) are included. 
#' \describe{
#'   \item{State Names}{}
#'   \item{State Females}{}
#'   \item{State Total}{}
#'   \item{Federal Males}{}
#'   \item{Federal Females}{}
#'   \item{Total Inmates}{Residents.Population}
#' }

south_dakota_population_scraper <- R6Class(
    "south_dakota_population_scraper",
    inherit = generic_scraper,
    public = list(
        log = NULL,
        initialize = function(
            log,
            url = "https://doc.sd.gov/about/stats/adult/index.aspx",
            id = "south_dakota_population",
            type = "pdf",
            state = "SD",
            jurisdiction = "state",
            check_date = NULL,
            pull_func = south_dakota_population_pull,
            restruct_func = south_dakota_population_restruct,
            extract_func = south_dakota_population_extract){
            super$initialize(
                url = url, id = id, pull_func = pull_func, type = type,
                restruct_func = restruct_func, extract_func = extract_func,
                log = log, state = state, jurisdiction  = jurisdiction,
                check_date = check_date)
        }
    )
)

if(sys.nframe() == 0){
    south_dakota_population <- south_dakota_population_scraper$new(log=TRUE)
    south_dakota_population$run_check_date()
    south_dakota_population$raw_data
    south_dakota_population$pull_raw()
    south_dakota_population$raw_data
    south_dakota_population$save_raw()
    south_dakota_population$restruct_raw()
    south_dakota_population$restruct_data
    south_dakota_population$extract_from_raw()
    south_dakota_population$extract_data
    south_dakota_population$validate_extract()
    south_dakota_population$save_extract()
}
