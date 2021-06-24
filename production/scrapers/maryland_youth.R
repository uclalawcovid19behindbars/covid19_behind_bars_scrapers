source("./R/generic_scraper.R")
source("./R/utilities.R")

maryland_youth_pull <- function(x){
    srcs <- get_src_by_attr(x, "a", attr = "href", attr_regex = "(?i)daily-report")
    djs_pdf <- srcs %>% .[[1]]
    return(djs_pdf)
    ## gotta ask neal about this! saving multiple pdfs 
    # contract_pdf <- srcs %>% .[[3]]
    # sub_dir <- str_c(
    #     "./results/raw_files/", Sys.Date(), "_maryland_youth")
    # dir.create(sub_dir, showWarnings = FALSE)
}

maryland_youth_restruct <- function(x){
    djs_tab <- magick::image_read_pdf(x)
    # djs_tab <- magick::image_read_pdf(x[[1]])
    # contract_tab <- magick::image_read_pdf(x[[2]])
    
    djs_results <- djs_tab %>%
                         magick::image_crop("1900x16750+200+1600") %>%
                         ExtractTable()
    
    contract_results <- contract_tab %>%
        magick::image_crop("1900x267+200+1000") %>%
        ExtractTable()
    out <- list(djs_results, contract_results)
    return(out)
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
    df_ <- as.data.frame(x[[1]]) %>% 
        slice(-1)
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
            check_date = NULL,
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

