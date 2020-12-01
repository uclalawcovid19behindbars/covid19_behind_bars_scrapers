source("./R/generic_scraper.R")
source("./R/utilities.R")

missouri_pull <- function(x){
    get_src_by_attr(x, "img", attr="src", attr_regex = "(?i)covid-chart") %>%
        magick::image_read()
}

missouri_restruct <- function(x){
    ExtractTable(x)
}

missouri_extract <- function(x){
    col_name_mat <- matrix(c(
        "Facility", "0", "Name",
        "Staff Active Cases", "1", "Drop.Staff.Active",
        "Staff Recovered", "2", "Staff.Recovered",
        "Offender Active Cases", "3", "Residents.Active",
        "Offenders Recovered", "4", "Residents.Recovered"
    ), ncol = 3, nrow = 5, byrow = TRUE)
    
    colnames(col_name_mat) <- c("check", "raw", "clean")
    col_name_df <- as_tibble(col_name_mat)
    
    check_names_extractable(x[[1]], col_name_df)
    
    rename_extractable(x[[1]], col_name_df) %>%
        filter(Name!="Facility") %>%
        select(-starts_with("Drop")) %>%
        clean_scraped_df() %>%
        as_tibble()
}

#' Scraper class for general Missouri COVID data
#' 
#' @name missouri_scraper
#' @description Facility specific MO data comes from a power bi html table. 
#' Cumulative cases for facilities and deaths at the state level appear to
#' be no longer reported here.
#' \describe{
#'   \item{Facility}{The facility name}
#'   \item{Staff Active Cases}{Current infections among staff}
#'   \item{Staff Recovered}{Staff who have recovered from infection}
#'   \item{Offender Active Cases}{Current infections among residents}
#'   \item{Offenders Recovered}{Residents who have recovered from infection}
#' }

missouri_scraper <- R6Class(
    "missouri_scraper",
    inherit = generic_scraper,
    public = list(
        log = NULL,
        initialize = function(
            log,
            url = "https://doc.mo.gov/media-center/newsroom/covid-19/data",
            id = "missouri",
            type = "img",
            state = "MO",
            jurisdiction = "state",
            # pull the JSON data directly from the API
            pull_func = missouri_pull,
            # restructuring the data means pulling out the data portion of the json
            restruct_func = missouri_restruct,
            # Rename the columns to appropriate database names
            extract_func = missouri_extract){
            super$initialize(
                url = url, id = id, pull_func = pull_func, type = type,
                restruct_func = restruct_func, extract_func = extract_func,
                log = log, state = state, jurisdiction = jurisdiction)
        }
    )
)

if(sys.nframe() == 0){
    missouri <- missouri_scraper$new(log=TRUE)
    missouri$raw_data
    missouri$pull_raw()
    missouri$raw_data
    missouri$save_raw()
    missouri$restruct_raw()
    missouri$restruct_data
    missouri$extract_from_raw()
    missouri$extract_data
    missouri$validate_extract()
    missouri$save_extract()
}

