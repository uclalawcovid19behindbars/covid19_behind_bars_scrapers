source("./R/generic_scraper.R")
source("./R/utilities.R")

california_psychiatric_pull <- function(x){
    get_src_by_attr(x, "img", attr = "src", attr_regex = "(?i)patient") %>%
        magick::image_read()
}

california_psychiatric_restruct <- function(x){
    NULL
}

california_psychiatric_extract <- function(x){
    NULL
}

#' Scraper class for general california_psychiatric COVID data
#' 
#' @name california_psychiatric_scraper
#' @description Pull the resident data from the first image. This data is
#' facility specific so we need to grab the facility names from the first row.
#' Only need to grab three rows of relevant data. Any value that is less than
#' 11 should be treated as NA.
#' 
#' \describe{
#'   \item{Patients Positive}{Residents.Confirmed}
#'   \item{Patients Death}{Residents.Deaths}
#'   \item{Tests}{Residents.Tadmin}
#' }

california_psychiatric_scraper <- R6Class(
    "california_psychiatric_scraper",
    inherit = generic_scraper,
    public = list(
        log = NULL,
        initialize = function(
            log,
            url = "https://www.dsh.ca.gov/COVID-19/Patient_and_Staff_COVID-19_Tracking.html",
            id = "california_psychiatric",
            type = "img",
            state = "CA",
            jurisdiction = "psychiatric",
            pull_func = california_psychiatric_pull,
            restruct_func = california_psychiatric_restruct,
            extract_func = california_psychiatric_extract){
            super$initialize(
                url = url, id = id, pull_func = pull_func, type = type,
                restruct_func = restruct_func, extract_func = extract_func,
                log = log, state = state, jurisdiction = jurisdiction)
        }
    )
)

if(sys.nframe() == 0){
    california_psychiatric <- california_psychiatric_scraper$new(log=TRUE)
    california_psychiatric$perma_save()
    california_psychiatric$raw_data
    california_psychiatric$pull_raw()
    california_psychiatric$raw_data
    california_psychiatric$save_raw()
    california_psychiatric$restruct_raw()
    california_psychiatric$restruct_data
    california_psychiatric$extract_from_raw()
    california_psychiatric$extract_data
    california_psychiatric$validate_extract()
    california_psychiatric$save_extract()
}

