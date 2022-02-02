source("./R/generic_scraper.R")
source("./R/utilities.R")

california_psychiatric_staff_pull <- function(x){
    get_src_by_attr(x, "img", attr = "src", attr_regex = "(?i)staff") %>%
        magick::image_read()
}

california_psychiatric_staff_restruct <- function(x){
    stop_defunct_scraper("https://www.dsh.ca.gov/COVID-19/Patient_and_Staff_COVID-19_Tracking.html")
}

california_psychiatric_staff_extract <- function(x){
    NULL
}

#' Scraper class for general california_psychiatric_staff COVID data
#' 
#' @name california_psychiatric_staff_scraper
#' @description Pull the staff data from the second image. This data is
#' facility specific so we need to grab the facility names from the first row.
#' Only need to grab three rows of relevant data. Any value that is less than
#' 11 should be treated as NA.
#' 
#' \describe{
#'   \item{Staff Positive}{Staff.Confirmed}
#' }

california_psychiatric_staff_scraper <- R6Class(
    "california_psychiatric_staff_scraper",
    inherit = generic_scraper,
    public = list(
        log = NULL,
        initialize = function(
            log,
            url = "https://www.dsh.ca.gov/COVID-19/Patient_and_Staff_COVID-19_Tracking.html",
            id = "california_psychiatric_staff",
            type = "img",
            state = "CA",
            jurisdiction = "psychiatric",
            pull_func = california_psychiatric_staff_pull,
            restruct_func = california_psychiatric_staff_restruct,
            extract_func = california_psychiatric_staff_extract,
            check_date = NULL){
            super$initialize(
                url = url, id = id, pull_func = pull_func, type = type,
                restruct_func = restruct_func, extract_func = extract_func,
                log = log, state = state, jurisdiction = jurisdiction,
                check_date = check_date)
        }
    )
)

if(sys.nframe() == 0){
    california_psychiatric_staff <- california_psychiatric_staff_scraper$new(log=TRUE)
    california_psychiatric_staff$run_check_date()
    california_psychiatric_staff$perma_save()
    california_psychiatric_staff$raw_data
    california_psychiatric_staff$pull_raw()
    california_psychiatric_staff$raw_data
    california_psychiatric_staff$save_raw()
    california_psychiatric_staff$restruct_raw()
    california_psychiatric_staff$restruct_data
    california_psychiatric_staff$extract_from_raw()
    california_psychiatric_staff$extract_data
    california_psychiatric_staff$validate_extract()
    california_psychiatric_staff$save_extract()
}

