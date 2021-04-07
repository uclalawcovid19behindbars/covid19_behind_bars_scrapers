source("./R/generic_scraper.R")
source("./R/utilities.R")

pennsylvania_psychiatric_pull <- function(x){
    xml2::read_html(x)
}

pennsylvania_psychiatric_restruct <- function(x){
    stop_defunct_scraper("https://www.dhs.pa.gov/providers/Providers/Pages/Coronavirus-State-Facility-Data.aspx")
}

pennsylvania_psychiatric_extract <- function(x){
    NULL
}

#' Scraper class for general pennsylvania_psychiatric COVID data
#' 
#' @name pennsylvania_psychiatric_scraper
#' @description There are multiple html tables on this page however we are
#' interested in the table titled State Operated Psychiatric Hospitals. Row
#' names here indicate state psychiatric facilities and we want the columns
#' below
#' \describe{
#'   \item{Facility name}{The facility name.}
#'   \item{Patients testing positive}{Residents.Confirmed}
#'   \item{Staff testing positive}{Staff.Confirmed}
#' }

pennsylvania_psychiatric_scraper <- R6Class(
    "pennsylvania_psychiatric_scraper",
    inherit = generic_scraper,
    public = list(
        log = NULL,
        initialize = function(
            log,
            url = "https://www.dhs.pa.gov/providers/Providers/Pages/Coronavirus-State-Facility-Data.aspx",
            id = "pennsylvania_psychiatric",
            type = "html",
            state = "PA",
            jurisdiction = "psychiatric",
            pull_func = pennsylvania_psychiatric_pull,
            restruct_func = pennsylvania_psychiatric_restruct,
            extract_func = pennsylvania_psychiatric_extract){
            super$initialize(
                url = url, id = id, pull_func = pull_func, type = type,
                restruct_func = restruct_func, extract_func = extract_func,
                log = log, state = state, jurisdiction = jurisdiction)
        }
    )
)

if(sys.nframe() == 0){
    pennsylvania_psychiatric <- pennsylvania_psychiatric_scraper$new(log=TRUE)
    pennsylvania_psychiatric$perma_save()
    pennsylvania_psychiatric$raw_data
    pennsylvania_psychiatric$pull_raw()
    pennsylvania_psychiatric$raw_data
    pennsylvania_psychiatric$save_raw()
    pennsylvania_psychiatric$restruct_raw()
    pennsylvania_psychiatric$restruct_data
    pennsylvania_psychiatric$extract_from_raw()
    pennsylvania_psychiatric$extract_data
    pennsylvania_psychiatric$validate_extract()
    pennsylvania_psychiatric$save_extract()
}

