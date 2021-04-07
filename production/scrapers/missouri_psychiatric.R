source("./R/generic_scraper.R")
source("./R/utilities.R")

missouri_psychiatric_pull <- function(x){
    xml2::read_html(x)
}

missouri_psychiatric_restruct <- function(x){
    stop_defunct_scraper("https://dmh.mo.gov/disaster-services/covid-19-information/dmh-positive-cases-data")
}

missouri_psychiatric_extract <- function(x){
    NULL
}

#' Scraper class for general missouri_psychiatric COVID data
#' 
#' @name missouri_psychiatric_scraper
#' @description data is in a loosely structured format and will require some
#' serious checks to ensure the data is correctly being pulled.
#' \describe{
#'   \item{Facility name}{Name}
#'   \item{Patients testing positive}{Residents.Confirmed}
#'   \item{Staff testing positive}{Staff.Confirmed}
#'   \item{Patients Deaths}{Residents.Deaths}
#'   \item{Staff Deaths}{Staff.Deaths}
#' }

missouri_psychiatric_scraper <- R6Class(
    "missouri_psychiatric_scraper",
    inherit = generic_scraper,
    public = list(
        log = NULL,
        initialize = function(
            log,
            url = "https://dmh.mo.gov/disaster-services/covid-19-information/dmh-positive-cases-data",
            id = "missouri_psychiatric",
            type = "html",
            state = "MO",
            jurisdiction = "psychiatric",
            pull_func = missouri_psychiatric_pull,
            restruct_func = missouri_psychiatric_restruct,
            extract_func = missouri_psychiatric_extract){
            super$initialize(
                url = url, id = id, pull_func = pull_func, type = type,
                restruct_func = restruct_func, extract_func = extract_func,
                log = log, state = state, jurisdiction = jurisdiction)
        }
    )
)

if(sys.nframe() == 0){
    missouri_psychiatric <- missouri_psychiatric_scraper$new(log=TRUE)
    missouri_psychiatric$perma_save()
    missouri_psychiatric$raw_data
    missouri_psychiatric$pull_raw()
    missouri_psychiatric$raw_data
    missouri_psychiatric$save_raw()
    missouri_psychiatric$restruct_raw()
    missouri_psychiatric$restruct_data
    missouri_psychiatric$extract_from_raw()
    missouri_psychiatric$extract_data
    missouri_psychiatric$validate_extract()
    missouri_psychiatric$save_extract()
}

