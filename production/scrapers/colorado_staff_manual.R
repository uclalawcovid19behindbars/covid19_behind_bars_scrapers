source("./R/generic_scraper.R")
source("./R/utilities.R")

colorado_staff_manual_pull <- function(x){
    get_latest_manual("Colorado_Staff")
}

colorado_staff_manual_restruct <- function(x){
    x %>%
        select(Name, starts_with("Staff"), starts_with("Residents"))
}

colorado_staff_manual_extract <- function(x){
    x %>%
        {suppressWarnings(mutate_at(., vars(starts_with("Res")), as.numeric))} %>%
        {suppressWarnings(mutate_at(., vars(starts_with("Staff")), as.numeric))} %>%
        filter(!is.na(Name))
}

#' Scraper class for colorado_staff COVID data
#' 
#' @name colorado_staff_manual_scraper
#' @description colorado_staff's dashboard isn't machine-readable, so we manually
#' extract the relevant information from the second page of the dashboard
#' \describe{
#'   \item{Name}{The facility name.}
#'   \item{Staff Positive}{Staff Confirmed from the right most orange column}
#'   \item{Staff Vaccinations}{Total vaccinations given to staff}
#' }

colorado_staff_manual_scraper <- R6Class(
    "colorado_staff_manual_scraper",
    inherit = generic_scraper,
    public = list(
        log = NULL,
        initialize = function(
            log,
            url = "https://www.colorado.gov/pacific/cdoc/covid-19-faq-and-updates",
            id = "colorado_staff_manual",
            type = "manual",
            state = "CO",
            jurisdiction = "state",
            # pull the JSON data directly from the API
            pull_func = colorado_staff_manual_pull,
            # restructuring the data means pulling out the data portion of the json
            restruct_func = colorado_staff_manual_restruct,
            # Rename the columns to appropriate database names
            extract_func = colorado_staff_manual_extract){
            super$initialize(
                url = url, id = id, pull_func = pull_func, type = type,
                restruct_func = restruct_func, extract_func = extract_func,
                log = log, state = state, jurisdiction = jurisdiction)
        }
    )
)

if(sys.nframe() == 0){
    colorado_staff_manual <- colorado_staff_manual_scraper$new(log=TRUE)
    colorado_staff_manual$raw_data
    colorado_staff_manual$pull_raw()
    colorado_staff_manual$raw_data
    colorado_staff_manual$save_raw()
    colorado_staff_manual$restruct_raw()
    colorado_staff_manual$restruct_data
    colorado_staff_manual$extract_from_raw()
    colorado_staff_manual$extract_data
    colorado_staff_manual$validate_extract()
    colorado_staff_manual$save_extract()
}
