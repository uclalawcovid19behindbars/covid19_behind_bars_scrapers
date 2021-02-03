source("./R/generic_scraper.R")
source("./R/utilities.R")

oklahoma_manual_pull <- function(x){
    get_latest_manual("Oklahoma")
}

oklahoma_manual_restruct <- function(x){
    x %>%
        select(
            Name,
            `Inmate Deaths by Facility`,
            `Inmate Total Tested`,
            `Inmate Total Positive`, 
            `Inmate Total Currently Positive`,
            `Employee Total Tested`, 
            `Employee Total Positive`, 
            `Employee Total Currently Positive`
        )
}

oklahoma_manual_extract <- function(x){
    x %>%
        rename("Residents.Deaths" = "Inmate Deaths by Facility", 
               "Residents.Tadmin" = "Inmate Total Tested", 
               "Residents.Confirmed" = "Inmate Total Positive", 
               "Residents.Active" = "Inmate Total Currently Positive", 
               "Staff.Tadmin.Drop_" = "Employee Total Tested", 
               "Staff.Confirmed" = "Employee Total Positive", 
               "Staff.Active.Drop_" = "Employee Total Currently Positive") %>% 
        select(!ends_with(".Drop_")) %>% 
        clean_scraped_df() %>% 
        # To avoid double-counting, statewide deaths should be NA 
        # These are reported at the facility-level instead 
        # This should be reflected in the manual entry, so this is just to be safe  
        mutate(Residents.Deaths = ifelse(Name == "STATEWIDE", NA, Residents.Deaths))
}

#' Scraper class for Oklahoma COVID data
#' 
#' @name oklahoma_manual_scraper
#' @description Oklahoma's dashboard isn't machine-readable, so we manually extract 
#' the relevant information. 
#' \describe{
#'   \item{Name}{The facility name.}
#'   \item{Inmate Deaths by Facility}{Facility-level cumulative deaths}
#'   \item{Inmate Total Tested}{Statewide-level total tests administered}
#'   \item{Inmate Total Positive}{Statewide-level cumulative COVID cases for inmates}
#'   \item{Inmate Total Currently Positive}{Statewide-level current active COVID infections}
#'   \item{Employee Total Positive}{Statewide-level cumulative COVID cases for staff}

oklahoma_manual_scraper <- R6Class(
    "oklahoma_manual_scraper",
    inherit = generic_scraper,
    public = list(
        log = NULL,
        initialize = function(
            log,
            url = "https://oklahoma.gov/doc/covid-19-stats.html",
            id = "oklahoma_manual",
            type = "manual",
            state = "OK",
            jurisdiction = "state",
            # pull the JSON data directly from the API
            pull_func = oklahoma_manual_pull,
            # restructuring the data means pulling out the data portion of the json
            restruct_func = oklahoma_manual_restruct,
            # Rename the columns to appropriate database names
            extract_func = oklahoma_manual_extract){
            super$initialize(
                url = url, id = id, pull_func = pull_func, type = type,
                restruct_func = restruct_func, extract_func = extract_func,
                log = log, state = state, jurisdiction = jurisdiction)
        }
    )
)

if(sys.nframe() == 0){
    oklahoma_manual <- oklahoma_manual_scraper$new(log=TRUE)
    oklahoma_manual_scraper$raw_data
    oklahoma_manual_scraper$pull_raw()
    oklahoma_manual_scraper$raw_data
    oklahoma_manual_scraper$save_raw()
    oklahoma_manual_scraper$restruct_raw()
    oklahoma_manual_scraper$restruct_data
    oklahoma_manual_scraper$extract_from_raw()
    oklahoma_manual_scraper$extract_data
    oklahoma_manual_scraper$validate_extract()
    oklahoma_manual_scraper$save_extract()
}
