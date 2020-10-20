source("./R/generic_scraper.R")
source("./R/utilities.R")

massachusetts_pull <- function(x){
    tf <- tempfile(fileext = ".xlsx")
    
    "https://docs.google.com/spreadsheets/d/" %>%
        str_c("1nmZ84rjOxQgdTL0PdV7SrbyDTbD7nROQ/export#gid=1419540291") %>%
        httr::GET(httr::write_disk(tf))
    
    readxl::read_excel(tf, sheet="DOC Facilities")
}

massachusetts_restruct <- function(x){
    # get latest date for each facility
    x 
}

massachusetts_extract <- function(x){
    x %>%
        arrange(`DOC Facility`, Date) %>%
        select(
            Name = "DOC Facility",
            Residents.Population = "Total Population",
            Residents.Tested = "N Tested - Detainees/Inmates",
            Residents.Confirmed = "N Positive - Detainees/Inmates",
            Staff.Tested = "N Tested - Staff",
            Staff.Confirmed = "N Positive - Staff") %>%
        clean_scraped_df() %>%
        group_by(Name) %>%
        mutate(Residents.Population = last(Residents.Population)) %>%
        group_by(Name, Residents.Population) %>%
        summarize_all(sum, na.rm = T) %>%
        ungroup()
}

#' Scraper class for general Massachusetts COVID data
#' 
#' @name massachusetts_scraper
#' @description This will be a description of massachusetts data and what the scraper
#' does
#' \describe{
#'   \item{Facility_Name}{The facility name.}
#' }

massachusetts_scraper <- R6Class(
    "massachusetts_scraper",
    inherit = generic_scraper,
    public = list(
        log = NULL,
        initialize = function(
            log,
            url = "http://url/goes/here",
            id = "massachusetts",
            type = "pdf",
            state = "",
            # pull the JSON data directly from the API
            pull_func = massachusetts_pull,
            # restructuring the data means pulling out the data portion of the json
            restruct_func = massachusetts_restruct,
            # Rename the columns to appropriate database names
            extract_func = massachusetts_extract){
            super$initialize(
                url = url, id = id, pull_func = pull_func, type = type,
                restruct_func = restruct_func, extract_func = extract_func,
                log = log, state = state)
        }
    )
)

if(sys.nframe() == 0){
    massachusetts <- massachusetts_scraper$new(log=TRUE)
    massachusetts$raw_data
    massachusetts$pull_raw()
    massachusetts$raw_data
    massachusetts$save_raw()
    massachusetts$restruct_raw()
    massachusetts$restruct_data
    massachusetts$extract_from_raw()
    massachusetts$extract_data
    massachusetts$validate_extract()
    massachusetts$save_extract()
}

