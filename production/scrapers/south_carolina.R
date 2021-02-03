source("./R/generic_scraper.R")
source("./R/utilities.R")

south_carolina_pull <- function(x){
    xml2::read_html(x)
}

south_carolina_restruct <- function(x){

    data_tables <- x %>%
        rvest::html_nodes("table")

    list(
        rez_df = rvest::html_table(data_tables[[1]], header = TRUE),
        staff_df = rvest::html_table(data_tables[[2]], header = TRUE)
    )
}

south_carolina_extract <- function(x){

    staff_exp <- c(
        Name = "Assigned Locations", 
        Staff.Confirmed = "Staff", 
        Staff.Recovered = "Staff Cleared", 
        Staff.Drop = "Staff Active Cases",
        Staff.Deaths = "Staff Deaths")
    rez_exp <- c(
        Name = "Assigned Locations",
        Residents.Confirmed = "Offenders",
        Residents.Recovered = "Offenders Cleared",
        Residents.Active = "Offender Active Cases",
        Residents.Deaths = "Offender Deaths"
    )
    
    check_names(x$staff_df, staff_exp)
    check_names(x$rez_df, rez_exp)
    
    names(x$staff_df) <- names(staff_exp)
    names(x$rez_df) <- names(rez_exp)
    
    x$staff_df %>%
        mutate(Name = clean_fac_col_txt(Name)) %>%
        full_join(
            x$rez_df %>%
                mutate(Name = clean_fac_col_txt(Name)),
            by = "Name"
        ) %>%
        filter(!str_detect(Name, "Total")) %>%
        select(-ends_with("Drop"))
}

#' Scraper class for general south_carolina COVID data
#' 
#' @name south_carolina_scraper
#' @description SC data is pulled from two HTML tables one for staff and one
#' for residents. Minimal cleaning is necessary however, it should be noted
#' that at some point SC added columns for death so they may add or remove
#' columns at sometime in the future. 
#' \describe{
#'   \item{Assigned locations}{The facility name.}
#'   \item{Offenders}{Residents who are confirmed}
#'   \item{Offenders Cleared}{Residents who have been confirmed and recovered}
#'   \item{Offender Active Cases}{Residents with outstanding cases}
#'   \item{Offender Deaths}{Residents who have died because of COVID}
#'   \item{Staff}{Staff who are confirmed}
#'   \item{Staff Cleared}{Staff who have recovered}
#'   \item{Staff Active Cases}{Staff with outstanding cases}
#'   \item{Staff Deaths}{Staff who have died because of COVID}
#' }

south_carolina_scraper <- R6Class(
    "south_carolina_scraper",
    inherit = generic_scraper,
    public = list(
        log = NULL,
        initialize = function(
            log,
            url = "http://www.doc.sc.gov/covid.html",
            id = "south_carolina",
            type = "html",
            state = "SC",
            jurisdiction = "state",
            # pull the JSON data directly from the API
            pull_func = south_carolina_pull,
            # restructuring the data means pulling out the data portion of the json
            restruct_func = south_carolina_restruct,
            # Rename the columns to appropriate database names
            extract_func = south_carolina_extract){
            super$initialize(
                url = url, id = id, pull_func = pull_func, type = type,
                restruct_func = restruct_func, extract_func = extract_func,
                log = log, state = state, jurisdiction = jurisdiction)
        }
    )
)

if(sys.nframe() == 0){
    south_carolina <- south_carolina_scraper$new(log=TRUE)
    south_carolina$raw_data
    south_carolina$pull_raw()
    south_carolina$raw_data
    south_carolina$save_raw()
    south_carolina$restruct_raw()
    south_carolina$restruct_data
    south_carolina$extract_from_raw()
    south_carolina$extract_data
    south_carolina$validate_extract()
    south_carolina$save_extract()
}

