source("./R/generic_scraper.R")
source("./R/utilities.R")

south_carolina_pull <- function(x){
    xml2::read_html(x)
}

get_sc_table <- function(tables, headers, idx) {
    t <- rvest::html_table(tables[[idx]], header = FALSE)
    n <- read.table(text = headers[idx], sep = "\r", strip.white = T) %>% 
        unlist() %>% 
        as.character() %>% 
        clean_fac_col_txt()
    
    names(t) <- n
    
    return(t)
}
    

south_carolina_restruct <- function(x){
    tables <- x %>%
        rvest::html_nodes("table")
    
    headers <- x %>% 
        rvest::html_nodes("thead") %>% 
        rvest::html_text() 
    
    if(length(tables) != length(headers) | length(tables) != 3){
        stop(str_c("Number of tables different from the expected.", 
                   "Check if page structure changed.")
        )
    }
    
    list(
        rez_df = get_sc_table(tables, headers, 1), 
        vaccine_df = get_sc_table(tables, headers, 2), 
        staff_df = get_sc_table(tables, headers, 3)
    )
}

south_carolina_extract <- function(x){

    staff_exp <- c(
        Name = "Assigned Locations", 
        Staff.Confirmed = "Staff", 
        Staff.Recovered = "Staff Cleared", 
        Staff.Active = "Staff Active Cases",
        Staff.Deaths = "Staff Deaths"
    )
    rez_exp <- c(
        Name = "Assigned Locations",
        Residents.Confirmed = "Offenders",
        Residents.Recovered = "Offenders Cleared",
        Residents.Active = "Offender Active Cases",
        Residents.Deaths = "Offender Deaths"
    )
    vaccine_exp <- c(
        Name = "Assigned Location", 
        Residents.Completed = "Completed Vaccinations", 
        Declined.Drop = "Declined"
    )
    
    check_names(x$staff_df, staff_exp)
    check_names(x$rez_df, rez_exp)
    check_names(x$vaccine_df, vaccine_exp)
    
    names(x$staff_df) <- names(staff_exp)
    names(x$rez_df) <- names(rez_exp)
    names(x$vaccine_df) <- names(vaccine_exp)
    
    x$staff_df %>%
        mutate(Name = clean_fac_col_txt(Name)) %>%
        full_join(
            x$rez_df %>%
                mutate(Name = clean_fac_col_txt(Name)),
            by = "Name"
        ) %>%
        full_join(
            x$vaccine_df %>% 
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
#'   \item{Completed Vaccinations}{}
#'   \item{Declined Vaccinations}{}
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
            check_date = NULL,
            # pull the JSON data directly from the API
            pull_func = south_carolina_pull,
            # restructuring the data means pulling out the data portion of the json
            restruct_func = south_carolina_restruct,
            # Rename the columns to appropriate database names
            extract_func = south_carolina_extract){
            super$initialize(
                url = url, id = id, pull_func = pull_func, type = type,
                restruct_func = restruct_func, extract_func = extract_func,
                log = log, state = state, jurisdiction = jurisdiction,
                check_date = check_date)
        }
    )
)

if(sys.nframe() == 0){
    south_carolina <- south_carolina_scraper$new(log=TRUE)
    south_carolina$run_check_date()
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

