source("./R/generic_scraper.R")
source("./R/utilities.R")

hennepin_county_pull <- function(x){
    xml2::read_html(x)
}

hennepin_county_restruct <- function(x){
    x %>%
        rvest::html_node("table") %>%
        rvest::html_table(header = TRUE)
}

hennepin_county_extract <- function(x){
    exp_names <- c(
        Residents.Population = "CURRENT POPULATION",
        Drop.Res.Iso = "MEDICALLY ISOLATED",
        Drop.Res.Quar = "QUARANTINE",
        Drop.Res.Active = "TOTAL POSITIVE IN CUSTODY",
        Residents.Confirmed = "TOTAL POSITIVE TO DATE",
        Residents.Pending = "PENDING",
        Residents.Negative = "CONFIRMED NEGATIVE",
        Residents.Tested = "TOTAL TESTED"
    )
    
    df_ <- x
    check_names(df_, exp_names)
    names(df_) <- names(exp_names)
    
    df_ %>%
        as_tibble() %>%
        mutate(Name = "Hennepin County Jail") %>%
        clean_scraped_df() %>%
        as_tibble() %>%
        mutate(Residents.Quarantine = Drop.Res.Quar + Drop.Res.Iso) %>%
        select(-starts_with("Drop"))
}

#' Scraper class for general hennepin_county COVID data
#' 
#' @name hennepin_county_scraper
#' @description Hennepin county reports data in an HTML table. Staff data is not
#' currently reported.
#' \describe{
#'   \item{CURRENT POPULATION}{Daily resident population}
#'   \item{MEDICALLY ISOLATED}{Medically isolated}
#'   \item{QUARANTINE}{Quarantined}
#'   \item{TOTAL POSITIVE IN CUSTODY}{Residents with active infection}
#'   \item{TOTAL POSITIVE TO DATE}{Cumulative confirmed}
#'   \item{PENDING}{Tests still awaiting results}
#'   \item{CONFIRMED NEGATIVE}{Cumulative negative}
#'   \item{TOTAL TESTED}{Total tests administered.}
#' }

hennepin_county_scraper <- R6Class(
    "hennepin_county_scraper",
    inherit = generic_scraper,
    public = list(
        log = NULL,
        initialize = function(
            log,
            url = "https://www.hennepinsheriff.org/jail-warrants/jail-information/COVID-19",
            id = "hennepin_county",
            type = "html",
            state = "MN",
            # pull the JSON data directly from the API
            pull_func = hennepin_county_pull,
            # restructuring the data means pulling out the data portion of the json
            restruct_func = hennepin_county_restruct,
            # Rename the columns to appropriate database names
            extract_func = hennepin_county_extract){
            super$initialize(
                url = url, id = id, pull_func = pull_func, type = type,
                restruct_func = restruct_func, extract_func = extract_func,
                log = log, state = state)
        }
    )
)

if(sys.nframe() == 0){
    hennepin_county <- hennepin_county_scraper$new(log=TRUE)
    hennepin_county$raw_data
    hennepin_county$pull_raw()
    hennepin_county$raw_data
    hennepin_county$save_raw()
    hennepin_county$restruct_raw()
    hennepin_county$restruct_data
    hennepin_county$extract_from_raw()
    hennepin_county$extract_data
    hennepin_county$validate_extract()
    hennepin_county$save_extract()
}

