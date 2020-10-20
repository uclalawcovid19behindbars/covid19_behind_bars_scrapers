source("./R/generic_scraper.R")
source("./R/utilities.R")

virginia_pull <- function(x){
    xml2::read_html(x)
}

virginia_restruct <- function(x){
    x %>%
        rvest::html_node("table") %>%
        rvest::html_table() %>%
        as_tibble()
}

virginia_extract <- function(x){
    exp_names <- c(
        Name = "Location",
        Drop.Active1 = "Offenders on-site",
        Drop.Active2 = "Offenders in hospitals",
        Residents.Deaths = "Death of COVID-19 positive offender",
        Residents.Confirmed = "Total positive offendersonsite + hospital + deaths + releases + recovered + transfers in - transfers out",
        Drop.Active.Staff = "Staff active cases including employees & contractors"
    )
    
    check_names(x, exp_names)
    df_ <- x
    names(df_) <- names(exp_names)
    
    df_ %>%
        clean_scraped_df() %>%
        select(-starts_with("Drop"))
}

#' Scraper class for general Virginia COVID data
#' 
#' @name virginia_scraper
#' @description VA cumulative confirmed does not account for transfers out as
#' listed in the table. data is pulled from the html table directly
#' \describe{
#'   \item{Location}{The facility name.}
#'   \item{Offenders on site}{Infected and at facility}
#'   \item{Offenders in hospital}{Currently hospitalized}
#'   \item{Death of COVID-19 positive offender}{Residents deaths}
#'   \item{Total positive offenders on site}{No transfers but cumulative}
#'   \item{Staff active}{Staff currently infected}
#' }

virginia_scraper <- R6Class(
    "virginia_scraper",
    inherit = generic_scraper,
    public = list(
        log = NULL,
        initialize = function(
            log,
            url = "https://vadoc.virginia.gov/news-press-releases/2020/covid-19-updates/",
            id = "virginia",
            type = "html",
            state = "VA",
            # pull the JSON data directly from the API
            pull_func = virginia_pull,
            # restructuring the data means pulling out the data portion of the json
            restruct_func = virginia_restruct,
            # Rename the columns to appropriate database names
            extract_func = virginia_extract){
            super$initialize(
                url = url, id = id, pull_func = pull_func, type = type,
                restruct_func = restruct_func, extract_func = extract_func,
                log = log, state = state)
        }
    )
)

if(sys.nframe() == 0){
    virginia <- virginia_scraper$new(log=TRUE)
    virginia$raw_data
    virginia$pull_raw()
    virginia$raw_data
    virginia$save_raw()
    virginia$restruct_raw()
    virginia$restruct_data
    virginia$extract_from_raw()
    virginia$extract_data
    virginia$validate_extract()
    virginia$save_extract()
}

