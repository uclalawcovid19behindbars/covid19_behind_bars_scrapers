source("./R/generic_scraper.R")
source("./R/utilities.R")

st_luce_county_pull <- function(x){
    xml2::read_html(x)
}

st_luce_county_restruct <- function(x){
    x %>%
        rvest::html_node("table") %>%
        rvest::html_table() %>%
        t() %>%
        as.data.frame()
}

st_luce_county_extract <- function(x){

    exp_names <- c(
        Residents.Population = "Current Jail Population",
        Residents.Tested = "Inmates Tested",
        Drop.Residents.Active = "Current Positive Cases",
        Drop.Residents.Hospitalized = "Inmate Hospitalizations"
    )

    df_ <- x[2,]
    names(df_) <- x[1,]
    check_names(df_, exp_names)

    names(df_) <- names(exp_names)

    df_ %>%
        as_tibble() %>%
        mutate(Name = "St. Luce County Jail") %>%
        clean_scraped_df() %>%
        select(-starts_with("Drop"))
}

#' Scraper class for general St Luce county COVID data
#' 
#' @name st_luce_county_scraper
#' @description St Luce county reports information in an html table. What has
#' gone in to this table has changed over time.
#' \describe{
#'   \item{Current Jail Population}{}
#'   \item{Inmates Tested}{}
#'   \item{Current Positive Cases}{}
#'   \item{Inmate Hospitalizations}{}
#' }

st_luce_county_scraper <- R6Class(
    "st_luce_county_scraper",
    inherit = generic_scraper,
    public = list(
        log = NULL,
        initialize = function(
            log,
            url = "https://www.stluciesheriff.com/288/COVID-19-Information",
            id = "st_luce_county",
            type = "html",
            state = "FL",
            jurisdiction = "county",
            check_date = NULL, 
            # pull the JSON data directly from the API
            pull_func = st_luce_county_pull,
            # restructuring the data means pulling out the data portion of the json
            restruct_func = st_luce_county_restruct,
            # Rename the columns to appropriate database names
            extract_func = st_luce_county_extract){
            super$initialize(
                url = url, id = id, pull_func = pull_func, type = type,
                restruct_func = restruct_func, extract_func = extract_func,
                log = log, state = state, jurisdiction = jurisdiction,
                check_date = check_date)
        }
    )
)

if(sys.nframe() == 0){
    st_luce_county <- st_luce_county_scraper$new(log=TRUE)
    st_luce_county$run_check_date()
    st_luce_county$raw_data
    st_luce_county$pull_raw()
    st_luce_county$raw_data
    st_luce_county$save_raw()
    st_luce_county$restruct_raw()
    st_luce_county$restruct_data
    st_luce_county$extract_from_raw()
    st_luce_county$extract_data
    st_luce_county$validate_extract()
    st_luce_county$save_extract()
}

