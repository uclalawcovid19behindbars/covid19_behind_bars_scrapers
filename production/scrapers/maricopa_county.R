source("./R/generic_scraper.R")
source("R/utilities.R")

maricopa_county_restruct <- function(x){
    df_ <- x %>%
        rvest::html_nodes(".dataNumber") %>%
        rvest::html_text() %>%
        parse_number() %>%
        t() %>%
        as.data.frame()
    
    names(df_) <- x %>%
        rvest::html_nodes(".dataBox") %>%
        rvest::html_text() %>%
        str_replace_all('(?<=\\d),(?=\\d)', '') %>%
        str_remove_all("[0-9]+$") %>%
        str_squish()
    
    expect_names <- c(
        "Total Number of Tests and Re-tests" = "Residents.Tadmin",
        "Positive Results" = "Residents.Confirmed",
        "Active Cases In Custody" = "Residents.Active",
        "Hospitalizations" = "Drop3",
        "Deaths" = "Residents.Deaths"
    )
    
    check_names(df_, names(expect_names))
    
    names(df_) <- expect_names
    
    as_tibble(df_)
}

maricopa_county_extract <- function(x){
    
    # In 2021, Maricopa County reset its reported data to the start of 2021. 
    # To get cumulative data, we have to add the totals from 2020
    # Raw source: http://104.131.72.50:3838/scraper_data/raw_files/2021-01-05_maricopa_county.html 
    
    Residents.Tadmin_2020 <- 23131
    Residents.Confirmed_2020 <- 1875
    Residents.Deaths_2020 <- 0 

    x %>%
        select(-starts_with("Drop")) %>%
        mutate(Name = "Maricopa County Jail", 
               Residents.Tadmin = Residents.Tadmin + Residents.Tadmin_2020, 
               Residents.Confirmed = Residents.Confirmed + Residents.Confirmed_2020, 
               Residents.Deaths = Residents.Deaths + Residents.Deaths_2020) 
    
}

#' Scraper class for general Maricopa County COVID data
#' 
#' @name maricopa_county_scraper
#' @description County jail data scraper pulls data from special elements of
#' class dataNumber. 
#' \describe{
#'   \item{Total Number of Tests and Re-tests}{}
#'   \item{Negative Results}{}
#'   \item{Positive Results}{}
#'   \item{Pending Results & in Med Obs/Med Iso}{}
#'   \item{Active Cases In Custody}{}
#'   \item{Recovered CasesIn Custody}{}
#'   \item{Released Cases}{}
#'   \item{Hospitalizations}{}
#'   \item{Deaths}{}
#' }

maricopa_county_scraper <- R6Class(
    "maricopa_county_scraper",
    inherit = generic_scraper,
    public = list(
        log = NULL,
        initialize = function(
            log,
            url = "https://www.maricopa.gov/5574/COVID-19-in-County-Jails",
            id = "maricopa_county",
            type = "html",
            state = "AZ",
            jurisdiction = "county",
            # pull the JSON data directly from the API
            pull_func = xml2::read_html,
            # restructuring the data
            restruct_func = maricopa_county_restruct,
            # Rename the columns to appropriate database names
            extract_func = maricopa_county_extract){
            super$initialize(
                url = url, id = id, pull_func = pull_func, type = type,
                restruct_func = restruct_func, extract_func = extract_func,
                log = log, state = state, jurisdiction = jurisdiction)
        }
    )
)

if(sys.nframe() == 0){
    maricopa_county <- maricopa_county_scraper$new(log=TRUE)
    maricopa_county$raw_data
    maricopa_county$pull_raw()
    maricopa_county$raw_data
    maricopa_county$save_raw()
    maricopa_county$restruct_raw()
    maricopa_county$restruct_data
    maricopa_county$extract_from_raw()
    maricopa_county$extract_data
    maricopa_county$validate_extract()
}

