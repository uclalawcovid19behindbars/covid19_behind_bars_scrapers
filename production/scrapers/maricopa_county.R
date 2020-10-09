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
        "Total Number of Tests and Re-tests" = "Residents.Tested",
        "Negative Results" = "Residents.Negative",
        "Positive Results" = "Residents.Confirmed",
        "Pending Results &in Med Obs/Med Iso" = "Residents.Pending", 
        "Active Cases In Custody" = "Drop1",
        "Recovered CasesIn Custody" = "Residents.Recovered",
        "Released Cases" = "Drop2", 
        "Hospitalizations" = "Drop3", 
        "Deaths" = "Residents.Deaths"
    )
    
    check_names(df_, names(expect_names))
    
    names(df_) <- expect_names
    
    as_tibble(df_)
}

maricopa_county_extract <- function(x){
    x %>%
        select(-starts_with("Drop")) %>%
        mutate(Name = "Maricopa County Jail")
    
}

#' Scraper class for general Maricopa County COVID data
#' 
#' @name maricopa_county_scraper
#' @description This will be a description of Maricopa County data and what the
#' scraper does
#' \describe{
#'   \item{Facility_Name}{The faciilty name.}
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
            # pull the JSON data directly from the API
            pull_func = xml2::read_html,
            # restructuring the data
            restruct_func = maricopa_county_restruct,
            # Rename the columns to appropriate database names
            extract_func = maricopa_county_extract){
            super$initialize(
                url = url, id = id, pull_func = pull_func, type = type,
                restruct_func = restruct_func, extract_func = extract_func,
                log = log, state = state)
        }
    )
)

if(sys.nframe() == 0){
    maricopa_county <- maricopa_county_scraper$new(log=FALSE)
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

