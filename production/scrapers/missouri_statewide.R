source("./R/generic_scraper.R")
source("./R/utilities.R")

missouri_statewide_pull <- function(x){
    xml2::read_html(x)
}

missouri_statewide_restruct <- function(x){
    Residents.Tested <- x %>%
        rvest::html_node(
            xpath="//h3[contains(text(),'Tests')]/following::ul") %>%
        rvest::html_text() %>%
        string_to_clean_numeric()
    
    all_deaths <- x %>%
        rvest::html_node(
            xpath="//h3[contains(text(),'Deaths')]/following::ul") %>%
        rvest::html_nodes("li") %>%
        rvest::html_text()
    
    if(!(grepl("Offender", all_deaths[1]) & grepl("Staff", all_deaths[2]))){
        warning("Web data for residents and staff not formatted as expected")
    }
    
    
    Staff.Deaths <- str_split(all_deaths[2], ":")[[1]] %>%
        last() %>%
        string_to_clean_numeric()
    Residents.Deaths <- str_split(all_deaths[1], ":")[[1]] %>%
        last() %>%
        string_to_clean_numeric()
    Name <- "State-wide"
    MOSW <- data.frame(Name, Staff.Deaths, Residents.Deaths, Residents.Tested)
    
    
    
    # Non Facility Row
    NameNF <- "Non-Facility"
    Staff.Confirmed <- x %>%
        rvest::html_node(xpath="//h3[contains(text(),'Cumulative')]") %>%
        rvest::html_text() %>%
        str_split(":") %>%
        .[[1]] %>%
        last() %>%
        string_to_clean_numeric()
    
    Staff.Recovered <- x %>%
        rvest::html_node(
            xpath="//h3[contains(text(),'Cumulative')]/following::ul") %>%
        rvest::html_node(xpath="//li[contains(text(),'Recovered')]") %>%
        rvest::html_text() %>%
        str_split(":") %>%
        .[[1]] %>%
        last() %>%
        string_to_clean_numeric()

    MO.NF <- data.frame(Name = NameNF, Staff.Confirmed, Staff.Recovered)
    
    for(i in 1:ncol(MOSW)){
        if(is.na(MOSW[,i])){
            warning(str_c(
                "Extracted NA value where not expected: ", names(MOSW)[i]))
        }
    }
    for(i in 1:ncol(MO.NF)){
        if(is.na(MO.NF[,i])){
            warning(str_c(
                "Extracted NA value where not expected: ", names(MO.NF)[i]))
        }
    }
    
    dplyr::bind_rows(MOSW, MO.NF)
}

missouri_statewide_extract <- function(x){
    clean_scraped_df(x)
}

#' Scraper class for general missouri_statewide COVID data
#' 
#' @name missouri_statewide_scraper
#' @description State wide MO data comes directly from html on page.
#' Provides more detail than facility specific scraper.Also includes
#' non-facility information for staff.
#' \describe{
#'   \item{Active Inmate Cases}{Active Inmate Cases}
#'   \item{Active Prison Staff Cases}{Active Prison Staff Cases}
#'   \item{Inmate Tests Performed}{Staff deaths related to COVID}
#'   \item{Deaths offenders}{Resident deaths related to COVID}
#'   \item{Non-Prison Staff Cumulative Cases}{NPS cumulative infections}
#'   \item{Non-Prison Staff Active}{NPS active}
#'   \item{Non-Prison Staff Recovered}{NPS recovered}
#' }

missouri_statewide_scraper <- R6Class(
    "missouri_statewide_scraper",
    inherit = generic_scraper,
    public = list(
        log = NULL,
        initialize = function(
            log,
            url = "https://doc.mo.gov/media-center/newsroom/covid-19/data",
            id = "missouri_statewide",
            type = "html",
            state = "MO",
            jurisdiction = "state",
            # pull the JSON data directly from the API
            pull_func = missouri_statewide_pull,
            restruct_func = missouri_statewide_restruct,
            # Rename the columns to appropriate database names
            extract_func = missouri_statewide_extract){
            super$initialize(
                url = url, id = id, pull_func = pull_func, type = type,
                restruct_func = restruct_func, extract_func = extract_func,
                log = log, state = state, jurisdiction = jurisdiction)
        }
    )
)

if(sys.nframe() == 0){
    missouri_statewide <- missouri_statewide_scraper$new(log=TRUE)
    missouri_statewide$raw_data
    missouri_statewide$pull_raw()
    missouri_statewide$raw_data
    missouri_statewide$save_raw()
    missouri_statewide$restruct_raw()
    missouri_statewide$restruct_data
    missouri_statewide$extract_from_raw()
    missouri_statewide$extract_data
    missouri_statewide$validate_extract()
    missouri_statewide$save_extract()
}

