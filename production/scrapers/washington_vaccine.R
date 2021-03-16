source("./R/generic_scraper.R")
source("./R/utilities.R")

washington_vaccine_pull <- function(x){
    xml2::read_html(x)
}

washington_vaccine_restruct <- function(x){
    x %>%
        rvest::html_nodes(., css = 'table')
}

washington_vaccine_extract <- function(x){
    rvest::html_table(x[1], fill = TRUE) %>% 
        as.data.frame() %>% 
        rename(
            "Name" = "Vaccine.Facility", 
            "Residents.Initiated" = "Initiating.Vaccination.Administered", 
            "Residents.Completed" = "Second.Vaccination.Administered", 
            "Total.Drop" = "Total"
        ) %>% 
        filter(!Name %in% c("All Vaccine Facilities")) %>% 
        select(-ends_with("Drop")) %>% 
        clean_scraped_df()
}

#' Scraper class for Washington vaccine data 
#' 
#' @name washington_vaccine_scraper
#' @description WA has a number of html tables on their vaccine page with both facility
#' specific and state-wide information. Currently only pulling data from the 
#' vaccine administration status by facility table. This table displays the total count 
#' of vaccines administered by the Department of Corrections to include an analysis 
#' of initiated vaccine counts (1st dose received) and second vaccine counts 
#' (2nd dose received) by facility. Vaccine facilities are approved by the Department 
#' of Health as vaccine storage sites. Vaccines given at Corrections' Headquarters 
#' (HQ) are included in Washington Corrections Center's count.
#'  
#' \describe{
#'   \item{Vaccine Facility}{The facility which stored the vaccine that was administered.}
#'   \item{Initiating Vaccination Administered}{The first dose administered in the two dose series}
#'   \item{Second Vaccination Administered}{The second dose administered in the two dose series}
#'   \item{Total}{}
#' }

washington_vaccine_scraper <- R6Class(
    "washington_vaccine_scraper",
    inherit = generic_scraper,
    public = list(
        log = NULL,
        initialize = function(
            log,
            url = "https://www.doc.wa.gov/corrections/covid-19/data-vaccines.htm",
            id = "washington",
            type = "html",
            state = "WA",
            jurisdiction = "state",
            # pull the JSON data directly from the API
            pull_func = washington_vaccine_pull,
            # restructuring the data means pulling out the data portion of the json
            restruct_func = washington_vaccine_restruct,
            # Rename the columns to appropriate database names
            extract_func = washington_vaccine_extract){
            super$initialize(
                url = url, id = id, pull_func = pull_func, type = type,
                restruct_func = restruct_func, extract_func = extract_func,
                log = log, state = state, jurisdiction = jurisdiction)
        }
    )
)

if(sys.nframe() == 0){
    washington_vaccine <- washington_vaccine_scraper$new(log=TRUE)
    washington_vaccine$raw_data
    washington_vaccine$pull_raw()
    washington_vaccine$raw_data
    washington_vaccine$save_raw()
    washington_vaccine$restruct_raw()
    washington_vaccine$restruct_data
    washington_vaccine$extract_from_raw()
    washington_vaccine$extract_data
    washington_vaccine$validate_extract()
    washington_vaccine$save_extract()
}
