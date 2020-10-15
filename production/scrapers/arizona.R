source("./R/generic_scraper.R")
source("./R/utilities.R")

arizona_pull <- function(x){
    xml2::read_html(x)
}

arizona_restruct <- function(x){

    bind_rows(
        x %>%
            rvest::html_nodes('table') %>%
            .[[2]] %>%
            rvest::html_table() %>%
            plyr::rename(c("Location" = "Name", 
                           "Inmates Tested" = "Residents.Tested",
                           "Inmates Negative" = "Residents.Negative",
                           "Inmates Confirmed" = "Residents.Confirmed",
                           "Inmates Pending" = "Residents.Pending",
                           "Inmates Recovered" = "Residents.Recovered",
                           "Daily Total Population" = "Resident.Population",
                           "Inmates Confirmed Deaths" = "Resident.Deaths1",
                           "Inmates Potential Deaths" = "Resident.Deaths2"
            )),

        x %>%
            rvest::html_nodes('table') %>%
            .[[3]] %>%
            rvest::html_table() %>%
            plyr::rename(c("Self-Reported Staff Positive" = "Staff.Confirmed",
                           "Staff Certified Recovered" = "Staff.Recovered")) %>%
            mutate(Name = "State-Wide")) %>%
        clean_scraped_df()
}

arizona_extract <- function(x){
    x %>%
        mutate(Residents.Deaths = Resident.Deaths1 + Resident.Deaths2) %>%
        select(-Resident.Deaths1, -Resident.Deaths2, -Resident.Population)
}

#' Scraper class for general Arizona COVID data
#' 
#' @name arizona_scraper
#' @description HTML table scraping of semi structured data. The information
#' that we care about are located in the html tables located at position 2 for 
#' residents and 3 for staff. Should be checked regularly for changing format.
#' \describe{
#'   \item{Location}{The facility name.}
#'   \item{Inmates Tested}{Likely cummulative number of inmates tested.}
#'   \item{Inmates Negative}{Likely cummulative number of inmates negative.}
#'   \item{Inmates Confirmed}{Likely cummulative number of inmates confirmed.}
#'   \item{Inmates Pending}{Current number of inmates pending.}
#'   \item{Inmates Recovered}{Likely cummulative number of inmates recovered.}
#'   \item{Daily Total Population}{Resdient population.}
#'   \item{Inmates Confirmed Deaths}{Resident death occured w/confirmed status.}
#'   \item{Inmates Potential Deaths}{Resident death but no confirmed status.}
#' }

arizona_scraper <- R6Class(
    "arizona_scraper",
    inherit = generic_scraper,
    public = list(
        log = NULL,
        initialize = function(
            log,
            url = str_c(
                "https://corrections.az.gov/adcrr-covid-19-dashboard?",
                "order=title_field&sort=asc"),
            id = "arizona",
            type = "html",
            state = "AZ",
            # pull the JSON data directly from the API
            pull_func = arizona_pull,
            # restructuring the data means pulling out the data portion of the json
            restruct_func = arizona_restruct,
            # Rename the columns to appropriate database names
            extract_func = arizona_extract){
            super$initialize(
                url = url, id = id, pull_func = pull_func, type = type,
                restruct_func = restruct_func, extract_func = extract_func,
                log = log, state = state)
        }
    )
)

if(sys.nframe() == 0){
    arizona <- arizona_scraper$new(log=TRUE)
    arizona$raw_data
    arizona$pull_raw()
    arizona$raw_data
    arizona$restruct_raw()
    arizona$restruct_data
    arizona$extract_from_raw()
    arizona$extract_data
    arizona$validate_extract()
    arizona$save_extract()
}

