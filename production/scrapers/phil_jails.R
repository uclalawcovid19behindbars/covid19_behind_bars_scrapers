source("./R/generic_scraper.R")
source("./R/utilities.R")

phil_jails_pull <- function(x){
    "https://api.phila.gov/inmate-locator/census/covid?" %>%
        str_c("gatekeeperKey=ad0050d3c6e40064546a18af371f7826") %>%
        jsonlite::read_json(simplifyVector = TRUE)
}

phil_jails_restruct <- function(x){
    as_tibble(x)
}

phil_jails_extract <- function(x){
    x %>%
        mutate(Residents.Population = totalDailyPop) %>%
        mutate(Residents.Active = currentSymtomaticCase +
                   currentAsymtomaticCase) %>%
        mutate(Residents.Confirmed = totalSymtomaticCase +
                   totalAsymtomaticCase) %>%
        select(Residents.Population, Residents.Active, Residents.Confirmed) %>%
        mutate(Name = "Philadelphia Dept of Jails")
}

#' Scraper class for general phil_jails COVID data
#' 
#' @name phil_jails_scraper
#' @description Philadelphia data jail is sparse but is conveniently reported
#' in an api.
#' \describe{
#'   \item{totalDailyPop}{}
#'   \item{currentSymtomaticCase}{}
#'   \item{totalSymtomaticCase}{}
#'   \item{currentAsymtomaticCase}{}
#'   \item{totalAsymtomaticCase}{}
#'   \item{reportDate}{}
#' }

phil_jails_scraper <- R6Class(
    "phil_jails_scraper",
    inherit = generic_scraper,
    public = list(
        log = NULL,
        initialize = function(
            log,
            url = "https://www.phila.gov/programs/coronavirus-disease-2019-covid-19/testing-and-data/#/philadelphia-prisons-covid-19-data",
            id = "phil_jails",
            type = "json",
            state = "PA",
            jurisdiction = "county",
            check_date = NULL,
            # pull the JSON data directly from the API
            pull_func = phil_jails_pull,
            # restructuring the data means pulling out the data portion of the json
            restruct_func = phil_jails_restruct,
            # Rename the columns to appropriate database names
            extract_func = phil_jails_extract){
            super$initialize(
                url = url, id = id, pull_func = pull_func, type = type,
                restruct_func = restruct_func, extract_func = extract_func,
                log = log, state = state, jurisdiction = jurisdiction,
                check_date = check_date)
        }
    )
)

if(sys.nframe() == 0){
    phil_jails <- phil_jails_scraper$new(log=TRUE)
    phil_jails$run_check_date()
    phil_jails$raw_data
    phil_jails$pull_raw()
    phil_jails$raw_data
    phil_jails$save_raw()
    phil_jails$restruct_raw()
    phil_jails$restruct_data
    phil_jails$extract_from_raw()
    phil_jails$extract_data
    phil_jails$validate_extract()
    phil_jails$save_extract()
}

