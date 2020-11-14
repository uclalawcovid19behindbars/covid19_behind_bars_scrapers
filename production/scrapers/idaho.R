source("./R/generic_scraper.R")
source("./R/utilities.R")

idaho_pull <- function(x){
    xml2::read_html(x)
}

idaho_restruct <- function(x){
    x %>%
        rvest::html_nodes(".covid-table") %>%
        lapply(rvest::html_table, header = TRUE)
}

idaho_extract <- function(x){
    sw <- x[[1]][1,]
    
    exp_names <- c(
        Residents.Tested = "Tested",
        Residents.Pending = "Pending",
        Residents.Negative = "Negative",
        Drop.Residents.Active = "Positive",
        Drop.Residents.Asymp = "Asymptomatic Positive",
        Residents.Confirmed = "Inactive",
        Residents.Deaths = "Deaths"
    )
    
    check_names(sw, exp_names)
    names(sw) <- names(exp_names)
    
    sw %>%
        mutate_all(as.numeric) %>%
        mutate(Name = "State-Wide") %>%
        mutate(Residents.Confirmed = 
                   Residents.Confirmed + Residents.Deaths + 
                   Drop.Residents.Asymp + Drop.Residents.Active) %>%
        mutate(Residents.Active = 
                   Drop.Residents.Asymp + Drop.Residents.Active) %>%
        bind_rows(
            x[[6]] %>%
                rename(Name = Location) %>%
                clean_scraped_df() %>%
                mutate(Positive = ifelse(is.na(Positive), 0, Positive)) %>%
                mutate(Inactive = ifelse(is.na(Inactive), 0, Inactive)) %>%
                mutate(Staff.Confirmed = Positive + Inactive) %>%
                mutate(Staff.Recovered = Inactive) %>%
                select(-Positive, -Inactive)
        ) %>%
        clean_scraped_df() %>%
        select(-starts_with("Drop")) %>%
        as_tibble()
}

#' Scraper class for general Idaho COVID data
#' 
#' @name idaho_scraper
#' @description ID reports a mix of facility and state-wide data across
#' various html tables within the page. Note that residents who leave the
#' facility are considered inactive if they tested positive in the past.
#' \describe{
#'   \item{Facility_Name}{The facility name}
#'   \item{Staff Positive}{Staff Active}
#'   \item{Staff Inactive}{Staff Recovered}
#'   \item{Residents Tested}{state-wide only, not sure if administered or individuals tested}
#'   \item{Residents Pending}{state-wide only}
#'   \item{Residents Negative}{state-wide only}
#'   \item{Residents Positive}{state-wide only}
#'   \item{Residents Asymptomatic Positive}{state-wide only}
#'   \item{Residents Inactive}{state-wide only}
#'   \item{Residents Deaths}{state-wide only}
#' }

idaho_scraper <- R6Class(
    "idaho_scraper",
    inherit = generic_scraper,
    public = list(
        log = NULL,
        initialize = function(
            log,
            url = "https://www.idoc.idaho.gov/content/careers/covid-19",
            id = "idaho",
            type = "html",
            state = "ID",
            jurisdiction = "state",
            # pull the JSON data directly from the API
            pull_func = idaho_pull,
            # restructuring the data means pulling out the data portion of the json
            restruct_func = idaho_restruct,
            # Rename the columns to appropriate database names
            extract_func = idaho_extract){
            super$initialize(
                url = url, id = id, pull_func = pull_func, type = type,
                restruct_func = restruct_func, extract_func = extract_func,
                log = log, state = state, jurisdiction = jurisdiction)
        }
    )
)

if(sys.nframe() == 0){
    idaho <- idaho_scraper$new(log=T)
    idaho$raw_data
    idaho$pull_raw()
    idaho$raw_data
    idaho$save_raw()
    idaho$restruct_raw()
    idaho$restruct_data
    idaho$extract_from_raw()
    idaho$extract_data
    idaho$validate_extract()
    idaho$save_extract()
}

