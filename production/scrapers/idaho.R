source("./R/generic_scraper.R")
source("./R/utilities.R")

idaho_date_check <- function(x, date = Sys.Date()){
    base_html <- xml2::read_html(x)
    
    base_html %>%
        rvest::html_nodes("p") %>%
        rvest::html_text() %>% 
        {.[which(str_detect(., "current as of"))]} %>% 
        str_extract("\\d{1,2}/\\d{1,2}/\\d{2,4}") %>% 
        lubridate::mdy() %>%
        error_on_date(date)
}

idaho_pull <- function(x){
    xml2::read_html(x)
}

idaho_restruct <- function(x){
    x %>%
        rvest::html_nodes("table") %>%
        lapply(rvest::html_table, header = TRUE)
}

idaho_extract <- function(x){
    sw <- x[[1]][1,]
    
    exp_names <- c(
        Residents.Tadmin = "Tested",
        Residents.Pending = "Pending",
        Residents.Negative = "Negative",
        Drop.Residents.Active = "Positive",
        Drop.Residents.Asymp = "Asymptomatic\n\t\t\tPositive *",
        Residents.Recovered = "Inactive",
        Residents.Deaths = "Deaths"
    )
    
    check_names(sw, exp_names)
    names(sw) <- names(exp_names)
    
    nlist <- lapply(x, names)
    
    sw %>%
        mutate_all(as.numeric) %>%
        mutate(Name = "State-Wide") %>%
        mutate(Residents.Confirmed = 
                   Residents.Recovered + Residents.Deaths + 
                   Drop.Residents.Asymp + Drop.Residents.Active) %>%
        mutate(Residents.Active = 
                   Drop.Residents.Asymp + Drop.Residents.Active) %>%
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
#'   \item{Residents Tested}{state-wide only, tests administered}
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
            check_date = idaho_date_check,
            # pull the JSON data directly from the API
            pull_func = idaho_pull,
            # restructuring the data means pulling out the data portion of the json
            restruct_func = idaho_restruct,
            # Rename the columns to appropriate database names
            extract_func = idaho_extract){
            super$initialize(
                url = url, id = id, pull_func = pull_func, type = type,
                restruct_func = restruct_func, extract_func = extract_func,
                log = log, state = state, jurisdiction = jurisdiction,
                check_date = check_date)
        }
    )
)

if(sys.nframe() == 0){
    idaho <- idaho_scraper$new(log=T)
    idaho$run_check_date()
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

