source("./R/generic_scraper.R")
source("./R/utilities.R")

idaho_vaccine_pull <- function(x){
    xml2::read_html(x)
}

idaho_vaccine_restruct <- function(x){
    x %>%
        rvest::html_nodes(".covid-table") %>%
        lapply(rvest::html_table, header = TRUE)
}

idaho_vaccine_extract <- function(x){
    sw <- x[[4]]
    
    exp_names <- c(
        Name = "Facility",
        Residents.FirstDose = "1st dose only",
        Residents.Full = "Fully vaccinated*"
        # Drop_ = "", 
        # CRC_ = "Community Reentry Center",
        # CRC.Residents.FirstDose_ = "1st dose only",
        # CRC.Residents.Full_ = "Fully vaccinated*"
    )
    
    check_names(sw, exp_names)
    names(sw) <- names(exp_names)
    
        sw %>% 
        select(Name, Residents.FirstDose, Residents.Full) %>% 
        mutate(across(where(is.numeric), ~ ifelse(is.na(.), 0, .))) %>% 
        mutate(Residents.Initiated = Residents.FirstDose + Residents.Full,
               Residents.Completed = Residents.Full) %>%
        select(Name, Residents.Initiated, Residents.Completed) %>%
        filter(!str_detect(Name, "(?i)total")) %>% 
        filter(Name != "") %>% 
        {suppressWarnings(mutate_at(., vars(starts_with("Res")), as.numeric))} %>%
        clean_scraped_df() %>%
        as_tibble()
}

#' Scraper class for general Idaho COVID data
#' 
#' @name idaho_vaccine_scraper
#' @description ID reports a mix of facility and state-wide data across
#' various html tables within the page. Vaccine data is reported for facilities 
#' and community reentry centers. The table includes a note saying "The data in the 
#' table above indicate where the resident was at the time they received their 
#' vaccine dose, it may not reflect their current location." 
#' \describe{
#'   \item{Facility}{The facility name}
#'   \item{Community Reentry Center}{The community reentry center name}
#'   \item{Residents Initiated}{# of individuals that have received first dose of vaccine}
#'   \item{Residents Completed}{# of individuals that have received second dose of vaccine}
#' }

idaho_vaccine_scraper <- R6Class(
    "idaho_vaccine_scraper",
    inherit = generic_scraper,
    public = list(
        log = NULL,
        initialize = function(
            log,
            url = "https://www.idoc.idaho.gov/content/careers/covid-19",
            id = "idaho_vaccine",
            type = "html",
            state = "ID",
            jurisdiction = "state",
            # pull the JSON data directly from the API
            pull_func = idaho_vaccine_pull,
            # restructuring the data means pulling out the data portion of the json
            restruct_func = idaho_vaccine_restruct,
            # Rename the columns to appropriate database names
            extract_func = idaho_vaccine_extract){
            super$initialize(
                url = url, id = id, pull_func = pull_func, type = type,
                restruct_func = restruct_func, extract_func = extract_func,
                log = log, state = state, jurisdiction = jurisdiction)
        }
    )
)

if(sys.nframe() == 0){
    idaho_vaccine <- idaho_vaccine_scraper$new(log=T)
    idaho_vaccine$raw_data
    idaho_vaccine$pull_raw()
    idaho_vaccine$raw_data
    idaho_vaccine$save_raw()
    idaho_vaccine$restruct_raw()
    idaho_vaccine$restruct_data
    idaho_vaccine$extract_from_raw()
    idaho_vaccine$extract_data
    idaho_vaccine$validate_extract()
    idaho_vaccine$save_extract()
}
