source("./R/generic_scraper.R")
source("./R/utilities.R")

minnesota_vaccine_date_check <- function(x, date = Sys.Date(), wait = 10){
    app_source <- "https://app.smartsheet.com/b/publish?EQBCT=4fffc0afb455414da7680411f796b64c"
    
    remDr <- RSelenium::remoteDriver(
        remoteServerAddr = "localhost",
        port = 4445,
        browserName = "firefox"
    )
    
    del_ <- capture.output(remDr$open())
    remDr$navigate(app_source)
    Sys.sleep(wait)
    
    base_html <- xml2::read_html(remDr$getPageSource()[[1]])
    
    base_html %>% 
        rvest::html_elements("p") %>% 
        rvest::html_text() %>% 
        {.[str_detect(., "(?i)as of")]} %>% 
        str_extract("\\d{1,2}/\\d{1,2}/\\d{2,4}") %>% 
        lubridate::mdy() %>% 
        error_on_date(date)
}

minnesota_vaccine_pull <- function(x){
    "1VhAAbzipvheVRG0UWKMLT6mCVQRMdV98lUUkk-PCYtQ" %>%
        googlesheets4::read_sheet(sheet = "MN Vaccine", 
                                  col_types = "Dcccccccc")
}

minnesota_vaccine_restruct <- function(x){
    x %>%
        filter(!is.na(Date)) %>% 
        filter(Date == max(Date))
}

minnesota_vaccine_extract <- function(x, exp_date = Sys.Date()){
    
    error_on_date(first(x$Date), exp_date)
    
    check_names(x, c(
        "Date", 
        "Facility", 
        "Staff Received 1st Dose Only", 
        "Staff Completed DOC Vaccine Process", 
        "Staff Started External Vaccination Process", 
        "Staff Completed External Vaccination Process", 
        "Client Received 1st Dose", 
        "Client Received 2nd Dose", 
        "Client Received J&J Vaccine"
    ))
    
    x %>%
        {suppressWarnings(mutate_at(., vars(starts_with("Staff")), as.numeric))} %>%
        {suppressWarnings(mutate_at(., vars(starts_with("Client")), as.numeric))} %>%
        mutate(across(where(is.numeric), ~ ifelse(is.na(.), 0, .))) %>%
        mutate(
            Residents.Initiated = `Client Received 1st Dose` + `Client Received J&J Vaccine`, 
            Residents.Completed = `Client Received 2nd Dose` + `Client Received J&J Vaccine`, 
            Staff.Initiated = `Staff Received 1st Dose Only` + `Staff Started External Vaccination Process`, 
            Staff.Completed = `Staff Completed DOC Vaccine Process` + `Staff Completed External Vaccination Process`, 
            Staff.Initiated = Staff.Initiated + Staff.Completed, 
            Residents.Vadmin = `Client Received 1st Dose` + `Client Received 2nd Dose` + `Client Received J&J Vaccine`
            # Can't compute Staff.Vadmin bc we don't know if completed vaccinations 
            # were one-shot or two 
        ) %>% 
        select(
            Name = Facility,
            Residents.Initiated,
            Residents.Completed,
            Residents.Vadmin, 
            Staff.Initiated, 
            Staff.Completed
        ) %>% 
        # Remove non-DOC so numerator and denominator universes match  
        filter(!str_detect(Name, "(?i)non-doc")) %>% 
        clean_scraped_df()
}

#' Scraper class for Minnesota vaccine data 
#' 
#' @name minnesota_vaccine_scraper
#' @description Grabs manually entered Google Sheet data for MN vaccines. J&J 
#' vaccine doses are included in both the initiated and completed totals. 
#' 
#' \describe{
#'   \item{Facility}{The facilty name}
#'   \item{Staff Received 1st Dose Only}
#'   \item{Staff Completed DOC Vaccine Process}
#'   \item{Staff Started External Vaccination Process}
#'   \item{Client Received 1st Dose}
#'   \item{Client Received 2nd Dose}
#'   \item{Client Received J&J Vaccine}
#' }

minnesota_vaccine_scraper <- R6Class(
    "minnesota_vaccine_scraper",
    inherit = generic_scraper,
    public = list(
        log = NULL,
        initialize = function(
            log,
            url = "https://mn.gov/doc/about/covid-19-updates/",
            id = "minnesota_vaccine",
            type = "manual",
            state = "MN",
            jurisdiction = "state",
            check_date = minnesota_vaccine_date_check,
            # pull the JSON data directly from the API
            pull_func = minnesota_vaccine_pull,
            # restructuring the data means pulling out the data portion of the json
            restruct_func = minnesota_vaccine_restruct,
            # Rename the columns to appropriate database names
            extract_func = minnesota_vaccine_extract){
            super$initialize(
                url = url, id = id, pull_func = pull_func, type = type,
                restruct_func = restruct_func, extract_func = extract_func,
                log = log, state = state, jurisdiction = jurisdiction,
                check_date = check_date)
        }
    )
)

if(sys.nframe() == 0){
    minnesota_vaccine <- minnesota_vaccine_scraper$new(log=TRUE)
    minnesota_vaccine$run_check_date()
    minnesota_vaccine$raw_data
    minnesota_vaccine$pull_raw()
    minnesota_vaccine$raw_data
    minnesota_vaccine$save_raw()
    minnesota_vaccine$restruct_raw()
    minnesota_vaccine$restruct_data
    minnesota_vaccine$extract_from_raw()
    minnesota_vaccine$extract_data
    minnesota_vaccine$validate_extract()
    minnesota_vaccine$save_extract()
}