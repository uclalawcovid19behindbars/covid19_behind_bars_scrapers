source("./R/generic_scraper.R")
source("./R/utilities.R")

colorado_staff_check_date <- function(x, date = Sys.Date()){
    "1VhAAbzipvheVRG0UWKMLT6mCVQRMdV98lUUkk-PCYtQ" %>%
        googlesheets4::read_sheet(sheet = "CO Staff", col_types = "Dccc") %>%
        pull(Date) %>%
        max(na.rm = TRUE) %>%
        error_on_date(date)
}

colorado_staff_manual_pull <- function(x){
    "1VhAAbzipvheVRG0UWKMLT6mCVQRMdV98lUUkk-PCYtQ" %>%
        googlesheets4::read_sheet(sheet = "CO Staff", 
                                  col_types = "Dccc")
}

colorado_staff_manual_restruct <- function(x){
    x %>%
        filter(!is.na(Date)) %>% 
        filter(Date == max(Date))
}

colorado_staff_manual_extract <- function(x, exp_date = Sys.Date()){
    
    error_on_date(first(x$Date), exp_date)
    
    check_names(x, c(
        "Date", 
        "Name", 
        "Staff Positive Cases", 
        "Staff Vaccinations")
    )
    
    x %>%
        select(
            Name = `Name`,
            Staff.Confirmed = `Staff Positive Cases`,
            Staff.Vadmin = `Staff Vaccinations`) %>% 
        {suppressWarnings(mutate_at(., vars(starts_with("Res")), as.numeric))} %>%
        {suppressWarnings(mutate_at(., vars(starts_with("Staff")), as.numeric))} %>%
        clean_scraped_df()
}

#' Scraper class for Colorado staff COVID data
#' 
#' @name colorado_staff_manual_scraper
#' @description Colorado staff's dashboard isn't machine-readable, so we manually
#' extract the relevant information from the second page of the dashboard. 
#' \describe{
#'   \item{Name}{The facility name.}
#'   \item{Staff Positive}{Staff Confirmed from the right most orange column.}
#'   \item{Staff Vaccinations}{Total vaccinations given to staff.}
#' }

colorado_staff_manual_scraper <- R6Class(
    "colorado_staff_manual_scraper",
    inherit = generic_scraper,
    public = list(
        log = NULL,
        initialize = function(
            log,
            url = "https://www.colorado.gov/pacific/cdoc/covid-19-faq-and-updates",
            id = "colorado_staff_manual",
            type = "manual",
            state = "CO",
            jurisdiction = "state",
            check_date = colorado_staff_check_date,
            # pull the JSON data directly from the API
            pull_func = colorado_staff_manual_pull,
            # restructuring the data means pulling out the data portion of the json
            restruct_func = colorado_staff_manual_restruct,
            # Rename the columns to appropriate database names
            extract_func = colorado_staff_manual_extract){
            super$initialize(
                url = url, id = id, pull_func = pull_func, type = type,
                restruct_func = restruct_func, extract_func = extract_func,
                log = log, state = state, jurisdiction = jurisdiction,
                check_date = check_date)
        }
    )
)

if(sys.nframe() == 0){
    colorado_staff_manual <- colorado_staff_manual_scraper$new(log=TRUE)
    colorado_staff_manual$raw_data
    colorado_staff_manual$pull_raw()
    colorado_staff_manual$raw_data
    colorado_staff_manual$save_raw()
    colorado_staff_manual$restruct_raw()
    colorado_staff_manual$restruct_data
    colorado_staff_manual$extract_from_raw()
    colorado_staff_manual$extract_data
    colorado_staff_manual$validate_extract()
    colorado_staff_manual$save_extract()
}
