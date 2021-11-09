source("./R/generic_scraper.R")
source("./R/utilities.R")

massachusetts_check_date <- function(x, date = Sys.Date(), wait = 10){
    app_src <- "https://data.aclum.org/sjc-12926-tracker/"
    
    remDr <- RSelenium::remoteDriver(
        remoteServerAddr = "localhost",
        port = 4445,
        browserName = "firefox"
    )
    
    del_ <- capture.output(remDr$open())
    remDr$navigate(app_src)
    
    Sys.sleep(wait)
    
    base_html <- remDr$getPageSource()
    
    xml2::read_html(base_html[[1]]) %>% 
        rvest::html_nodes("#last_date_str3_DOC") %>% 
        rvest::html_text() %>% 
        lubridate::mdy() %>% 
        error_on_date(date)
}

massachusetts_pull <- function(x){
    tf <- tempfile(fileext = ".xlsx")
    
    # get data from sheet directly
    "https://docs.google.com/spreadsheets/d/" %>%
        str_c("1nmZ84rjOxQgdTL0PdV7SrbyDTbD7nROQ/export#gid=1419540291") %>%
        httr::GET(httr::write_disk(tf))
    
    readxl::read_excel(tf, sheet="DOC Facilities")
}

massachusetts_restruct <- function(x){
    x 
}

massachusetts_extract <- function(x){
    if(all(c("N Tested - COs", "N Positive - COs") %in% names(x))){
        z <- x %>%
            arrange(`DOC Facility`, Date) %>%
            select(
                Name = "DOC Facility",
                Residents.Population = "Total Population",
                Residents.Active = "Active Prisoner Cases",
                Residents.Tadmin = "N Tested - Detainees/Inmates",
                Residents.Confirmed = "N Positive - Detainees/Inmates",
                Residents.Deaths = "N Deaths",
                Staff.Tested = "N Tested - COs",
                Staff.Confirmed = "N Positive - COs")
    }
    else{
       z <-  x %>%
            arrange(`DOC Facility`, Date) %>%
            select(
                Name = "DOC Facility",
                Residents.Population = "Total Population",
                Residents.Active = "Active Prisoner Cases",
                Residents.Tadmin = "N Tested - Detainees/Inmates",
                Residents.Confirmed = "N Positive - Detainees/Inmates",
                Residents.Deaths = "N Deaths")
    }
    
    z %>%
        clean_scraped_df() %>%
        group_by(Name) %>%
        mutate(Residents.Population = last(Residents.Population)) %>%
        mutate(Residents.Active = last(Residents.Active)) %>%
        group_by(Name, Residents.Population, Residents.Active) %>%
        summarize_all(sum, na.rm = T) %>%
        ungroup()
}

#' Scraper class for general Massachusetts COVID data
#' 
#' @name massachusetts_scraper
#' @description Massachusetts data comes from an xlsx file that is updated
#' weekly. Currently a full time series is available but it is not exactly clear
#' which numbers are cumulative. We consistently get tested values that are
#' less than confirmed.
#' \describe{
#'   \item{DOC Facility}{The facility name}
#'   \item{Date}{}
#'   \item{Total Population}{}
#'   \item{N Tested - Detainees/Inmates}{}
#'   \item{N Positive - Detainees/Inmates}{}
#'   \item{N Tested - Staff}{}
#'   \item{N Positive - Staff}{}
#'   \item{Total Tested}{}
#'   \item{Total Positive}{}
#'   \item{Active Prisoner Cases}{}
#'   \item{N Deaths}{}
#'   \item{N Released}{}
#'   \item{Notes}{} 
#' }

massachusetts_scraper <- R6Class(
    "massachusetts_scraper",
    inherit = generic_scraper,
    public = list(
        log = NULL,
        initialize = function(
            log,
            url = "https://data.aclum.org/sjc-12926-tracker/",
            id = "massachusetts",
            type = "csv",
            state = "MA",
            jurisdiction = "state",
            check_date = massachusetts_check_date,
            # pull the JSON data directly from the API
            pull_func = massachusetts_pull,
            # restructuring the data means pulling out the data portion of the json
            restruct_func = massachusetts_restruct,
            # Rename the columns to appropriate database names
            extract_func = massachusetts_extract){
            super$initialize(
                url = url, id = id, pull_func = pull_func, type = type,
                restruct_func = restruct_func, extract_func = extract_func,
                log = log, state = state, jurisdiction = jurisdiction, 
                check_date = check_date)
        }
    )
)

if(sys.nframe() == 0){
    massachusetts <- massachusetts_scraper$new(log=TRUE)
    massachusetts$run_check_date()
    massachusetts$raw_data
    massachusetts$pull_raw()
    massachusetts$raw_data
    massachusetts$save_raw()
    massachusetts$restruct_raw()
    massachusetts$restruct_data
    massachusetts$extract_from_raw()
    massachusetts$extract_data
    massachusetts$validate_extract()
    massachusetts$save_extract()
}

