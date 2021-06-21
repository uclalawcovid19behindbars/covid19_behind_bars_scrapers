source("./R/generic_scraper.R")
source("./R/utilities.R")

santa_barbara_jails_pull <- function(x){
    xml2::read_html(x)
}

santa_barbara_jails_restruct <- function(x){
    x %>% 
        rvest::html_nodes("table") %>%
        .[[1]] %>% 
        rvest::html_table(header= FALSE) %>% 
        slice(-1) # Table had two headers (title and then colnames), removing the title header
}

santa_barbara_jails_extract <- function(x){
    
    x_ <- x %>% 
        t() %>% 
        janitor::row_to_names(row_number = 1) %>% 
        as.data.frame() 
    
    exp_names <- c(
        "V1", 
        "Active Cases Medically Monitored/ Treated", 
        "Recovered", 
        "Released from Custody", 
        "Deceased", 
        "TOTAL"
    )
    basic_check(names(x_), exp_names)
    
    names(x_) <- c(
        "Metric.Drop", 
        "Residents.Active",  
        "Residents.Recovered", 
        "Released.Drop", 
        "Residents.Deaths", 
        "Residents.Confirmed"
    )
    
    x_ %>% 
        as_tibble() %>% 
        filter(Metric.Drop == "Total Cases") %>% 
        mutate(Name = "Santa Barbara Jails") %>% 
        select(!ends_with(".Drop")) %>% 
        clean_scraped_df()
}

#' Scraper class for general santa_barbara_jails COVID data
#' 
#' @name santa_barbara_jails_scraper
#' @description Santa Barbara jails reports data on a number of covid variables
#' in an html table. The names used in the table correspond to a number of our
#' collected variables below.
#' \describe{
#'   \item{Facility_Name}{Santa Barbara Jails}
#'   \item{Active Cases}{Residents.Active}
#'   \item{Recovered Cases}{Residents.Recovered}
#'   \item{Released Cases}{Number of indviduals released who had covid, we dont use this}
#'   \item{Deceased}{Residents.Deaths}
#'   \item{Total}{Residents.Confirmed}
#' }

santa_barbara_jails_scraper <- R6Class(
    "santa_barbara_jails_scraper",
    inherit = generic_scraper,
    public = list(
        log = NULL,
        initialize = function(
            log,
            url = "https://www.sbsheriff.org/covid-update/",
            id = "santa_barbara_jails",
            type = "html",
            state = "CA",
            jurisdiction = "county",
            check_date = NULL,
            pull_func = santa_barbara_jails_pull,
            restruct_func = santa_barbara_jails_restruct,
            # Rename the columns to appropriate database names
            extract_func = santa_barbara_jails_extract){
            super$initialize(
                url = url, id = id, pull_func = pull_func, type = type,
                restruct_func = restruct_func, extract_func = extract_func,
                log = log, state = state, jurisdiction = jurisdiction,
                check_date = check_date)
        }
    )
)

if(sys.nframe() == 0){
    santa_barbara_jails <- santa_barbara_jails_scraper$new(log=TRUE)
    santa_barbara_jails$run_check_date()
    santa_barbara_jails$raw_data
    santa_barbara_jails$pull_raw()
    santa_barbara_jails$raw_data
    santa_barbara_jails$save_raw()
    santa_barbara_jails$restruct_raw()
    santa_barbara_jails$restruct_data
    santa_barbara_jails$extract_from_raw()
    santa_barbara_jails$extract_data 
    santa_barbara_jails$validate_extract()
    santa_barbara_jails$save_extract()
}

