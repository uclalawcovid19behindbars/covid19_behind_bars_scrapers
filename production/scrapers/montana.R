source("./R/generic_scraper.R")
source("./R/utilities.R")

montana_check_date <- function(x, date = Sys.Date()){
    base_html <- xml2::read_html(x)
    date_check_txt <- rvest::html_nodes(base_html, xpath = "//*[@id=\"content-wrapper\"]/main/div/div[2]/div/p[4]/em/text()") %>%
        rvest::html_text()

    date_check_txt %>%
        {.[str_detect(., "(?i)21")]} %>%
        str_split("Data last updated at") %>%
        unlist() %>%
        .[2] %>%
        str_split("on") %>%
        unlist() %>%
        .[2] %>%
        str_remove("\\.") %>%
        lubridate::mdy() %>%
        error_on_date(expected_date = date)
}

montana_pull <- function(x){
    xml2::read_html(x)
}

montana_restruct <- function(x){
    list(
        tables = x %>%
            rvest::html_nodes("table") %>%
            lapply(rvest::html_table, header = TRUE),
        death_text = x %>%
            rvest::html_node(
                xpath=str_c(
                    "//strong[contains(text(), 'Death')]/",
                    "parent::span/following::p")) %>%
            rvest::html_text()
    )
}

montana_extract <- function(x){
    
    exp_names1 <- c(
        Name = "Location",
        Residents.Confirmed = "Inmates Confirmed",
        Staff.Confirmed = "Staff Confirmed"
    )
    
    exp_names2 <- c(
        Name = "Locations",
        Residents.Confirmed = "Offenders Confirmed",
        Staff.Confirmed = "Staff Confirmed"
    )
    
    check_names(x$tables[[1]], exp_names1)
    check_names(x$tables[[2]], exp_names2)
    
    tab1 <- x$tables[[1]]
    tab2 <- x$tables[[2]]
    
    names(tab1) <- names(exp_names1)
    names(tab2) <- names(exp_names2)
    
    conf_df <- tab1 %>%
        filter(Name != "Secure - State") %>%
        clean_scraped_df() %>%
        bind_rows(clean_scraped_df(tab2)) %>%
        filter(Name != "Totals")
    
    if(!str_starts(x$death_text, "(?i)offend")){
        warning("Website structure is not as expected, please check scraper.")
    }
    
    dc <- x$death_text %>%
        str_remove_all(",") %>%
        str_extract_all("[0-9]+") %>%
        unlist() %>%
        as.numeric()
    
    bind_rows(
        conf_df,
        tibble(
            Name = "State-Wide", Residents.Deaths = dc[1], Staff.Deaths = dc[2])
    )
}

#' Scraper class for general montana COVID data
#' 
#' @name montana_scraper
#' @description MN records a cumulative account since June 1, 2020 for 
#' confirmed cases in contract facilities.
#' \describe{
#'   \item{Facility_Name}{The facility name.}
#'   \item{Residents Confirmed}{Residents cumulative confirmed}
#'   \item{Staff Confirmed}{Staff cumulative confirmed}
#'   \item{Residents Deaths}{State-wide only}
#'   \item{Staff Deaths}{State-wide only}
#' }

montana_scraper <- R6Class(
    "montana_scraper",
    inherit = generic_scraper,
    public = list(
        log = NULL,
        initialize = function(
            log,
            url = "https://cor.mt.gov/COVID-19",
            id = "montana",
            type = "html",
            state = "MT",
            jurisdiction = "state",
            check_date = montana_check_date, 
            pull_func = montana_pull,
            restruct_func = montana_restruct,
            # Rename the columns to appropriate database names
            extract_func = montana_extract){
            super$initialize(
                url = url, id = id, pull_func = pull_func, type = type,
                restruct_func = restruct_func, extract_func = extract_func,
                log = log, state = state, jurisdiction = jurisdiction,
                check_date = check_date)
        }
    )
)

if(sys.nframe() == 0){
    montana <- montana_scraper$new(log=TRUE)
    montana$run_check_date()
    montana$raw_data
    montana$pull_raw()
    montana$raw_data
    montana$save_raw()
    montana$restruct_raw()
    montana$restruct_data
    montana$extract_from_raw()
    montana$extract_data
    montana$validate_extract()
    montana$save_extract()
}

