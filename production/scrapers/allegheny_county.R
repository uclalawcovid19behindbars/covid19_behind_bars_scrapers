source("./R/generic_scraper.R")
source("./R/utilities.R")

allegheny_county_check_date <- function(x, date = Sys.Date()){
    base_page <- xml2::read_html(x)
    
    site_date <- base_page %>%
        rvest::html_node("table") %>%
        rvest::html_table(header = T) %>%
        names() %>%
        .[1] %>%
        str_extract_all("\\([^()]+\\)") %>%
        unlist() %>%
        str_remove_all("\\(|\\)") %>%
        str_remove("(?i)as of ") %>%
        lubridate::mdy()

    error_on_date(site_date, date)
}

allegheny_county_pull <- function(x){
    xml2::read_html(x)
}

allegheny_county_restruct <- function(x){
    tabs <- x %>%
        rvest::html_nodes("table") %>%
        .[1:2] %>%
        rvest::html_table()
    
    if(!any(str_detect(tabs[[1]][,1], "(?i)inmate"))){
        warning("Website structure may have changed. Please check.")
    }
    if(!any(str_detect(tabs[[2]][,1], "(?i)staff"))){
        warning("Website structure may have changed. Please check.")
    }

    tabs %>%
        lapply(function(y){
            z <- y
            names(z) <- clean_fac_col_txt(unname(unlist(z[2,])))
            z[3,] %>%
                .[,2:ncol(.)] %>%
                as_tibble()
        })
}

allegheny_county_extract <- function(x){
    exp_names_rez <- c(
        Residents.Tested = "Tested",
        Residents.Confirmed = "Positive",
        Residents.Negative = "Negative",
        Residents.Pending = "Pending",
        Drop.Active = "Number ofCOVID-19PositiveInmatesin Facility",
        Residents.Recovered = "Released/Recovered",
        Drop.Hosp = "Number of COVID-19RelatedHospitalizations"
    )
    
    exp_names_staff <- c(
        Staff.Tested = "Tested",
        Staff.Confirmed = "Positive",
        Staff.Negative = "Negative",
        Staff.Pending = "Pending",
        Staff.Recovered = "Recovered"
    )
    
    rez_df <- x[[1]]
    staff_df <- x[[2]]
    check_names(rez_df, exp_names_rez)
    check_names(staff_df, exp_names_staff)
    names(rez_df) <- names(exp_names_rez)
    names(staff_df) <- names(exp_names_staff)
    
    bind_cols(rez_df, staff_df) %>%
        select(-starts_with("Drop")) %>%
        mutate(Name = "Allegheny County Jail") %>%
        clean_scraped_df()
}

#' Scraper class for general allegheny_county COVID data
#' 
#' @name allegheny_county_scraper
#' @description Allegheny data comes from an html tables which reports data on
#' numerous statistics. First table is for residents second is for staff.
#' \describe{
#'   \item{Residents Tested}{}
#'   \item{Residents Positive}{}
#'   \item{Residents Negative}{}
#'   \item{Residents Pending}{}
#'   \item{Residents Active}{}
#'   \item{Residents Recovered}{}
#'   \item{Residents Hospitalized}{}
#'   \item{Staff Tested}{}
#'   \item{Staff Positive}{}
#'   \item{Staff Negative}{}
#'   \item{Staff Pending}{}
#'   \item{Staff Recovered}{}
#' }

allegheny_county_scraper <- R6Class(
    "allegheny_county_scraper",
    inherit = generic_scraper,
    public = list(
        log = NULL,
        initialize = function(
            log,
            url = "https://www.alleghenycounty.us/jail/index.aspx",
            id = "allegheny_county",
            type = "html",
            state = "PA",
            jurisdiction = "county",
            check_date = allegheny_county_check_date,
            # pull the JSON data directly from the API
            pull_func = allegheny_county_pull,
            # restructuring the data means pulling out the data portion of the json
            restruct_func = allegheny_county_restruct,
            # Rename the columns to appropriate database names
            extract_func = allegheny_county_extract){
            super$initialize(
                url = url, id = id, pull_func = pull_func, type = type,
                restruct_func = restruct_func, extract_func = extract_func,
                log = log, state = state, jurisdiction = jurisdiction,
                check_date = check_date)
        }
    )
)

if(sys.nframe() == 0){
    allegheny_county <- allegheny_county_scraper$new(log=TRUE)
    allegheny_county$raw_data
    allegheny_county$pull_raw()
    allegheny_county$raw_data
    allegheny_county$save_raw()
    allegheny_county$restruct_raw()
    allegheny_county$restruct_data
    allegheny_county$extract_from_raw()
    allegheny_county$extract_data
    allegheny_county$validate_extract()
    allegheny_county$save_extract()
}

