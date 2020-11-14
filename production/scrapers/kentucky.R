source("./R/generic_scraper.R")
source("./R/utilities.R")

kentucky_pull <- function(x){
    xml2::read_html(x)
}

kentucky_restruct <- function(x){
    x %>%
        rvest::html_node("table") %>%
        rvest::html_table(header = TRUE) %>%
        as_tibble()
}

kentucky_extract <- function(x){
    df_ <- x
    
    exp_names <- c(
        Name = "Institution", 
        Drop.Staff.Active = "Active Staff",
        Staff.Confirmed = "Total Staff",
        Staff.Deaths = "Total Staff Deaths",
        Residents.Active = "Active Inmates",
        Residents.Confirmed = "Total Inmates",
        Residents.Deaths = "Total Inmate Deaths"
        )
    
    check_names(df_, exp_names)
    names(df_) <- names(exp_names)
    
    df_ %>%
        mutate(Name = clean_fac_col_txt(Name)) %>%
        filter(!str_detect(Name, "(?i)total") & Name != "") %>%
        clean_scraped_df() %>%
        select(-starts_with("Drop"))
}

#' Scraper class for general Kentucky COVID data
#' 
#' @name kentucky_scraper
#' @description KY data is hosted on an image on the url but only contains
#' information about confirmed and deaths, no testing. More information about
#' transfers and recoveries is sometimes embedded in the text of the website
#' but it is inconsistent.
#' \describe{
#'   \item{Institution}{The facility name}
#'   \item{Staff}{staff confirmed}
#'   \item{Staff Deaths}{Staff deaths}
#'   \item{Inmates}{Residents confirmed}
#'   \item{Inmates Deaths}{Resident deaths}
#' }

kentucky_scraper <- R6Class(
    "kentucky_scraper",
    inherit = generic_scraper,
    public = list(
        log = NULL,
        initialize = function(
            log,
            url = "https://corrections.ky.gov/Facilities/Pages/covid19.aspx",
            id = "kentucky",
            type = "html",
            state = "KY",
            jurisdiction = "state",
            pull_func = kentucky_pull,
            restruct_func = kentucky_restruct,
            # Rename the columns to appropriate database names
            extract_func = kentucky_extract){
            super$initialize(
                url = url, id = id, pull_func = pull_func, type = type,
                restruct_func = restruct_func, extract_func = extract_func,
                log = log, state = state, jurisdiction = jurisdiction)
        }
    )
)

if(sys.nframe() == 0){
    kentucky <- kentucky_scraper$new(log=TRUE)
    kentucky$raw_data
    kentucky$pull_raw()
    kentucky$raw_data
    kentucky$save_raw()
    kentucky$restruct_raw()
    kentucky$restruct_data
    kentucky$extract_from_raw()
    kentucky$extract_data
    kentucky$validate_extract()
    kentucky$save_extract()
}

