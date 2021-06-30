source("./R/generic_scraper.R")
source("./R/utilities.R")

pennsylvania_psychiatric_check_date <- function(x, date = Sys.Date()){
    base_html <- xml2::read_html(x)
    date_txt <- rvest::html_nodes(base_html, 
                                  xpath="//*[@id=\"ctl00_PlaceHolderMain_PageContent__ControlWrapper_RichHtmlField\"]/p[14]/span/em") %>%
        rvest::html_text()
    
    date_txt %>%
        {.[str_detect(., "(?i)21")]} %>%
        str_split(., "(?i)Updated: |at") %>%
        unlist() %>%
        .[2] %>%
        lubridate::mdy() %>%
        error_on_date(expected_date = date)
}


pennsylvania_psychiatric_pull <- function(x){
    xml2::read_html(x)
}

pennsylvania_psychiatric_restruct <- function(x){
    table1 <- x %>% 
        rvest::html_nodes("table") %>%
        .[[2]] %>% 
        rvest::html_table(header= TRUE)
    
    exp_names <- c(
        "State Hospital", "Current Census of Clients",
        "Current Positive Cases Among Clients",
        "Cumulative Positive Cases Among Clients", "Deaths of Clients",
        "Current Census of Staff", "Current Positive Cases Among Staff",
        "Cumulative Positive Cases Among Staff"
    )
    
    check_names(table1, exp_names)

    table1 <- table1[-c(7)] 
    table1[table1 == "Less than 5"] <- NA
    
    colnames(table1) <- c(
        "Name", "Residents.Population", "Residents.Active", "Residents.Confirmed",
        "Residents.Deaths", "Staff.Population", "Staff.Confirmed")
    table1[,2:7] <- sapply(table1[,2:7], as.numeric)
    
    table1
}

pennsylvania_psychiatric_extract <- function(x){
    x
}

#' Scraper class for general pennsylvania_psychiatric COVID data
#' 
#' @name pennsylvania_psychiatric_scraper
#' @description There are multiple html tables on this page however we are
#' interested in the table under the title State Hospitals. Row
#' names here indicate state psychiatric facilities and we want the columns
#' below. Note whenever we see "less than 5" we want to convert the value to NA
#' and make sure those columns are numeric.
#' \describe{
#'   \item{State Hospital}{Name}
#'   \item{Current Census of Clients}{Residents.Population}
#'   \item{Current Positive Cases Among Clients}{Residents.Active}
#'   \item{Cumulative Positive Cases Among Clients}{Residents.Confirmed}
#'   \item{Deaths of Clients}{Residents.Deaths}
#'   \item{Current Census of Staff}{Staff.Population}
#'   \item{Staff testing positive}{Staff.Confirmed}
#' }

pennsylvania_psychiatric_scraper <- R6Class(
    "pennsylvania_psychiatric_scraper",
    inherit = generic_scraper,
    public = list(
        log = NULL,
        initialize = function(
            log,
            url = "https://www.dhs.pa.gov/providers/Providers/Pages/Coronavirus-State-Facility-Data.aspx",
            id = "pennsylvania_psychiatric",
            type = "html",
            state = "PA",
            jurisdiction = "psychiatric",
            check_date = pennsylvania_psychiatric_check_date,
            pull_func = pennsylvania_psychiatric_pull,
            restruct_func = pennsylvania_psychiatric_restruct,
            extract_func = pennsylvania_psychiatric_extract){
            super$initialize(
                url = url, id = id, pull_func = pull_func, type = type,
                restruct_func = restruct_func, extract_func = extract_func,
                log = log, state = state, jurisdiction = jurisdiction,
                check_date = check_date)
        }
    )
)

if(sys.nframe() == 0){
    pennsylvania_psychiatric <- pennsylvania_psychiatric_scraper$new(log=TRUE)
    pennsylvania_psychiatric$run_check_date()
    pennsylvania_psychiatric$perma_save()
    pennsylvania_psychiatric$raw_data
    pennsylvania_psychiatric$pull_raw()
    pennsylvania_psychiatric$raw_data
    pennsylvania_psychiatric$save_raw()
    pennsylvania_psychiatric$restruct_raw()
    pennsylvania_psychiatric$restruct_data
    pennsylvania_psychiatric$extract_from_raw()
    pennsylvania_psychiatric$extract_data
    pennsylvania_psychiatric$validate_extract()
    pennsylvania_psychiatric$save_extract()
}

