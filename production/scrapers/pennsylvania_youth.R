source("./R/generic_scraper.R")
source("./R/utilities.R")

pennsylvania_youth_check_date <- function(x, date = Sys.Date()){
    raw_html <- xml2::read_html(x)
    
    update_text <- raw_html %>%
        rvest::html_nodes("em") %>%
        rvest::html_text() %>%
        .[str_detect(., "(?i)update")]
    
    if(length(update_text) != 1){
        stop("Update text is not as expected please inspect")
    }
    
    update_text %>%
        str_split(":") %>%
        unlist() %>%
        .[2] %>%
        str_split("at") %>%
        unlist() %>%
        first() %>%
        lubridate::mdy() %>%
        error_on_date(expected_date = date)
}

pennsylvania_youth_pull <- function(x){
    xml2::read_html(x)
}

pennsylvania_youth_restruct <- function(x){
    x %>%
        rvest::html_nodes('table') %>%
        .[[3]] %>%
        rvest::html_table()
}

pennsylvania_youth_extract <- function(x){
    semi_cln <- x %>%
        janitor::row_to_names(row_number = 1)
    
    check_names(semi_cln,
                c("YDC/YFC",
                  "Current Census of Youth",
                  "Current Positive Cases Among Youth",
                  "Cumulative Positive Cases Among Youth",
                  "Deaths of Youth",
                  "Current Census of Staff",
                  "Current Positive Cases Among Staff",
                  "Cumulative Positive Cases Among Staff"
                  ))
    
    clean <- semi_cln %>%
        select(Name = `YDC/YFC`, 
               Residents.Population = `Current Census of Youth`,
               Residents.Active = `Current Positive Cases Among Youth`,
               Residents.Confirmed = `Cumulative Positive Cases Among Youth`,
               Residents.Deaths = `Deaths of Youth`,
               Staff.Confirmed = `Cumulative Positive Cases Among Staff`
        ) %>%
        mutate(Name = str_c(toupper(Name), " YOUTH"))
    
    out <- clean %>%
        clean_scraped_df() %>%
        as_tibble()
    
    return(out)
}

#' Scraper class for general Pennsylvania Youth COVID data
#' 
#' @name pennsylvania_youth_scraper
#' @description PA data self contained within html table under juvenile justice services header
#' \describe{
#'   \item{Location}{The facilty name}
#'   \item{Staff.Confirmed}{Staff Confirmed}
#'   \item{Staff.Recovered}{Staff Recovered}
#'   \item{Residents.Confirmed}{Residents Confirmed}
#'   \item{Residents.Recovered}{Residents Recovered}
#' }

pennsylvania_youth_scraper <- R6Class(
    "pennsylvania_youth_scraper",
    inherit = generic_scraper,
    public = list(
        log = NULL,
        initialize = function(
            log,
            url = "https://www.dhs.pa.gov/providers/Providers/Pages/Coronavirus-State-Facility-Data.aspx",
            id = "pennsylvania_youth",
            type = "html",
            state = "PA",
            jurisdiction = "state",
            check_date = pennsylvania_youth_check_date,
            # pull the JSON data directly from the API
            pull_func = pennsylvania_youth_pull,
            # restructuring the data means pulling out the data portion of the json
            restruct_func = pennsylvania_youth_restruct,
            # Rename the columns to appropriate database names
            extract_func = pennsylvania_youth_extract){
            super$initialize(
                url = url, id = id, pull_func = pull_func, type = type,
                restruct_func = restruct_func, extract_func = extract_func,
                log = log, state = state, jurisdiction = jurisdiction,
                check_date = check_date)
        }
    )
)

if(sys.nframe() == 0){
    pennsylvania_youth <- pennsylvania_youth_scraper$new(log=TRUE)
    pennsylvania_youth$run_check_date()
    pennsylvania_youth$raw_data
    pennsylvania_youth$pull_raw()
    pennsylvania_youth$raw_data
    pennsylvania_youth$save_raw()
    pennsylvania_youth$restruct_raw()
    pennsylvania_youth$restruct_data
    pennsylvania_youth$extract_from_raw()
    pennsylvania_youth$extract_data
    pennsylvania_youth$validate_extract()
    pennsylvania_youth$save_extract()
}

