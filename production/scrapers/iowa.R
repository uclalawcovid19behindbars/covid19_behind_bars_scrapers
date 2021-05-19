source("./R/generic_scraper.R")
source("./R/utilities.R")

iowa_restruct <- function(x){
    
    bind_rows(
        x %>%
            rvest::html_nodes("table") %>%
            # there is only one table on the page and it has no names
            # associated with it
            .[[1]] %>%
            rvest::html_table(header = TRUE), 
        
        # Staff deaths are reported in text at the bottom at the statewide level 
        x %>%
            rvest::html_nodes("span") %>% 
            rvest::html_text() %>% 
            as_tibble() %>% 
            filter(stringr::str_detect(value, "Number of staff that have passed away with COVID-19")) %>%
            separate(value, sep = ":", into = c("Prison", "Staff.Deaths")) %>% 
            mutate(Prison = "STATEWIDE") 
    )
}

iowa_extract <- function(x){
    names(x) <- str_squish(names(x))
    expected_names <- c(
        "Prison", "Inmates Tested", "Inmates Positive", 
        "Inmates No Longer Positive", "Staff Positive*",
        "Staff No Longer Positive", 
        "COVID Related Inmate Deaths", "Staff.Deaths")
    
    check_names(x, expected_names)
    
    names(x) <- c(
        "Name", "Residents.Tadmin", "Residents.Active",
        "Residents.Recovered", "Staff.Active", "Staff.Recovered",
        "Residents.Deaths", "Staff.Deaths")

    x %>% 
        filter(!Name %in% c("Prison", "Total")) %>% 
        clean_scraped_df() %>% 
        mutate(Residents.Confirmed = Residents.Active + Residents.Recovered, 
               Staff.Confirmed = Staff.Active + Staff.Recovered) 
}

#' Scraper class for general Iowa COVID data
#' 
#' @name iowa_scraper
#' @description Html table with minimal recoding and cleaning
#' \describe{
#'   \item{Prison}{The faciilty name}
#'   \item{Inmates Tested}{tests administered (Residents.Tadmin)}
#'   \item{Inmates Positive}{Cumulative number of residents who are positive.}
#'   \item{Inmates Recovered}{Cumulative residents recovered.}
#'   \item{Staff Positive}{Cumulative number of staff who are positive.}
#'   \item{Staff Recovered}{Cumulative staff recovered.}
#'   \item{COVID Related Inmate Deaths}{Cumulative residents who have died.}
#' }

iowa_scraper <- R6Class(
    "iowa_scraper",
    inherit = generic_scraper,
    public = list(
        log = NULL,
        initialize = function(
            log,
            url = "https://doc.iowa.gov/COVID19",
            id = "iowa",
            state = "IA",
            type = "html",
            jurisdiction = "state",
            pull_func = xml2::read_html,
            restruct_func = iowa_restruct,
            extract_func = iowa_extract){
            super$initialize(
                url = url, id = id, pull_func = pull_func, type = type,
                restruct_func = restruct_func, extract_func = extract_func,
                log = log, state = state, jurisdiction = jurisdiction)
            })
)

if(sys.nframe() == 0){
    iowa <- iowa_scraper$new(log = FALSE)
    iowa$raw_data
    iowa$pull_raw()
    iowa$raw_data
    iowa$restruct_raw()
    iowa$restruct_data
    iowa$extract_from_raw()
    iowa$extract_data
    iowa$validate_extract()
    iowa$save_extract()
}
