source("./R/generic_scraper.R")
source("./R/utilities.R")

get_fiscal_year <- function(date){
    if (date >= as.Date("2019-07-01") & date < as.Date("2020-07-01")){
        fy <- 2020 
    } else if (date >= as.Date("2020-07-01") & date < as.Date("2021-07-01")){
        fy <- 2021
    }
    fy 
}

historical_wa_pop_pull <- function(x, date, file = NULL){
    if (get_fiscal_year(date) == 2020){
        report <- "400-RE002-2006"
        
    } else if (get_fiscal_year(date) == 2021){
        report <- "400-RE002"
    }
    
    stringr::str_c(
        "https://www.doc.wa.gov/docs/publications/reports/", report, ".pdf"
    )
}        

historical_wa_pop_restruct <- function(x, date){
    if (get_fiscal_year(date) == 2020){
        colnames <- c(
            "Name", 
            "Jul.19.ADP", "Jul.19.Cap", 
            "Aug.19.ADP", "Aug.19.Cap", 
            "Sep.19.ADP", "Sep.19.Cap", 
            "Oct.19.ADP", "Oct.19.Cap", 
            "Nov.19.ADP", "Nov.19.Cap", 
            "Dec.19.ADP", "Dec.19.Cap", 
            "Jan.20.ADP", "Jan.20.Cap", 
            "Feb.20.ADP", "Feb.20.Cap", 
            "Mar.20.ADP", "Mar.20.Cap", 
            "Apr.20.ADP", "Apr.20.Cap", 
            "May.20.ADP", "May.20.Cap", 
            "Jun.20.ADP", "Jun.20.Cap"
        )
        
    } else if (get_fiscal_year(date) == 2021){
        colnames <- c(
            "Name", 
            "Jul.20.ADP", "Jul.20.Cap", 
            "Aug.20.ADP", "Aug.20.Cap", 
            "Sep.20.ADP", "Sep.20.Cap", 
            "Oct.20.ADP", "Oct.20.Cap", 
            "Nov.20.ADP", "Nov.20.Cap", 
            "Dec.20.ADP", "Dec.20.Cap", 
            "Jan.21.ADP", "Jan.21.Cap", 
            "Feb.21.ADP", "Feb.21.Cap", 
            "Mar.21.ADP", "Mar.21.Cap", 
            "Apr.21.ADP", "Apr.21.Cap", 
            "May.21.ADP", "May.21.Cap", 
            "Jun.21.ADP", "Jun.21.Cap"
        )
    }
    
    x_ <- x %>% 
        tabulizer::extract_tables(pages = 1) %>% 
        as.data.frame()
    
    names(x_) <- colnames
    
    x_ %>% 
        filter(stringr::str_detect(
            Name, 
            "Airway|Cedar|Clallam|Coyote|Larch|Mission|Monroe|Olympic|Stafford|Washington|Yakima County Prison")) %>% 
        pivot_longer(!Name, names_to = "Metric", values_to = "Value") %>% 
        tidyr::separate(Metric, c("Month", "Year", "Metric"), "\\.") %>% 
        pivot_wider(names_from = Metric, values_from = Value)
}
    
historical_wa_pop_extract <- function(x, date){
    out <- x %>% 
        filter(lubridate::month(date) == match(Month, month.abb)) %>% 
        rename("Residents.Population" = "ADP") %>% 
        select(Name, Residents.Population) %>% 
        clean_scraped_df()
    
    if (nrow(out) != 13){
        stop(str_c("Number of facilities ", nrow(out), " different from expected 13"))
    }
    
    out
}

#' Scraper class for Washington population data
#' 
#' @name historical_washington_pop_scraper
#' @description The WA DOC posts population tracking reports with the monthly 
#' average daily population of incarcerated individuals by facility. They post 
#' these in a single pdf table per fiscal year. In addition to prisons, the file 
#' includes statewide emergency beds, rented beds, and work release data. 
#' \describe{
#'   \item{ADP}{Average Daily Population}
#'   \item{Capacity}{}
#' }

historical_wa_pop_scraper <- R6Class(
    "historical_washington_pop_scraper",
    inherit = generic_scraper,
    public = list(
        log = NULL,
        initialize = function(
            log,
            url = "https://www.doc.wa.gov/information/records/publications.htm",
            id = "historical_wa_pop",
            type = "pdf",
            state = "WA",
            jurisdiction = "state",
            pull_func = historical_wa_pop_pull,
            restruct_func = historical_wa_pop_restruct,
            extract_func = historical_wa_pop_extract){
            super$initialize(
                url = url, id = id, pull_func = pull_func, type = type,
                restruct_func = restruct_func, extract_func = extract_func,
                log = log, state = state, jurisdiction  = jurisdiction)
        }
    )
)

if(sys.nframe() == 0){
    historical_wa_pop <- historical_wa_pop_scraper$new(log=TRUE)
    historical_wa_pop$reset_date("2021-03-01")
    historical_wa_pop$raw_data
    historical_wa_pop$pull_raw(date = historical_wa_pop$date, .dated_pull = TRUE)
    historical_wa_pop$raw_data
    historical_wa_pop$save_raw()
    historical_wa_pop$restruct_raw(date = historical_wa_pop$date)
    historical_wa_pop$restruct_data
    historical_wa_pop$extract_from_raw(date = historical_wa_pop$date)
    historical_wa_pop$extract_data
    historical_wa_pop$validate_extract()
    historical_wa_pop$save_extract()
}
 