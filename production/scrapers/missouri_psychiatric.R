source("./R/generic_scraper.R")
source("./R/utilities.R")

missouri_psychiatric_check_date <- function(x, date = Sys.Date()){
    base_html <- xml2::read_html(x)
    date_updated <- rvest::html_nodes(base_html, xpath="/html/body/div/div/div/section/div/article/div/div/div/div/p[65]/em") %>%
        rvest::html_text()
    
    date_updated %>%
        {.[str_detect(., "21")]} %>%
        str_split("Updated: ") %>%
        unlist() %>%
        .[2] %>%
        lubridate::mdy() %>%
        error_on_date(expected_date = date)
}

missouri_psychiatric_pull <- function(x){
    xml2::read_html(x)
}

missouri_psychiatric_restruct <- function(x){
    
    strong_text <- x %>%
        rvest::html_node(".content") %>%
        rvest::html_nodes("strong") %>%
        rvest::html_text() %>%
        unique()
    
    all_text <- x %>%
        rvest::html_node(".content") %>%
        rvest::html_nodes("p") %>%
        rvest::html_text() %>%
        unique() %>%
        str_replace_all("–", "-") %>%
        str_replace_all("-", "-") %>%
        str_squish()
    
    st_starts <- which(str_detect(all_text, "STAFF"))
    res_starts <- which(str_detect(all_text, "PATIENTS/RESIDENTS"))
    txt_end <- which(str_detect(all_text, "TOTAL"))
    
    if(length(st_starts) != 2 | length(res_starts) != 2 | length(txt_end) != 1){
        stop("text is not as expected please inspect.")
    }
    
    if(!all(str_detect(all_text[c(st_starts[2], res_starts[2])], "DEATHS"))){
        stop("text is not as expected please inspect.")
    }
    
    bind_rows(
        all_text[(st_starts[1]+1):(res_starts[1]-1)] %>%
            str_split_fixed(" - ", 2) %>%
            `colnames<-`(c("Staff.Confirmed", "Name")) %>% 
            as_tibble() %>%
            mutate(Staff.Confirmed = as.numeric(Staff.Confirmed)),
        
        all_text[(res_starts[1]+1):(st_starts[2]-1)] %>%
            str_split_fixed(" - ", 2) %>%
            `colnames<-`(c("Residents.Confirmed", "Name")) %>% 
            as_tibble() %>%
            mutate(Residents.Confirmed = as.numeric(Residents.Confirmed)),
        
        all_text[(st_starts[2]+1):(res_starts[2]-1)] %>%
            str_split_fixed(" - ", 2) %>%
            `colnames<-`(c("Staff.Deaths", "Name")) %>% 
            as_tibble() %>%
            mutate(Staff.Deaths = as.numeric(Staff.Deaths)),
        
        all_text[(res_starts[2]+1):(txt_end-1)] %>%
            str_split_fixed(" - ", 2) %>%
            `colnames<-`(c("Residents.Deaths", "Name")) %>% 
            as_tibble() %>%
            mutate(Residents.Deaths = as.numeric(Residents.Deaths)))
    
}

missouri_psychiatric_extract <- function(x){
    x %>%
        group_by(Name) %>%
        summarize_all(sum_na_rm)
}

#' Scraper class for general missouri_psychiatric COVID data
#' 
#' @name missouri_psychiatric_scraper
#' @description data is in a loosely structured format and will require some
#' serious checks to ensure the data is correctly being pulled.
#' \describe{
#'   \item{Facility name}{Name}
#'   \item{Patients testing positive}{Residents.Confirmed}
#'   \item{Staff testing positive}{Staff.Confirmed}
#'   \item{Patients Deaths}{Residents.Deaths}
#'   \item{Staff Deaths}{Staff.Deaths}
#' }

missouri_psychiatric_scraper <- R6Class(
    "missouri_psychiatric_scraper",
    inherit = generic_scraper,
    public = list(
        log = NULL,
        initialize = function(
            log,
            url = "https://dmh.mo.gov/disaster-services/covid-19-information/dmh-positive-cases-data",
            id = "missouri_psychiatric",
            type = "html",
            state = "MO",
            jurisdiction = "psychiatric",
            check_date = missouri_psychiatric_check_date,
            pull_func = missouri_psychiatric_pull,
            restruct_func = missouri_psychiatric_restruct,
            extract_func = missouri_psychiatric_extract){
            super$initialize(
                url = url, id = id, pull_func = pull_func, type = type,
                restruct_func = restruct_func, extract_func = extract_func,
                log = log, state = state, jurisdiction = jurisdiction,
                check_date = check_date)
        }
    )
)

if(sys.nframe() == 0){
    missouri_psychiatric <- missouri_psychiatric_scraper$new(log=TRUE)
    missouri_psychiatric$run_check_date()
    missouri_psychiatric$perma_save()
    missouri_psychiatric$raw_data
    missouri_psychiatric$pull_raw()
    missouri_psychiatric$raw_data
    missouri_psychiatric$save_raw()
    missouri_psychiatric$restruct_raw()
    missouri_psychiatric$restruct_data
    missouri_psychiatric$extract_from_raw()
    missouri_psychiatric$extract_data
    missouri_psychiatric$validate_extract()
    missouri_psychiatric$save_extract()
}

