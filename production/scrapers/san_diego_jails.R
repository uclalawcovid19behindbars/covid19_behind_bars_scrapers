source("./R/generic_scraper.R")
source("./R/utilities.R")

san_diego_jails_check_date <- function(x, date = Sys.Date()){
    html_obj <- xml2::read_html(x)

    html_obj %>%
        rvest::html_nodes("ul") %>%
        .[str_detect(rvest::html_text(.), "(?i)jail status")] %>%
        rvest::html_text() %>%
        str_split("(?i)covid-19 jail status report as of |\n") %>%
        unlist() %>%
        .[2] %>%
        lubridate::mdy() %>%
        error_on_date(date)
}

san_diego_jails_pull <- function(x) {
    html_obj <- xml2::read_html(x)
    html_obj %>%
        rvest::html_nodes("a") %>%
        .[str_detect(rvest::html_text(.), "(?i)jail status")] %>%
        rvest::html_attr("href") %>%
        xml2::url_absolute(x) %>%
        magick::image_read_pdf()
}

san_diego_jails_restruct <- function(x) {
    x %>% 
        magick::image_crop("2700x700+0+300") %>% 
        ExtractTable()
}

san_diego_jails_extract <- function(x) {
    
    if(str_detect(x[[1]][["0"]][1], "(?i)population")){
        col_name <- matrix(c(
            "Residents.Population", "0", "CURRENT JAIL POPULATION", 
            "Residents.Active", "1", "ACTIVE COVID-19 CASES IN CUSTODY",
            "Drop.Residents.pact", "2", "% ACTIVE CASES IN CUSTODY", 
            "Residents.Quarantine", "3", "INDIVIDUALS IN ISOLATION FOR PRECAUTIONS", 
            "Drop.Residents.pquar", "4", "% INDIVIDUALS IN ISOLATION"
        ), ncol = 3, nrow = 5, byrow = TRUE)
        
        colnames(col_name) <- c("clean", "raw", "check")
        col_name_df <- as_tibble(col_name)
        
        df_ <- as.data.frame(x[[1]])
        
        check_names_extractable(df_, col_name_df)
        
        out_df <- rename_extractable(df_, col_name_df) %>% 
            filter(!str_detect(Residents.Population, "(?i)population")) %>% 
            mutate(Name = "San Diego County Jails") %>% 
            clean_scraped_df() %>%
            select(-starts_with("Drop")) %>%
            as_tibble()
    }
    
    else{
        col_name <- matrix(c(
            "Residents.Tested", "0", "COVID-19 TESTS ADMINISTERED", 
            "Residents.Confirmed", "1", "CUMULATIVE POSITIVE CASES",
            "Residents.Active", "2", "ACTIVE CASES IN CUSTODY", 
            "Residents.Recovered", "3", "RECOVERED/ RELEASED CASES", 
            "Residents.Quarantine", "4", "INMATES IN ISOLATION FOR PRECAUTIONS"
        ), ncol = 3, nrow = 5, byrow = TRUE)
        
        colnames(col_name) <- c("clean", "raw", "check")
        col_name_df <- as_tibble(col_name)
        
        df_ <- as.data.frame(x[[1]])
        
        check_names_extractable(df_, col_name_df)
        
        out_df <- rename_extractable(df_, col_name_df) %>% 
            filter(!Residents.Tested == "COVID-19 TESTS ADMINISTERED") %>% 
            mutate(Name = "San Diego County Jails") %>% 
            clean_scraped_df() %>% 
            as_tibble()
    }
    
    out_df
}

#' Scraper class for general San Diego County jails COVID data
#' 
#' @name san_diego_jails_scraper
#' @description San Diego County reports testing and confirmed data for 
#' residents in a pdf hosted on the web page. The data includes all seven San 
#' Diego County detention facilities. Tests administered and cumulative positive 
#' cases are based on data collected from March 16, 2020 to the report date. 
#' \describe{
#'   \item{COVID-19 Tests Administered}{Number of tests administered to residents}
#'   \item{Cumulative Positive Cases}{Number of residents confirmed positive}
#'   \item{Active Cases in Custody}{Number of inmates currently in custody who are confirmed COVID-19 positve}
#'   \item{Recovered/Released Cases}{Number of inmates with a positve COVID-19 test result who later tested 
#'   negative and have been cleared by medical personnel while in custody; and inmates with a positive COVID-19 
#'   test result who were released from custody either before or after clearance from medical isolation.}
#'   \item{Inmates in Isolation for Precautions}{Number of inmates who are being monitored by medical personnel 
#'   either due to presenting COVID-like symptoms or due to possible exposure.}

san_diego_jails_scraper <- R6Class(
    "san_diego_jails_scraper",
    inherit = generic_scraper,
    public = list(
        log = NULL,
        initialize = function(
            log,
            url = "https://www.sdsheriff.net/resources/covid-19-response/covid-19-figures",
            id = "san_diego_jails",
            type = "img",
            state = "CA",
            jurisdiction = "county",
            check_date = san_diego_jails_check_date,
            # pull the JSON data directly from the API
            pull_func = san_diego_jails_pull,
            # restructuring the data means pulling out the data portion of the json
            restruct_func = san_diego_jails_restruct,
            # Rename the columns to appropriate database names
            extract_func = san_diego_jails_extract){
            super$initialize(
                url = url, id = id, pull_func = pull_func, type = type,
                restruct_func = restruct_func, extract_func = extract_func,
                log = log, state = state, jurisdiction = jurisdiction,
                check_date = check_date)
        }
    )
)

if(sys.nframe() == 0){
    san_diego_jails <- san_diego_jails_scraper$new(log=FALSE)
    san_diego_jails$run_check_date()
    san_diego_jails$raw_data
    san_diego_jails$pull_raw()
    san_diego_jails$raw_data
    san_diego_jails$save_raw()
    san_diego_jails$restruct_raw()
    san_diego_jails$restruct_data
    san_diego_jails$extract_from_raw()
    san_diego_jails$extract_data
    san_diego_jails$validate_extract()
    san_diego_jails$save_extract()
}
