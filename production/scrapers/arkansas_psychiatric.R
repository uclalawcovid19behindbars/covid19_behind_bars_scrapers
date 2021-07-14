source("./R/generic_scraper.R")
source("./R/utilities.R")

arkansas_psychiatric_pull <- function(x){
    z <- get_src_by_attr(
        x, "a", attr = "href", attr_regex = "(?i)congregate") %>%
        first()
    
    z %>%
        str_split("Settings_") %>%
        unlist() %>%
        last() %>%
        str_remove(".pdf") %>%
        lubridate::mdy() %>%
        error_on_date()
}

arkansas_psychiatric_pull <- function(x){
    get_src_by_attr(x, "a", attr = "href", attr_regex = "(?i)congregate") %>%
        first()
}

arkansas_psychiatric_restruct <- function(x){
    stop_defunct_scraper("https://www.healthy.arkansas.gov/programs-services/topics/covid-19-reports")
    z <- magick::image_read_pdf(x)
    
    z_text <- magick::image_ocr(z)
    
    z_sub_text <- z_text %>%
        str_split_fixed("\nCumulative Total", 2) %>%
        .[[2]]
    
    z_val_vec <- z_sub_text %>%
        str_split_fixed("\nThese data", 2) %>%
        .[[1]] %>%
        str_split_fixed("Days\n", 2) %>%
        .[[2]] %>%
        str_split(" ") %>%
        unlist() %>%
        str_remove(",") %>%
        as.numeric()
    
    z_text_str <- z_sub_text %>%
        str_split_fixed("\nThese data", 2) %>%
        .[[1]] %>%
        str_split_fixed("Days\n", 2) %>%
        .[[1]] %>%
        str_squish()
    
    test_str <- str_c(
        "Cumulative Positive Positive Inmate/Resident Total Positive ",
        "Positive Staff Cumulative Total Facilities Followed Inmate/Resident ",
        "Past 14 Days Staff Past 14"
    )
    
    if(z_text_str != test_str){
        warning("text is not as expected please inspect.")
    }
    
    death_vals <- z_sub_text %>%
        str_split_fixed("Expired", 2) %>%
        .[[2]] %>%
        str_split_fixed("\n", 2) %>%
        c() %>%
        str_remove("\n") %>%
        str_remove(".*:") %>%
        as.numeric()
    
    d_txt <- z_sub_text %>%
        str_split_fixed("Expired", 2) %>%
        .[[2]] %>%
        str_split_fixed("\n", 2) %>%
        c()
    
    if(!(str_detect(d_txt[1], "Inmate") & str_detect(d_txt[2], "Staff"))){
        warning("death text is not as expected please inspect.")
    }
    
    tibble(
        names = c(
            "facs", "Residents.Confirmed", "Residents.Active", 
            "Staff.Confirmed", "Staff.Active", "Residents.Deaths",
            "Staff.Deaths"),
        vals = c(z_val_vec, death_vals)
    )
}

arkansas_psychiatric_extract <- function(x){
    x %>%
        filter(names!="facs") %>%
        pivot_wider(names_from = "names", values_from = "vals") %>%
        mutate(Name = "ALL ARKANSAS PSYCHIATRIC")
}

#' Scraper class for Arkansas Psychiatric COVID data
#' 
#' @name arkansas_psychiatric_scraper
#' @description AR has data for many congregate settings is compiled by DHHS.
#' Here we need to filter down to just hospital psychiatric facilities. Note this
#' is a low priority scraper as the facilities posted are inconsistent.
#' \describe{
#'   \item{Facility name}{Name}
#' }

arkansas_psychiatric_scraper <- R6Class(
    "arkansas_psychiatric_scraper",
    inherit = generic_scraper,
    public = list(
        log = NULL,
        initialize = function(
            log,
            url = "https://www.healthy.arkansas.gov/programs-services/topics/covid-19-reports",
            id = "arkansas_psychiatric",
            type = "pdf",
            state = "AR",
            jurisdiction = "psychiatric",
            # pull the JSON data directly from the API
            pull_func = arkansas_psychiatric_pull,
            # restructuring the data means pulling out the data portion of the json
            restruct_func = arkansas_psychiatric_restruct,
            check_date = NULL,
            # Rename the columns to appropriate database names
            extract_func = arkansas_psychiatric_extract){
            super$initialize(
                url = url, id = id, pull_func = pull_func, type = type,
                restruct_func = restruct_func, extract_func = extract_func,
                log = log, state = state, jurisdiction = jurisdiction,
                check_date = check_date)
        }
    )
)

if(sys.nframe() == 0){
    arkansas_psychiatric <- arkansas_psychiatric_scraper$new(log=TRUE)
    arkansas_psychiatric$run_check_date()
    arkansas_psychiatric$raw_data
    arkansas_psychiatric$pull_raw()
    arkansas_psychiatric$raw_data
    arkansas_psychiatric$save_raw()
    arkansas_psychiatric$restruct_raw()
    arkansas_psychiatric$restruct_data
    arkansas_psychiatric$extract_from_raw()
    arkansas_psychiatric$extract_data
    arkansas_psychiatric$validate_extract()
    arkansas_psychiatric$save_extract()
}

