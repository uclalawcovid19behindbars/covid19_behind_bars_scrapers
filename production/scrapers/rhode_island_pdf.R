source("./R/generic_scraper.R")
source("./R/utilities.R")

rhode_island_pdf_check_date <- function(url, date = Sys.Date()){
    base_html <- rvest::read_html(url)
    
    pdf_date <- rvest::html_node(base_html, 
                                 xpath = "//span[contains(text(), 'RIDOC COVID Data')]") %>%
        rvest::html_text() %>% 
        lubridate::mdy()
    
    return(error_on_date(pdf_date, date))
}

rhode_island_pdf_pull <- function(url){
    base_html <- rvest::read_html(url)
    
    pdf_relative_address <- rvest::html_node(base_html, 
                                xpath = "//span[contains(text(), 'RIDOC COVID Data')]") %>%
        rvest::html_node(xpath = "parent::div/parent::a") %>% 
        rvest::html_attr("href") %>% 
        substring(2) # Take off leading '/'
    
    pdf_url <- str_c("http://www.doc.ri.gov/", pdf_relative_address)
    
    return(pdf_url)
}

rhode_island_pdf_restruct <- function(pdf_url){
    restruct <- pdf_url %>% 
        magick::image_read_pdf(pages = 1) %>% 
        magick::image_negate() %>%
        ExtractTable()
    
    return(restruct)
}

rhode_island_pdf_extract <- function(restruct){
    extract <- restruct[[1]]
    
    extract[1, ] <- extract %>%
        slice(c(1, 2)) %>%
        ## combine first two rows from the table to get ok column names to check
        summarise_all(str_c, collapse=' ') %>%
        .[1, ] %>%
        as.character() %>%
        tolower()
    
    ## check success with: clean_fac_col_txt(unname(unlist(extract[1,])))
    ri_col_name_mat <- matrix(c(
        "", "0", "Name",
        "total cases", "1", "All.Cases.Drop",
        "total", "2", "Residents.Confirmed",
        "deaths", "3", "Residents.Deaths",
        "recovered", "4", "Residents.Recovered",
        "active", "5", "Residents.Active",
        "past 72 hours", "6", "Residents.DayActive.Drop",
        "population", "7", "Residents.Population",
        "% population fully vaccinated***", "8", "Residents.Completed.Pct",
        "total", "9", "Staff.Confirmed",
        "deaths", "10", "Staff.Deaths",
        "recovered", "11", "Staff.Recovered",
        "active**", "12", "Staff.Active",
        "past 72 hours", "13", "Staff.DayActive.Drop"
    ), ncol = 3, nrow = 14, byrow = TRUE)
    
    colnames(ri_col_name_mat) <- c("check", "raw", "clean")
    ri_col_name_df <- as_tibble(ri_col_name_mat)
    
    df_ <- as.data.frame(extract)
    
    check_names_extractable(df_, ri_col_name_df)
    
    extract_out <- rename_extractable(df_, ri_col_name_df) %>% 
        as_tibble() %>% 
        filter(!str_detect(Name, "(?i)facilit")) %>% 
        filter(!str_detect(Name, "(?i)total")) %>% 
        filter(!str_detect(All.Cases.Drop, "(?i)total")) %>% 
        mutate_at(vars(-Name), string_to_clean_numeric) %>%
        select(-ends_with(".Drop")) %>% 
        clean_scraped_df() %>%
        mutate(Residents.Completed.Pct = Residents.Completed.Pct / 100) %>%
        relocate(Name, starts_with("Residents."))
}

#' Scraper class for general rhode_island_pdf COVID data
#' 
#' @name rhode_island_pdf_scraper
#' @description This will be a description of rhode_island_pdf data and what the scraper
#' does
#' \describe{
#'   \item{Facility_Name}{The faciilty name.}
#' }

rhode_island_pdf_scraper <- R6Class(
    "rhode_island_pdf_scraper",
    inherit = generic_scraper,
    public = list(
        log = NULL,
        initialize = function(
            log,
            url = "http://www.doc.ri.gov/covid-19/",
            id = "rhode_island_pdf",
            type = "pdf",
            state = "RI",
            jurisdiction = "state",
            check_date = rhode_island_pdf_check_date,
            # pull the JSON data directly from the API
            pull_func = rhode_island_pdf_pull,
            # restructuring the data means pulling out the data portion of the json
            restruct_func = rhode_island_pdf_restruct,
            # Rename the columns to appropriate database names
            extract_func = rhode_island_pdf_extract){
            super$initialize(
                url = url, id = id, pull_func = pull_func, type = type,
                restruct_func = restruct_func, extract_func = extract_func,
                log = log, state = state, jurisdiction = jurisdiction,
                check_date = check_date)
        }
    )
)

if(sys.nframe() == 0){
    rhode_island_pdf <- rhode_island_pdf_scraper$new(log=TRUE)
    rhode_island_pdf$run_check_date()
    rhode_island_pdf$raw_data
    rhode_island_pdf$pull_raw()
    rhode_island_pdf$raw_data
    rhode_island_pdf$save_raw()
    rhode_island_pdf$restruct_raw()
    rhode_island_pdf$restruct_data
    rhode_island_pdf$extract_from_raw()
    rhode_island_pdf$extract_data
    rhode_island_pdf$validate_extract()
    rhode_island_pdf$save_extract()
}

