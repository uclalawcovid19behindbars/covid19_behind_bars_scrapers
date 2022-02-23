source("./R/generic_scraper.R")
source("./R/utilities.R")

rhode_island_pdf_check_date <- function(url, date = Sys.Date()){
    pdf_url <- get_src_by_attr(url, "a", attr = "href", 
                               attr_regex = "ridoc-covid-data") %>% 
        first()
    
    pdf_url %>% 
        str_split("ridoc-covid-data-") %>% # get the part of the url with the date
        .[[1]] %>% 
        .[2] %>%
        lubridate::mdy() %>% 
        error_on_date(date)
    
}

rhode_island_pdf_pull <- function(url){
    # Create url front section
    url_front <- 'https://doc.ri.gov'
    # Pull pdf link
    pdf_url <- url %>%
        rvest::read_html() %>%
        rvest::html_nodes('a') %>%
        rvest::html_attr('href') %>%
        as.data.frame() %>%
        rename(c('links' = .)) %>%
        subset(str_detect(links,'media')) %>%
        mutate(order = str_replace_all(links, '.*media', ''), # Pull order of links from number embedded in download urls
               order = str_replace_all(order, 'download.*', ''),
               order = str_replace_all(order, '\\/', ''),
               order = as.numeric(order)) %>%
        arrange(desc(order)) %>%
        first() %>% # Pull highest number / most recent download link
        .[1, 'links'] %>%
        str_c(url_front, .)
    
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
        "inmates active**", "5", "Residents.Active",
        "past 24 hours", "6", "Residents.DayActive.Drop",
        "population", "7", "Residents.Population",
        "% population fully vaccinated***", "8", "Residents.Completed.Pct",
        "total", "9", "Staff.Confirmed",
        "deaths", "10", "Staff.Deaths",
        "staff recovered", "11", "Staff.Recovered",
        "active**", "12", "Staff.Active",
        "past 24 hours", "13", "Staff.DayActive.Drop"
    ), ncol = 3, nrow = 14, byrow = TRUE)
    
    colnames(ri_col_name_mat) <- c("check", "raw", "clean")
    ri_col_name_df <- as_tibble(ri_col_name_mat)
    
    df_ <- as.data.frame(extract)
    
    check_names_extractable(df_, ri_col_name_df)
    
    extract_out <- rename_extractable(df_, ri_col_name_df) %>% 
        as_tibble() %>% 
        filter(!str_detect(Name, "(?i)facilit")) %>% 
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

