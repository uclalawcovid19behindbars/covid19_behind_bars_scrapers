source("./R/generic_scraper.R")
source("./R/utilities.R")

historical_ca_pop_pull <- function(x, date, file = NULL){
    date <- as.Date(date, format = "%Y-%m-%d")
    year4 <- format.Date(date, "%Y")
    year2 <- format.Date(date, "%y")
    month <- format.Date(date, "%m")
    day <- format.Date(date, "%d")
    
    # 9/30/20 URL is in the 10 month folder 
    if (date == "2020-09-30") {
        "https://www.cdcr.ca.gov/research/wp-content/uploads/sites/174/2020/10/Tpop1d200930.pdf"
    }
    else {
    stringr::str_c(
        "https://www.cdcr.ca.gov/research/wp-content/uploads/sites/174/", 
        format.Date(date, "%Y"), "/", month, "/Tpop1d", year2, month, day, ".pdf")
    }
}

historical_ca_pop_restruct <- function(x, date = NULL){
    magick::image_read_pdf(x, pages = 2) %>% 
        ExtractTable()
}

historical_ca_pop_extract <- function(x, date = NULL){
    col_name_mat <- matrix(c(
        "Institutions", "X0", "Name", 
        "Felon/ Other", "X1", "Residents.Population", 
        "Design Capacity", "X2", "Capacity.Drop", 
        "Percent Occupied", "X3", "Percent.Occupied.Drop", 
        "Staffed Capacity", "X4", "Staffed.Capacity.Drop"
    ), ncol = 3, nrow = 5, byrow = TRUE)
    
    colnames(col_name_mat) <- c("check", "raw", "clean")
    col_name_df <- as_tibble(col_name_mat)
    
    df_ <- as.data.frame(x)
    
    check_names_extractable(df_, col_name_df)
    
    rename_extractable(df_, col_name_df) %>% 
        as_tibble() %>% 
        filter(!Name %in% c("Institutions", "Male Institutions", "Female Institutions")) %>% 
        filter(!str_detect(Name, "(?i)Total")) %>% 
        mutate_at(vars(-Name), string_to_clean_numeric) %>%
        select(-ends_with(".Drop")) %>% 
        clean_scraped_df()
}

#' Scraper class for California population data 
#' 
#' @name historical_california_pop_scraper
#' @description CDCR posts weekly population reports in PDF form. In addition to 
#' facility-level population, these reports also report Design Capacity, Percent
#' Occupied, and Staffed Capacity, which are not scraped for now. These reports are 
#' posted on Thursdays (with data as of midnight the previous night) and archived. 
#' \describe{
#'   \item{Felon/Other}{Residents.Population}
#' }

historical_ca_pop_scraper <- R6Class(
    "historical_california_pop_scraper",
    inherit = generic_scraper,
    public = list(
        log = NULL,
        initialize = function(
            log,
            url = "https://www.cdcr.ca.gov/research/weekly-total-population-report-archive-2/",
            id = "historical_ca_pop",
            type = "pdf",
            state = "CA",
            jurisdiction = "state",
            pull_func = historical_ca_pop_pull,
            restruct_func = historical_ca_pop_restruct,
            extract_func = historical_ca_pop_extract){
            super$initialize(
                url = url, id = id, pull_func = pull_func, type = type,
                restruct_func = restruct_func, extract_func = extract_func,
                log = log, state = state, jurisdiction  = jurisdiction)
        }
    )
)

if(sys.nframe() == 0){
    historical_ca_pop <- historical_ca_pop_scraper$new(log=TRUE)
    historical_ca_pop$reset_date("SET_DATE_HERE")
    historical_ca_pop$raw_data
    historical_ca_pop$pull_raw(date = historical_ca_pop$date, .dated_pull = TRUE)
    historical_ca_pop$raw_data
    historical_ca_pop$save_raw()
    historical_ca_pop$restruct_raw(date = historical_ca_pop$date)
    historical_ca_pop$restruct_data
    historical_ca_pop$extract_from_raw(date = historical_ca_pop_scraper$date)
    historical_ca_pop$extract_data
    historical_ca_pop$validate_extract()
    historical_ca_pop$save_extract()
}
