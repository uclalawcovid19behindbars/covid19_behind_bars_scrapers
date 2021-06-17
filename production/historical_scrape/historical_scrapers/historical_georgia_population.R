source("./R/generic_scraper.R")
source("./R/utilities.R")

# Monthly files hosted here: http://www.gdc.ga.gov/Research/Monthly_Profile_all_inmates
# Add URL in config file 
# (e.g. 2020-04-01 -> http://www.gdc.ga.gov/sites/all/themes/gdc/pdf/Profile_all_inmates_2021_03.pdf)
historical_ga_pop_pull <- function(x, file, date = NULL){
    file
}

check_ga_pop_table <- function(x, type_str) {
    col_name_mat_row1 <- matrix(c(
        "X0", "",
        "X1", "",
        "X2", "Male",
        "X3", "",
        "X4", "",
        "X5", "Female",
        "X6", "", 
        "X7", "Total", 
        "X8", ""
    ), ncol = 2, nrow = 9, byrow = TRUE)
    
    colnames(col_name_mat_row1) <- c("raw", "check")
    col_name_df_row1 <- as_tibble(col_name_mat_row1)
    
    check_names_extractable(as.data.frame(x), col_name_df_row1)
    
    col_name_mat_row2 <- matrix(c(
        "Name", "X0", type_str, 
        "Male.Count.Drop", "X1", "Count",
        "Male.ColPct.Drop", "X2", "Col %",
        "Male.RowPct.Drop", "X3", "Row %",
        "Female.Count.Drop", "X4", "Count",
        "Female.ColPct.Drop", "X5", "Col %",
        "Female.RowPct.Drop", "X6", "Row %", 
        "Residents.Population", "X7", "Total", 
        "Total.ColPct.Drop", "X8", "Col %"
    ), ncol = 3, nrow = 9, byrow = TRUE)
    
    colnames(col_name_mat_row2) <- c("clean", "raw", "check")
    col_name_df_row2 <- as_tibble(col_name_mat_row2)
    
    out <- x %>% 
        as.data.frame() %>% 
        slice(-1)
    
    check_names_extractable(out, col_name_df_row2)
    
    rename_extractable(out, col_name_df_row2)
}

historical_ga_pop_restruct <- function(x, date = NULL){
    # Page numbers are hard-coded, will throw errors if this changes though 
    bind_rows(
        x %>% 
            magick::image_read_pdf(pages = 26) %>%  
            magick::image_crop("2500x1200+0+700") %>% 
            ExtractTable() %>% 
            check_ga_pop_table(type_str = "Institution Type - Trans. Centers"),
        x %>% 
            magick::image_read_pdf(pages = 27) %>% 
            ExtractTable() %>% 
            check_ga_pop_table(type_str = "Institution Type - County Prisons"),
        x %>% 
            magick::image_read_pdf(pages = 28) %>% 
            magick::image_crop("2500x400+0+700") %>% 
            ExtractTable() %>% 
            check_ga_pop_table(type_str = "Institution Type - County Prisons"),
        x %>%
            magick::image_read_pdf(pages = 29) %>% 
            ExtractTable() %>% 
            check_ga_pop_table(type_str = "Institution Type - State Prisons"), 
        x %>% 
            magick::image_read_pdf(pages = 30) %>% 
            magick::image_crop("2500x400+0+700") %>% 
            ExtractTable() %>% 
            check_ga_pop_table(type_str = "Institution Type - State Prisons"), 
        x %>% 
            magick::image_read_pdf(pages = 31) %>% 
            magick::image_crop("2500x500+0+700") %>% 
            ExtractTable() %>% 
            check_ga_pop_table(type_str = "Institution Type - Private Prisons")
    )
}

historical_ga_pop_extract <- function(x, date = NULL){
    x %>% 
        select(-ends_with(".Drop")) %>% 
        mutate(Name = clean_fac_col_txt(Name, to_upper = TRUE)) %>% 
        filter(!stringr::str_detect(Name, "TOTAL(?i)")) %>% 
        filter(!stringr::str_detect(Name, "REPORTED(?i)")) %>% 
        filter(!stringr::str_detect(Name, "INSTITUTION TYPE(?i)")) %>% 
        clean_scraped_df() %>% 
        group_by(Name) %>% 
        summarise(Residents.Population = sum(Residents.Population)) %>% 
        ungroup()
}

#' Scraper class for Georgia population data
#' 
#' @name historical_georgia_pop_scraper
#' @description The GA DOC posts extremely long and detailed monthly inmate statistical 
#' profile reports on the first of every month. This includes demographic information, 
#' correctional information, educational, psychological and physical information, 
#' criminal history information, and medical information. The only information we 
#' scrape here is facility-level population data, reported for transitional centers, 
#' county prisons, state prisons, and private prisons. 
#' \describe{
#'   \item{Institution}{Facility name}
#'   \item{Total}{Incarcerated population}
#' }

historical_ga_pop_scraper <- R6Class(
    "historical_georgia_pop_scraper",
    inherit = generic_scraper,
    public = list(
        log = NULL,
        initialize = function(
            log,
            url = "http://www.gdc.ga.gov/Research/Monthly_Profile_all_inmates",
            id = "historical_ga_pop",
            type = "pdf",
            state = "GA",
            jurisdiction = "state",
            check_date = NULL,
            pull_func = historical_ga_pop_pull,
            restruct_func = historical_ga_pop_restruct,
            extract_func = historical_ga_pop_extract){
            super$initialize(
                url = url, id = id, pull_func = pull_func, type = type,
                restruct_func = restruct_func, extract_func = extract_func,
                log = log, state = state, jurisdiction  = jurisdiction,
                check_date = check_date)
        }
    )
)

if(sys.nframe() == 0){
    historical_ga_pop <- historical_ga_pop_scraper$new(log=TRUE)
    historical_ga_pop$reset_date("DATE")
    historical_ga_pop$raw_data
    historical_ga_pop$pull_raw(file, .dated_pull = TRUE)
    historical_ga_pop$raw_data
    historical_ga_pop$save_raw()
    historical_ga_pop$restruct_raw()
    historical_ga_pop$restruct_data
    historical_ga_pop$extract_from_raw()
    historical_ga_pop$extract_data
    historical_ga_pop$validate_extract()
    historical_ga_pop$save_extract()
}
