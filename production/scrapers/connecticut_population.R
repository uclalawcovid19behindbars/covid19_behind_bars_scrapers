source("./R/generic_scraper.R")
source("./R/utilities.R")

connecticut_population_pull <- function(x){
    read_csv("https://data.ct.gov/api/views/n8x6-s299/rows.csv", 
             col_types = "ccccc") %>% 
        mutate(Date = lubridate::mdy(Date))
}

connecticut_population_restruct <- function(x){
    x %>% 
        filter(Date == max(Date, na.rm = TRUE))
}

connecticut_population_extract <- function(x, exp_date = Sys.Date()){
    
    error_on_date(first(x$Date), exp_date, 30)
    
    check_names(x, c(
        "Date", 
        "Facility Name",  
        "Accused/Other Status Count", 
        "Sentenced Status Count", 
        "Total Facility Population Count"
    ))
    
    x %>% 
        select(
            "Name" = "Facility Name", 
            "Residents.Population" = "Total Facility Population Count"
        ) %>% 
        clean_scraped_df() %>% 
        
        # Aggregate names to match COVID data reporting 
        mutate(
            Name = clean_fac_col_txt(Name, to_upper = TRUE), 
            Name = case_when(str_detect(Name, "MACDOUGALL|WALKER") ~ "MACDOUGALL WALKER", 
                             str_detect(Name, "CORRIGAN|RADGOWSKI") ~ "CORRIGAN RADGOWSKI", 
                             TRUE ~ Name)
        ) %>% 
        group_by(Name) %>% 
        summarise(Residents.Population = sum_na_rm(Residents.Population)) %>% 
        ungroup() 
}

#' Scraper class for Connecticut population data 
#' 
#' @name connecticut_population_scraper
#' @description Connecticut's DOC posts facility-level population data on the 
#' state's open data portal. There's some facility name aggregation necessary to 
#' match how the DOC reports COVID data. The portal shows that it's updated 
#' almost daily, although the most recent date is almost a month old. 
#' \describe{
#'   \item{Date}{}
#'   \item{Facility Name}{Name}
#'   \item{Accused/Other Status Count}{}
#'   \item{Sentenced Status Count}{}
#'   \item{Total Facility Population Count}{Residents.Population}
#' }

connecticut_population_scraper <- R6Class(
    "connecticut_population_scraper",
    inherit = generic_scraper,
    public = list(
        log = NULL,
        initialize = function(
            log,
            url = "https://data.ct.gov/Public-Safety/Correctional-Facility-Daily-Population-Count-By-Fa/n8x6-s299",
            id = "connecticut_population",
            type = "csv",
            state = "CT",
            jurisdiction = "state",
            pull_func = connecticut_population_pull,
            restruct_func = connecticut_population_restruct,
            extract_func = connecticut_population_extract){
            super$initialize(
                url = url, id = id, pull_func = pull_func, type = type,
                restruct_func = restruct_func, extract_func = extract_func,
                log = log, state = state, jurisdiction  = jurisdiction)
        }
    )
)

if(sys.nframe() == 0){
    connecticut_population <- connecticut_population_scraper$new(log=TRUE)
    connecticut_population$raw_data
    connecticut_population$pull_raw()
    connecticut_population$raw_data
    connecticut_population$save_raw()
    connecticut_population$restruct_raw()
    connecticut_population$restruct_data
    connecticut_population$extract_from_raw()
    connecticut_population$extract_data
    connecticut_population$validate_extract()
    connecticut_population$save_extract()
}
