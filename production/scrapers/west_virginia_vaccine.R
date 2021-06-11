source("./R/generic_scraper.R")
source("./R/utilities.R")

west_virginia_vaccine_pull <- function(x){
    tsv_src <- get_src_by_attr(
        x, "a", attr = "href", attr_regex = "txt$") %>%
        {.[str_detect(., "vaccine")]}
    
    dev_null <- suppressWarnings(out <- read_tsv(
        tsv_src[1], skip = 1, col_types = cols()))
    
    out
}

west_virginia_vaccine_restruct <- function(x){
    df_ <- x
    
    if(!str_starts(names(df_)[1], "(?i)for week")){
        warning("Column names are not as expected. Please inspect.")
    }
    
    names(df_)[1] <- "Name"
    names(df_) <- paste(names(df_), df_[1, ], sep = "_")

    check_names(df_, c("name", "county", "pop", "both","johnson"), 
                detect = TRUE)
    
    names(df_) <- c(
        "Name", 
        "Drop.County", 
        "Drop.Population",
        "Moderna.Completed", 
        "Johnson"
        )
    
    df_[-1,]
}

west_virginia_vaccine_extract <- function(x){
    
    emp_idx <- first(which(str_detect(x$Name, "(?i)employee")))
    
    res_df <- x[1:(emp_idx-1),] %>%
        select(!starts_with("Drop")) %>%
        clean_scraped_df() %>% 
        mutate(Residents.Completed = vector_sum_na_rm(Moderna.Completed, Johnson)) %>% 
        filter(!across(c(Moderna.Completed, Johnson), ~ is.na(.x))) %>% 
        filter(!str_detect(Name, "(?i)total")) 
    
    staff_df <- x[emp_idx:nrow(x),] %>%
        filter(!str_detect(Drop.County, "(?i)total")) %>%
        mutate(Name = stringr::str_c(Drop.County, " staff total")) %>% 
        rename(Staff.Population = Drop.Population) %>% 
        select(!starts_with("Drop")) %>%
        clean_scraped_df() %>% 
        mutate(Staff.Completed = vector_sum_na_rm(Moderna.Completed, Johnson))
    
    if(nrow(res_df) != 34){
        warning(stringr::str_c(
            "Expected 34 resident rows, got ", nrow(res_df), 
            ". Check raw file for rows that should/shouldn't be dropped."))
    }
    if(nrow(staff_df) != 4){
        warning(stringr::str_c(
            "Expected 4 staff rows, got ", nrow(res_df), 
            ". Check raw file for rows that should/shouldn't be dropped."))
    }

    bind_rows(res_df, staff_df) %>% 
        select(Name, starts_with(c("Residents.", "Staff.")))
}

#' Scraper class for general West Virginia COVID data
#' 
#' @name west_virginia_vaccine_scraper
#' @description WV vaccine data resides in a tsv file hosted on the doc website. Staff
#' information is only provided at the state wide table and a number of coding
#' steps are necessary to extract the information. Stopped reporting initiated numbers 
#' around 6/11/2021. 
#' \describe{
#'   \item{Facility}{The facility name}
#'   \item{Staffing}{Staff.Population}
#'   \item{1st Doses}{Staff Initiated}
#'   \item{2nd Doses}{Staff Completed}
#'   \item{Vaccinated}{Residents.Initiated/Completed/Vadmin}
#' }

west_virginia_vaccine_scraper <- R6Class(
    "west_virginia_vaccine_scraper",
    inherit = generic_scraper,
    public = list(
        log = NULL,
        initialize = function(
            log,
            url = "https://dhhr.wv.gov/COVID-19/Pages/Correctional-Facilities.aspx",
            id = "west_virginia_vaccine",
            type = "csv",
            state = "WV",
            jurisdiction = "state",
            # pull the JSON data directly from the API
            pull_func = west_virginia_vaccine_pull,
            # restructuring the data means pulling out the data portion of the json
            restruct_func = west_virginia_vaccine_restruct,
            # Rename the columns to appropriate database names
            extract_func = west_virginia_vaccine_extract){
            super$initialize(
                url = url, id = id, pull_func = pull_func, type = type,
                restruct_func = restruct_func, extract_func = extract_func,
                log = log, state = state, jurisdiction = jurisdiction)
        }
    )
)

if(sys.nframe() == 0){
    west_virginia_vaccine <- west_virginia_vaccine_scraper$new(log=TRUE)
    west_virginia_vaccine$raw_data
    west_virginia_vaccine$pull_raw()
    west_virginia_vaccine$raw_data
    west_virginia_vaccine$save_raw()
    west_virginia_vaccine$restruct_raw()
    west_virginia_vaccine$restruct_data
    west_virginia_vaccine$extract_from_raw()
    west_virginia_vaccine$extract_data
    west_virginia_vaccine$validate_extract()
    west_virginia_vaccine$save_extract()
}

