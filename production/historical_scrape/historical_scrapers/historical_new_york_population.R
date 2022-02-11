source("./R/generic_scraper.R")
source("./R/utilities.R")

historical_ny_pop_restruct <- function(file, date){
    file <- "https://doccs.ny.gov/system/files/documents/2021/12/august-1-2021-inmate-profile.pdf"
    restruct <- file %>%
        magick::image_read_pdf(pages = 1) %>%
        magick::image_crop("2500x2700+0+125") %>%
        ExtractTable() 
        
    return(restruct)
}

historical_ny_pop_extract <- function(restruct){
    x_ <- restruct %>% 
        as.data.frame() %>%
        rowid_to_column()
    
    out <- x_ %>% 
        select(name_prefix = X0,
               Name = X1,
               Residents.Population = X13,
        ) %>% 
        mutate(Name = clean_fac_col_txt(Name, to_upper = TRUE), 
               Name = str_replace_all(Name, "[:digit:]", "")) %>% 
        mutate(Name = ifelse(str_detect(Name, "ATTICA"), "ATTICA", Name),
               Name = ifelse(str_detect(Name, "HALE CREEK"), "HALE CREEK", Name),
               Name = ifelse(str_detect(Name, "MARCY"), "MARCY", Name),
               Name = ifelse(str_detect(Name, "TACONIC"), "TACONIC", Name),
               Name = ifelse(str_detect(Name, "MID-STATE"), "MIDSTATE", Name),
               Name = ifelse(str_detect(Name, "MOHAWK"), "MOHAWK WALSH REGIONAL MEDICAL UNIT", Name),
               Name = ifelse(str_detect(Name, "WALSH"), "MOHAWK WALSH REGIONAL MEDICAL UNIT", Name),
               Name = str_replace(Name, " GEN", ""),
               Name = str_replace(Name, " WR", ""),
               Name = str_replace(Name, " SHOCK", ""),
               Name = str_replace(Name, " SENIOR", ""),
               Name = str_replace(Name, " SEP UNIT", ""),
               Name = str_replace(Name, " IT", ""),
               Name = str_replace(Name, " RMHU", ""),
               Name = str_replace(Name, " REL", ""),
               Name = str_replace(Name, " MEDIUM", ""),
               Name = str_replace(Name, " CASAT", ""),
               Name = str_replace(Name, " MAX", ""),
               Name = str_replace(Name, "-F WORK REL", ""),
               Name = str_replace(Name, "-F", ""),
               Name = str_replace(Name, " RELAPSE", ""),
               Name = str_replace(Name, " ASACTC", ""),
               Name = str_replace(Name, " WORK REL", ""),
               Name = str_replace(Name, " SH TR", ""),
               Name = str_replace(Name, " RMHUOSOTP", ""),
               Name = str_replace(Name, " WORK", ""),
               Name = str_replace(Name, " MEDICAL", "")
        ) %>% 
        filter(!str_detect(Name, "TOTAL"),
               !str_detect(Name, "ALL SHU")) %>% 
        mutate(Name = clean_fac_col_txt(Name),
               Residents.Population = as.numeric(Residents.Population),
               Residents.Population = ifelse(Name == "TACONIC", 
                                             163, Residents.Population)) %>%
        group_by(Name) %>% 
        summarise(Residents.Population = sum_na_rm(Residents.Population)) %>% 
        ungroup() %>%
        mutate(Jurisdiction = "state",
               State = "New York",
               Date.Population = "2021-08-01")
    
    clean_out <- clean_facility_name(out) %>%
        select(-jurisdiction_scraper)
    return(clean_out)
    
}

#' Scraper class for Alabama historical population data 
#' 
#' @name historical_ny_pop_scraper
#' @description Alabama's DOC posts monthly statistical reports in PDF form and 
#' archives these reports. They are long (16+ pages) and provide a lot of 
#' detailed information that isn't scraped. These reports are published 2-3 
#' months ex-post.  
#' 
#' \describe{
#'   \item{Facility}{}
#'   \item{Designed Capacity}{}
#'   \item{Current Beds}{}
#'   \item{Month End Population{}
#'   \item{Difference{}
#'   \item{Occupancy Rate{}
#' }

historical_ny_pop_scraper <- R6Class(
    "historical_nyabama_pop_scraper",
    inherit = generic_scraper,
    public = list(
        log = NULL,
        initialize = function(
            log,
            url = "http://www.doc.state.al.us/StatReports",
            id = "historical_ny_pop",
            type = "pdf",
            state = "AL",
            jurisdiction = "state",
            check_date = NULL,
            pull_func = function(x, file, date = NULL){file},
            restruct_func = historical_ny_pop_restruct,
            extract_func = historical_ny_pop_extract){
            super$initialize(
                url = url, id = id, pull_func = pull_func, type = type,
                restruct_func = restruct_func, extract_func = extract_func,
                log = log, state = state, jurisdiction  = jurisdiction,
                check_date = check_date)
        }
    )
)

if(sys.nframe() == 0){
    historical_ny_pop <- historical_ny_pop_scraper$new(log=TRUE)
    # historical_ny_pop$reset_date("DATE")
    historical_ny_pop$raw_data
    historical_ny_pop$pull_raw(file, .dated_pull = TRUE)
    historical_ny_pop$raw_data
    historical_ny_pop$save_raw()
    historical_ny_pop$restruct_raw()
    historical_ny_pop$restruct_data
    historical_ny_pop$extract_from_raw()
    historical_ny_pop$extract_data
    historical_ny_pop$validate_extract()
    historical_ny_pop$save_extract()
}

