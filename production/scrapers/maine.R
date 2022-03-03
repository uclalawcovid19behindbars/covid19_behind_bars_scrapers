source("./R/generic_scraper.R")
source("./R/utilities.R")

maine_pull <- function(x){
    get_src_by_attr(x, "a", attr = "href", attr_regex = "(?i)dashboard")
}

maine_restruct <- function(x){
    list(pop = x %>% 
             magick::image_read_pdf(pages = 1) %>% 
             magick::image_crop("900x2200+240+600") %>% 
             magick::image_convert(type = 'Bilevel') %>%
             ExtractTable(), 
         vaccines = x %>% 
             magick::image_read_pdf(pages = 1) %>% 
             magick::image_crop("1300x500+1100+1500") %>% 
             magick::image_convert(type = 'Bilevel') %>% 
             ExtractTable(), 
         cases = x %>% 
             magick::image_read_pdf(pages = 1) %>% 
             magick::image_crop("1300x920+1100+600") %>% 
             magick::image_convert(type = 'Bilevel') %>% 
             ExtractTable()
         )
}

maine_extract <- function(x){
    # Population 
    ad_pop_idx <- which(sapply(x$pop, function(z){
        any(str_detect(z[,1], "(?i)capacity"))}))
    
    juv_pop_idx <- which(sapply(x$pop, function(z){
        any(str_detect(z[,1], "(?i)Juvenile Facility"))}))
    
    pop_df <- bind_rows(
        x$pop[[ad_pop_idx]] %>% 
            .[str_detect(.[,1], "(?i)total population"),2] %>% 
            as.numeric() %>%
            {tibble(Name = "Adult Residents", Residents.Population = .)}, 
        
        x$pop[[juv_pop_idx]] %>% 
            .[str_detect(.[,1], "(?i)total population"),2] %>%
            as.numeric() %>% 
            {tibble(Name = "Juvenile Residents", Residents.Population = .)}
    )
    
    # Vaccines 
    people_idx <- which(sapply(x$vaccines, function(z){
        any(str_detect(z[,1], "(?i)juvenile"))}))
    
    vaccines_ <- x$vaccines[[people_idx]] 
    
    vax_col_name_mat <- matrix(c(
        "", "0", "Name",
        "Fully Vaccinated (%)", "1", "Perc.Fully.Vax"
    ), ncol = 3, nrow = 2, byrow = TRUE)
    
    colnames(vax_col_name_mat) <- c("check", "raw", "clean")
    vax_col_name_df <- as_tibble(vax_col_name_mat)
    check_names_extractable(vaccines_, vax_col_name_df)
    
    vaccines_df <- rename_extractable(vaccines_, vax_col_name_df) %>%
        as_tibble() %>% 
        filter(Name != "",
               Name != "All Facilities - Resident") %>%
        mutate(Residents.Completed.Pct = as.numeric(gsub("[\\%,]", "", Perc.Fully.Vax)) / 100) %>%
        select(-Perc.Fully.Vax)
    
    # cases (2022 YTD)
    cases <- x$cases[[1]] 
    
    col_name_mat <- matrix(c(
        "Adult Facilities - Resident", "0", "Name",
        "Testing*", "1", "Residents.Confirmed_since22.Drop"
    ), ncol = 3, nrow = 2, byrow = TRUE)
    
    colnames(col_name_mat) <- c("check", "raw", "clean")
    col_name_df <- as_tibble(col_name_mat)
    
    check_names_extractable(cases, col_name_df)
    
    ## read in last known values from 2021 to add to make true cumulative
    cumulative_old <- read_csv("http://104.131.72.50:3838/scraper_data/extracted_data/2021-12-27_maine.csv") %>%
        select(Name, 
               Residents.Confirmed_end21.Drop = Residents.Confirmed)
    
    out <- rename_extractable(cases, col_name_df) %>%
        as_tibble() %>%
        {suppressWarnings(mutate_at(., vars(starts_with("Res")), as.numeric))} %>%
        filter(!str_detect(Name, "(?i)total"),
               !str_detect(Name, "(?i)adult facilities"),
               !Name %in% "") %>% 
        clean_scraped_df() %>%
        full_join(cumulative_old, by = "Name") %>% 
        full_join(pop_df, by = "Name") %>% 
        full_join(vaccines_df, by = "Name") %>% 
        mutate(Residents.Completed = round(Residents.Population * Residents.Completed.Pct),
               Residents.Confirmed = vector_sum_na_rm(Residents.Confirmed_end21.Drop,Residents.Confirmed_since22.Drop)) %>%
        select(-ends_with("Drop")) %>% 
        clean_scraped_df()
    
    return(out)
}

#' Scraper class for general maine COVID data
#' 
#' @name maine_scraper
#' @description This will be a description of maine data and what the scraper
#' does
#' \describe{
#'   \item{Facility_Name}{The faciilty name.}
#' }

maine_scraper <- R6Class(
    "maine_scraper",
    inherit = generic_scraper,
    public = list(
        log = NULL,
        initialize = function(
            log,
            url = "https://www.maine.gov/corrections/covid-19-resources",
            id = "maine",
            type = "pdf",
            state = "ME",
            jurisdiction = "state",
            check_date = NULL,
            # pull the JSON data directly from the API
            pull_func = maine_pull,
            # restructuring the data means pulling out the data portion of the json
            restruct_func = maine_restruct,
            # Rename the columns to appropriate database names
            extract_func = maine_extract){
            super$initialize(
                url = url, id = id, pull_func = pull_func, type = type,
                restruct_func = restruct_func, extract_func = extract_func,
                log = log, state = state, jurisdiction = jurisdiction,
                check_date = check_date)
        }
    )
)

if(sys.nframe() == 0){
    maine <- maine_scraper$new(log=TRUE)
    maine$run_check_date()
    maine$raw_data
    maine$pull_raw()
    maine$raw_data
    maine$save_raw()
    maine$restruct_raw()
    maine$restruct_data
    maine$extract_from_raw()
    maine$extract_data
    maine$validate_extract()
    maine$save_extract()
}

