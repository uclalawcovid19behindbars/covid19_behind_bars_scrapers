source("./R/generic_scraper.R")
source("./R/utilities.R")

maine_pull <- function(x){
    get_src_by_attr(x, "a", attr = "href", attr_regex = "(?i)dashboard")
}

maine_restruct <- function(x){
    ExtractTable(magick::image_read_pdf(x, pages = 1))
}

maine_extract <- function(x){
    ad_pop_idx <- which(sapply(x, function(z){
        any(str_detect(z[,1], "(?i)Adult Facility"))}))
    
    res_test_idx <- which(sapply(x, function(z){
        any(str_detect(z[,2], "(?i)test"))}))
    
    juv_pop_idx <- which(sapply(x, function(z){
        any(str_detect(z[,1], "(?i)Juvenile")) &
            any(str_detect(z[,1], "(?i)population"))}))
    
    ad_pop_df <- x[[ad_pop_idx]] %>% 
        .[str_detect(.[,1], "(?i)total population"),2] %>% 
        as.numeric() %>%
        {tibble(Name = "State-Wide", Residents.Population = .)}
    
    juv_pop <- x[[juv_pop_idx]] %>% 
        .[str_detect(.[,1], "(?i)total population"),2] %>%
        as.numeric()
    
    df_ <- as.data.frame(x[[res_test_idx]])
    
    col_name_mat <- matrix(c(
        "Adult Facilities - Resident", "0", "Name",
        "Testing", "1", "Residents.Tadmin",
        "", "2", "Residents.Confirmed"
    ), ncol = 3, nrow = 3, byrow = TRUE)
    
    colnames(col_name_mat) <- c("check", "raw", "clean")
    col_name_df <- as_tibble(col_name_mat)
    
    check_names_extractable(df_, col_name_df)
    out_df <- rename_extractable(df_, col_name_df) %>%
        as_tibble() %>%
        {suppressWarnings(mutate_at(., vars(starts_with("Res")), as.numeric))} %>%
        filter(!(Name == "" | is.na(Residents.Tadmin))) %>%
        filter(!str_detect(Name, "(?i)total")) %>%
        mutate(Residents.Population = ifelse(
            str_detect(Name, "(?i)youth"),
            juv_pop,
            NA_real_)) %>%
        full_join(ad_pop_df, by = c("Name", "Residents.Population"))
    
    if(sum(str_detect(out_df$Name, "(?i)youth")) != 1){
        warning("Not as many youth detention centers as expected.")
    }
    
    out_df
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
            # pull the JSON data directly from the API
            pull_func = maine_pull,
            # restructuring the data means pulling out the data portion of the json
            restruct_func = maine_restruct,
            # Rename the columns to appropriate database names
            extract_func = maine_extract){
            super$initialize(
                url = url, id = id, pull_func = pull_func, type = type,
                restruct_func = restruct_func, extract_func = extract_func,
                log = log, state = state, jurisdiction = jurisdiction)
        }
    )
)

if(sys.nframe() == 0){
    maine <- maine_scraper$new(log=TRUE)
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

