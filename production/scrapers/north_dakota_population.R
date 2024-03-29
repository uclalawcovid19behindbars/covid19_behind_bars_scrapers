source("./R/generic_scraper.R")
source("./R/utilities.R")

nd_pop_check_date <- function(url, date = Sys.Date()){
    img <- magick::image_read_pdf(url, pages = 1) 
    
    date_box <- magick::image_crop(img, "500x240+2000+160") %>% 
        magick::image_ocr() 
    
    date_box %>%
        {.[str_detect(., "(?i)20")]} %>% # look for year 20xx
        str_extract("\\d{1,2}/\\d{1,2}/\\d{2,4}") %>%
        lubridate::mdy() %>%
        error_on_date(expected_date = date)
}

nd_pop_crop <- function(img, crop, detect){

    sub_txt <- img %>% 
        magick::image_crop(crop) %>% 
        magick::image_ocr() %>% 
        first()
    
    if(!str_detect(sub_txt, detect)){
        warning("Field does mot match expected text")
    }
    
    out_val <- as.numeric(
        str_c(unlist(str_extract_all(sub_txt, "[0-9]+")), collapse = ""))

    if(is.na(out_val)){
        warning("Na value extracted but not expected. Please inspect.")
    }
    
    out_val
}

nd_pop_restruct <- function(x){
    img <- magick::image_read_pdf(x) 
    
    # Hard code crop dimensions (LASD scraper approach)
    bind_rows(
        tibble(Name = "NDSP", 
               Residents.Population = nd_pop_crop(img, "700x140+20+460", "NDSP")), 
        tibble(Name = "JRCC", 
               Residents.Population = nd_pop_crop(img, "700x140+20+670", "JRCC")), 
        tibble(Name = "JRMU", 
               Residents.Population = nd_pop_crop(img, "700x140+20+790", "JRMU")), 
        tibble(Name = "MRCC", 
               Residents.Population = nd_pop_crop(img, "700x140+20+1010", "MRCC")), 
        tibble(Name = "DWCRC", 
               Residents.Population = nd_pop_crop(img, "700x140+20+1230", "DWCRC")) 
        # Not reported on 7/2/21 - commented out in case that changes 
        # tibble(Name = "TRC", 
        #        Residents.Population = nd_pop_crop(img, "700x140+20+1440", "TRC"))
    )
}

#' Scraper class for general North Dakota population data
#' tibble(
#' @name nd_pop_scraper
#' @description North Dakota posts daily pdf w/ facility-level population data. 
#' The scraper crops out the relevant text based on position on the pdf. 
#' 
#' \describe{
#'   \item{Name}{}
#'   \item{Gender}{}
#'   \item{Operational Capacity Daily Count}{Residents.Population}
#' }

north_dakota_population_scraper <- R6Class(
    "north_dakota_population",
    inherit = generic_scraper,
    public = list(
        log = NULL,
        initialize = function(
            log,
            url = "https://www.docr.nd.gov/sites/www/files/documents/reports/FACILITY_COUNTS.pdf",
            id = "north_dakota_population",
            type = "pdf",
            state = "ND",
            jurisdiction = "state",
            check_date = nd_pop_check_date,
            # pull the JSON data directly from the API
            pull_func = function(x){x}, 
            # restructuring the data means pulling out the data portion of the json
            restruct_func = nd_pop_restruct,
            # Rename the columns to appropriate database names
            extract_func = function(x){x}){
            super$initialize(
                url = url, id = id, pull_func = pull_func, type = type,
                restruct_func = restruct_func, extract_func = extract_func,
                log = log, state = state, jurisdiction = jurisdiction,
                check_date = check_date)
        }
    )
)

if(sys.nframe() == 0){
    nd_pop <- north_dakota_population_scraper$new(log=TRUE)
    nd_pop$run_check_date()
    nd_pop$raw_data
    nd_pop$pull_raw()
    nd_pop$raw_data
    nd_pop$save_raw()
    nd_pop$restruct_raw()
    nd_pop$restruct_data
    nd_pop$extract_from_raw()
    nd_pop$extract_data
    nd_pop$validate_extract()
    nd_pop$save_extract()
}
