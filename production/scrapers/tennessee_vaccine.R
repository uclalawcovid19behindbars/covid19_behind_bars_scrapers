source("./R/generic_scraper.R")
source("./R/utilities.R")

tennessee_vaccine_pull <- function(x){
    get_src_by_attr(
        x, "a", attr = "href", attr_regex = "TDOCInmatesCOVID19.pdf$")
}

tennessee_vaccine_restruct <- function(x){
    res_txt <- x %>% 
        magick::image_read_pdf(pages = 1) %>% 
        magick::image_crop("2000x175+500+2250") %>% 
        magick::image_ocr() %>% 
        str_to_upper()
    
    if (!str_detect(res_txt, "DOSES OF VACC") | !str_detect(res_txt, "OFFENDERS")){
        warning("Field does not match expected text")
    }
    
    res_vac <- as.numeric(str_c(unlist(
        str_extract_all(gsub(".*OFFENDERS", "", res_txt), "[0-9]+")), collapse = ""))
    
    staff_txt <- x %>% 
        magick::image_read_pdf(pages = 2) %>% 
        magick::image_crop("2200x175+800+800") %>% 
        magick::image_ocr() %>% 
        str_to_upper()
    
    if (!str_detect(staff_txt, "DOSES OF VACC") | !str_detect(staff_txt, "STAFF")){
        warning("Field does not match expected text")
    }
    
    staff_vac <- as.numeric(str_c(unlist(
        str_extract_all(gsub(".*STAFF", "", staff_txt), "[0-9]+")), collapse = ""))
    
    tibble(Residents.Vadmin = res_vac, 
           Staff.Vadmin = staff_vac)
}

tennessee_vaccine_extract <- function(x){
    x %>% 
        mutate(Name = "STATEWIDE") %>% 
        clean_scraped_df()
}

#' Scraper class for general Tennessee vaccine data
#' 
#' @name tennessee_vaccine_scraper
#' @description Statewide totals for doses of vaccination administered to 
#' incarcerated people and staff.  
#' \describe{
#'   \item{Doses of vaccination administered offenders}{Residents.Vadmin}
#'   \item{Doses of vaccination administered staff}{Staff.Vadmin}
#' }

tennessee_vaccine_scraper <- R6Class(
    "tennessee_vaccine_scraper",
    inherit = generic_scraper,
    public = list(
        log = NULL,
        initialize = function(
            log,
            url = "https://www.tn.gov/correction.html",
            id = "tennessee_vaccine",
            type = "pdf",
            state = "TN",
            jurisdiction = "state",
            # pull the JSON data directly from the API
            pull_func = tennessee_vaccine_pull,
            # restructuring the data means pulling out the data portion of the json
            restruct_func = tennessee_vaccine_restruct,
            # Rename the columns to appropriate database names
            extract_func = tennessee_vaccine_extract){
            super$initialize(
                url = url, id = id, pull_func = pull_func, type = type,
                restruct_func = restruct_func, extract_func = extract_func,
                log = log, state = state, jurisdiction = jurisdiction)
        }
    )
)

if(sys.nframe() == 0){
    tennessee_vaccine <- tennessee_vaccine_scraper$new(log=TRUE)
    tennessee_vaccine$raw_data
    tennessee_vaccine$pull_raw()
    tennessee_vaccine$raw_data
    tennessee_vaccine$save_raw()
    tennessee_vaccine$restruct_raw()
    tennessee_vaccine$restruct_data
    tennessee_vaccine$extract_from_raw()
    tennessee_vaccine$extract_data
    tennessee_vaccine$validate_extract()
    tennessee_vaccine$save_extract()
}

