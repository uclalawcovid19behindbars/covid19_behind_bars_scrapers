source("./R/generic_scraper.R")
source("./R/utilities.R")

connecticut_vaccine_pull <- function(x){
    ct_img2 <- xml2::read_html(x) %>%
        rvest::html_nodes("img") %>%
        rvest::html_attr("src") %>%
        # get by position because names are crazy
        .[3] %>%
        {str_c(x, .)}
    
    magick::image_read(ct_img2)
}

connecticut_vaccine_restruct <- function(x){
    in_txt <- magick::image_crop(x, "150x120+10+700") %>%
        magick::image_convert(type = 'Grayscale') %>%
        magick::image_ocr()
    
    if(!(str_detect(in_txt, "(?i)inmate") & str_detect(in_txt, "(?i)vacc"))){
        stop("Text not as expected for inmates, please inspect scrape")
    }
    
    st_txt <- magick::image_crop(x, "150x110+270+740") %>%
        magick::image_convert(type = 'Grayscale') %>%
        magick::image_ocr()
    
    if(!(str_detect(st_txt, "(?i)staff") & str_detect(st_txt, "(?i)vacc"))){
        stop("Text not as expected for staff, please inspect scrape")
    }
    
    tibble(
        Res = magick::image_crop(x, "150x80+28+852") %>%
            magick::image_convert(type = 'Grayscale') %>%
            magick::image_ocr(),
    
        Staff = magick::image_crop(x, "150x80+270+853") %>%
            magick::image_convert(type = 'Grayscale') %>%
            magick::image_ocr()
    )
}

connecticut_vaccine_extract <- function(x){
    x %>%
        mutate(Name = "STATEWIDE") %>%
        rename(Residents.Initiated = Res, Staff.Initiated = Staff) %>%
        clean_scraped_df()
}

#' Scraper class for general connecticut_vaccine COVID data
#' 
#' @name connecticut_vaccine_scraper
#' @description CT stores information within tables in images. The image
#' information appears to be consistent after the addition of vaccines
#' making OCR possible. There are a number of variables reported in the image
#' however we are only concerned with vaccine numbers in this case.
#' \describe{
#'   \item{COVID Vaccine Inmates}{Number of inmates who received vaccination}
#'   \item{COVID Vaccine staff}{Number of staff who received vaccination}
#' }

connecticut_vaccine_scraper <- R6Class(
    "connecticut_vaccine_scraper",
    inherit = generic_scraper,
    public = list(
        log = NULL,
        initialize = function(
            log,
            url = "https://portal.ct.gov/DOC/Common-Elements/Common-Elements/Health-Information-and-Advisories",
            id = "connecticut_vaccine",
            type = "img",
            state = "CT",
            jurisdiction = "state",
            # pull the JSON data directly from the API
            pull_func = connecticut_vaccine_pull,
            # restructuring the data means pulling out the data portion of the json
            restruct_func = connecticut_vaccine_restruct,
            # Rename the columns to appropriate database names
            extract_func = connecticut_vaccine_extract){
            super$initialize(
                url = url, id = id, pull_func = pull_func, type = type,
                restruct_func = restruct_func, extract_func = extract_func,
                log = log, state = state, jurisdiction = jurisdiction)
        }
    )
)

if(sys.nframe() == 0){
    connecticut_vaccine <- connecticut_vaccine_scraper$new(log=TRUE)
    connecticut_vaccine$raw_data
    connecticut_vaccine$pull_raw()
    connecticut_vaccine$raw_data
    connecticut_vaccine$save_raw()
    connecticut_vaccine$restruct_raw()
    connecticut_vaccine$restruct_data
    connecticut_vaccine$extract_from_raw()
    connecticut_vaccine$extract_data
    connecticut_vaccine$validate_extract()
    connecticut_vaccine$save_extract()
}

