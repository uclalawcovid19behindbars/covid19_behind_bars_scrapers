source("./R/generic_scraper.R")
source("./R/utilities.R")

connecticut_vaccine_check_date <- function(url, date = Sys.Date()){
    ct_img2 <- xml2::read_html(url) %>%
        rvest::html_nodes("img") %>%
        rvest::html_attr("src") %>%
        .[3] %>%
        {str_c(url, .)} %>%
        magick::image_read()
    
    ct_img2 %>%
        magick::image_ocr() %>%
        str_split("\n") %>%
        unlist() %>%
        {.[str_detect(., "(?i)posted")]} %>%
        str_extract("\\d{1,2}/\\d{1,2}/\\d{2,4}") %>%
        lubridate::mdy() %>%
        error_on_date(date)
}

connecticut_vaccine_pull <- function(url){
    ct_img_target <- xml2::read_html(url) %>%
        rvest::html_nodes("img") %>%
        rvest::html_attr("src") %>%
        # get by position because names are crazy
        .[3] %>%
        {str_c(url, .)}
    
    ct_image <- magick::image_read(ct_img_target)
}

connecticut_vaccine_restruct <- function(ct_image){
    ct_image_brightened <- magick::image_trim(ct_image) %>% 
        magick::image_modulate(brightness = 120)
    
    w_ <- magick::image_info(ct_image_brightened)$width
    h_ <- magick::image_info(ct_image_brightened)$height

    if (h_ >= 1000){
        ##### h = 1650, w = 700
        h_txt <- 250
        w_txt <- 250
        yoff_txt <- 1240
        res_xoff_txt <- 50
        st_xoff_txt <- 450
    }
    else{
        ##### h = 944, w = 422
        h_txt <- 160
        w_txt <- 200
        yoff_txt <- 740
        res_xoff_txt <- 10
        st_xoff_txt <- 250
    }
    
    in_txt <- magick::image_crop(ct_image_brightened, str_c(w_txt, "x", h_txt, "+", 
                                          res_xoff_txt, "+", yoff_txt)) %>%
        magick::image_convert(type = 'Grayscale') %>%
        magick::image_ocr()
    
    if(!(str_detect(in_txt, "(?i)inmate") & str_detect(in_txt, "(?i)vacc"))){
        stop("Text not as expected for inmates, please inspect scrape")
    }
    
    st_txt <- magick::image_crop(ct_image_brightened, str_c(w_txt, "x", h_txt, "+", 
                                          st_xoff_txt, "+", yoff_txt)) %>%
        magick::image_modulate(brightness = 200) %>% 
        magick::image_ocr()
    
    if(!(str_detect(st_txt, "(?i)staff") & str_detect(st_txt, "(?i)vacc"))){
        stop("Text not as expected for staff, please inspect scrape")
    }
    
    w_num <- w_txt
    h_num <- h_txt - (h_txt / 4)    
    res_xoff_num <- (res_xoff_txt * 1/4) + res_xoff_txt
    st_xoff_num <- st_xoff_txt
    yoff_num <- round(h_ * .9)
        
    out <- tibble(
        Res = magick::image_crop(ct_image_brightened, str_c(w_num, "x", h_num, "+",
                                          res_xoff_num, "+", yoff_num)) %>%
            magick::image_convert(type = 'Grayscale') %>%
            magick::image_ocr() %>%
            string_to_clean_numeric(),
        Staff = magick::image_crop(ct_image_brightened, str_c(w_num, "x", h_num, "+",
                                          st_xoff_num, "+", yoff_num)) %>%
            magick::image_convert(type = 'Grayscale') %>%
            magick::image_ocr() %>%
            string_to_clean_numeric()
    )
    
    if(out$Res < out$Staff){
        stop("Vaccination numbers not as expected, please inspect")
    }
    
    if(out$Res < 5000 | out$Staff < 3000){
        stop("Vaccination numbers lower than expected, please inspect")
    }
    
    return(out)
    
}

connecticut_vaccine_extract <- function(x){
    x %>%
        mutate_all(function(x) str_replace_all(x, "\\.", ",")) %>%
        mutate(Name = "STATEWIDE") %>%
        rename(Residents.Initiated = Res, 
               Staff.Initiated = Staff) %>%
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
            check_date = connecticut_vaccine_check_date,
            # pull the JSON data directly from the API
            pull_func = connecticut_vaccine_pull,
            # restructuring the data means pulling out the data portion of the json
            restruct_func = connecticut_vaccine_restruct,
            # Rename the columns to appropriate database names
            extract_func = connecticut_vaccine_extract){
            super$initialize(
                url = url, id = id, pull_func = pull_func, type = type,
                restruct_func = restruct_func, extract_func = extract_func,
                log = log, state = state, jurisdiction = jurisdiction,
                check_date = check_date)
        }
    )
)

if(sys.nframe() == 0){
    connecticut_vaccine <- connecticut_vaccine_scraper$new(log=TRUE)
    connecticut_vaccine$run_check_date()
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

