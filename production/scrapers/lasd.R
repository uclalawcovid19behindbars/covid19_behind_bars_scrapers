source("./R/generic_scraper.R")
source("./R/utilities.R")

lasd_date_check <- function(x, date = Sys.Date()){
    get_src_by_attr(
        x, "img", attr = "src", attr_regex = "(?i)fact.?sheet") %>%
        magick::image_read() %>% 
        magick::image_crop("500x50+700+0") %>% 
        magick::image_ocr() %>% 
        str_extract("\\d{1,2}/\\d{1,2}/\\d{2,4}") %>% 
        lubridate::mdy() %>% 
        error_on_date(date)
}

lasd_crop <- function(img, crop, detect = "", rimg = FALSE){
    sub_img <- img %>%
        magick::image_crop(crop)
    
    if(rimg){
        return(sub_img)
    }
    
    sub_txt <- sub_img %>%
        tesseract::ocr() %>%
        str_remove_all("(?i)covid.?19") %>%
        clean_fac_col_txt()
    
    if(!str_detect(sub_txt, detect)){
        warning("Field does mot match expected text")
    }
    
    out_val <- as.numeric(
        str_c(unlist(str_extract_all(sub_txt, "[0-9]+")), collapse = ""))
    
    if(is.na(out_val)){
        warning("Na value extracted but not expected. Please inspect.")
    }
    
    return(out_val)
}

lasd_pull <- function(x, wait = 5){
   get_src_by_attr(
       x, "img", attr = "src", attr_regex = "(?i)fact.?sheet") %>%
        magick::image_read()
}

lasd_restruct <- function(x){
    
    # Trim whitespace and brighten image 
    x <- x %>%
        #magick::image_read() %>%
        magick::image_trim() %>% 
        magick::image_modulate(brightness = 120)
    
    w_ <- magick::image_info(x)$width
    h_ <- magick::image_info(x)$height
    
    confirmed.crop <- "570x30+580+400"
    recover.crop <- '570x30+580+710'
    death.crop <- '570x30+580+780'
    quarantine.crop <- "570x30+590+950"
    drop.neg.asymp.1.crop <- "570x25+600+562"
    drop.neg.asymp.2.crop <- "570x25+600+585"
    drop.neg.symp.crop <- "570x25+0+562"
    drop.pos.asymp.crop <- '570x30+580+502'
    drop.pos.symp.crop <- "562x25+0+508"
    drop.test.asymp.1.crop <- '570x30+580+605'
    drop.test.asymp.2.crop <- '570x30+580+635'
    drop.test.symp.crop <- "562x25+00+590"
    pop.crop <- "562x25+00+209"
    
    
    # If image is way too big or small, resize it  
    if (h_ > 1700 | h_ < 1000){
        x <- magick::image_scale(x, "1200")
        
        w_ <- magick::image_info(x)$width
        h_ <- magick::image_info(x)$height
    }
    
    out <- tibble(
        Residents.Confirmed = lasd_crop(x, confirmed.crop, "(?i)total pos"),
        Residents.Recovered = lasd_crop(x, recover.crop, "(?i)recover"), 
        Residents.Deaths = lasd_crop(x, death.crop, "(?i)deaths"), 
        Residents.Quarantine = lasd_crop(x, quarantine.crop, "(?i)central"),
        drop.neg.asymp.1 = lasd_crop(x, drop.neg.asymp.1.crop, "(?i)negative"),
        drop.neg.asymp.2 = lasd_crop(x, drop.neg.asymp.2.crop, ""),
        drop.neg.symp = lasd_crop(x, drop.neg.symp.crop, "(?i)negative"),
        drop.pos.asymp = lasd_crop(x, drop.pos.asymp.crop, "(?i)current"), 
        drop.pos.symp = lasd_crop(x, drop.pos.symp.crop, "(?i)current"),
        drop.test.asymp.1 = lasd_crop(x, drop.test.asymp.1.crop, "(?i)total"),
        drop.test.asymp.2 = lasd_crop(x, drop.test.asymp.2.crop, ""),
        drop.test.symp = lasd_crop(x, drop.test.symp.crop, "(?i)total"),
        Residents.Population = lasd_crop(x, pop.crop, "(?i)jail pop"))
    
    out <- out %>%
        mutate(drop.test.asymp = as.numeric(str_c(drop.test.asymp.1, drop.test.asymp.2)),
               drop.neg.asymp = as.numeric(str_c(drop.neg.asymp.1, drop.neg.asymp.2))) %>%
        select(-c(drop.test.asymp.1, drop.test.asymp.2, drop.neg.asymp.1, drop.neg.asymp.2))
    
    return(out)
}

lasd_extract <- function(x){
    out_df <- x %>%
        mutate(Residents.Negative = drop.neg.asymp + drop.neg.symp) %>%
        mutate(Residents.Active = drop.pos.asymp + drop.pos.symp) %>%
        mutate(Residents.Tadmin = drop.test.asymp + drop.test.symp) %>% 
        select(-starts_with("drop")) %>%
        mutate(Name = "LA Jail")
    
    if(out_df$Residents.Deaths != 18){
        warning("You sure LA shouldnt be 18?")
    }
    
    out_df
}

#' Scraper class for general LASD staff COVID data
#' tibble(
#' @name lasd_scraper
#' @description Info comes from an image file hosted on the website however
#' scraping the image is extremely difficult and has required multiple
#' alterations in the past. Need to frequently monitor this scraper for errors.
#' \describe{
#'   \item{Residents.Confirmed}{}
#'   \item{Residents.Recovered }{}
#'   \item{Residents.Deaths}{}
#'   \item{Residents.Quarantine}{}
#'   \item{neg.asymp}{Negative Asymptomatic}
#'   \item{neg.symp}{Negative Symptomatic}
#'   \item{pos.asymp}{Positive Asymptomatic}
#'   \item{pos.symp}{Positive Symptomatic}
#'   \item{Residents.Population}{}
#' }

lasd_scraper <- R6Class(
    "lasd_scraper",
    inherit = generic_scraper,
    public = list(
        log = NULL,
        initialize = function(
            log,
            url = "https://lasd.org/covid19updates/",
            id = "lasd",
            type = "img",
            state = "CA",
            jurisdiction = "county",
            check_date = lasd_date_check,
            # pull the JSON data directly from the API
            pull_func = lasd_pull,
            # restructuring the data means pulling out the data portion of the json
            restruct_func = lasd_restruct,
            # Rename the columns to appropriate database names
            extract_func = lasd_extract){
            super$initialize(
                url = url, id = id, pull_func = pull_func, type = type,
                restruct_func = restruct_func, extract_func = extract_func,
                log = log, state = state, jurisdiction = jurisdiction,
                check_date = check_date)
        }
    )
)

if(sys.nframe() == 0){
    lasd <- lasd_scraper$new(log=TRUE)
    lasd$run_check_date()
    lasd$raw_data
    lasd$pull_raw()
    lasd$raw_data
    lasd$save_raw()
    lasd$restruct_raw()
    lasd$restruct_data
    lasd$extract_from_raw()
    lasd$extract_data
    lasd$validate_extract()
    lasd$save_extract()
}

