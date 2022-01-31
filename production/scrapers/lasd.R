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
    x <- magick::image_trim(x) %>% 
        magick::image_modulate(brightness = 120)
    
    w_ <- magick::image_info(x)$width
    h_ <- magick::image_info(x)$height
    
    # If image is way too big or small, resize it  
    if (h_ > 1700 | h_ < 1000){
        x <- magick::image_scale(x, "1200")
        
        w_ <- magick::image_info(x)$width
        h_ <- magick::image_info(x)$height
    }
    
    if (h_ <= 1380){
        out <- tibble(
            Residents.Confirmed = lasd_crop(x, "570x30+580+400", "(?i)total pos"),
            Residents.Recovered = lasd_crop(x, "570x30+600+660", "(?i)recover"),
            Residents.Deaths = lasd_crop(x, "570x30+600+734", "(?i)deaths"),
            Residents.Quarantine = lasd_crop(x, "570x30+590+950", "(?i)total"),
            drop.neg.asymp = lasd_crop(x, "570x25+600+562", "(?i)negative"),
            drop.neg.symp = lasd_crop(x, "570x25+0+562", "(?i)negative"),
            drop.pos.asymp = lasd_crop(x, "500x25+600+508", "(?i)current"),
            drop.pos.symp = lasd_crop(x, "562x25+0+508", "(?i)current"),
            drop.test.asymp = lasd_crop(x, "562x25+600+590", "(?i)total"),
            drop.test.symp = lasd_crop(x, "562x25+00+590", "(?i)total"),
            Residents.Population = lasd_crop(x, "562x25+00+209", "(?i)jail pop"))
    }
    
    else if (h_ <= 1420){
        out <- tibble(
            Residents.Confirmed = lasd_crop(x, "570x30+580+400", "(?i)total pos"),
            Residents.Recovered = lasd_crop(x, "570x30+600+660", "(?i)recover"),
            Residents.Deaths = lasd_crop(x, "570x30+600+734", "(?i)deaths"),
            Residents.Quarantine = lasd_crop(x, "570x30+600+975", "(?i)total"),
            drop.neg.asymp = lasd_crop(x, "570x25+600+562", "(?i)negative"),
            drop.neg.symp = lasd_crop(x, "570x25+0+562", "(?i)negative"),
            drop.pos.asymp = lasd_crop(x, "500x25+600+508", "(?i)current"),
            drop.pos.symp = lasd_crop(x, "562x25+0+508", "(?i)current"),
            drop.test.asymp = lasd_crop(x, "562x25+600+590", "(?i)total"),
            drop.test.symp = lasd_crop(x, "562x25+00+590", "(?i)total"),
            Residents.Population = lasd_crop(x, "562x25+00+209", "(?i)jail pop"))
    }
    
    else if(h_ <= 1447){
        out <- tibble(
            Residents.Confirmed = lasd_crop(x, "570x30+580+400", "(?i)total pos"),
            Residents.Recovered = lasd_crop(x, "570x30+600+660", "(?i)recover"),
            Residents.Deaths = lasd_crop(x, "570x30+600+732", "(?i)deaths"),
            Residents.Quarantine = lasd_crop(x, "570x30+600+1004", "(?i)total"),
            drop.neg.asymp = lasd_crop(x, "570x25+600+562", "(?i)negative"),
            drop.neg.symp = lasd_crop(x, "570x25+0+563", "(?i)negative"),
            drop.pos.asymp = lasd_crop(x, "500x25+600+508", "(?i)current"),
            drop.pos.symp = lasd_crop(x, "562x25+0+508", "(?i)current"),
            drop.test.asymp = lasd_crop(x, "562x25+600+590", "(?i)total"),
            drop.test.symp = lasd_crop(x, "562x25+00+590", "(?i)total"),
            Residents.Population = lasd_crop(x, "562x25+00+209", "(?i)jail pop"))
    }
    
    else if(h_ <= 1460){
        out <- tibble(
            Residents.Confirmed = lasd_crop(x, "570x30+580+400", "(?i)total pos"),
            Residents.Recovered = lasd_crop(x, "570x30+600+660", "(?i)recover"),
            Residents.Deaths = lasd_crop(x, "570x30+600+735", "(?i)deaths"),
            Residents.Quarantine = lasd_crop(x, "570x30+595+1025", "(?i)total"),
            drop.neg.asymp = lasd_crop(x, "570x25+600+565", "(?i)negative"),
            drop.neg.symp = lasd_crop(x, "570x25+0+565", "(?i)negative"),
            drop.pos.asymp = lasd_crop(x, "500x25+600+508", "(?i)current"),
            drop.pos.symp = lasd_crop(x, "562x25+0+508", "(?i)current"),
            drop.test.asymp = lasd_crop(x, "562x25+595+590", "(?i)total"),
            drop.test.symp = lasd_crop(x, "562x25+00+590", "(?i)total"),
            Residents.Population = lasd_crop(x, "562x25+00+209", "(?i)jail pop"))
    }
    
    else if(h_ <= 1485){
        out <- tibble(
            Residents.Confirmed = lasd_crop(x, "570x30+580+400", "(?i)total pos"),
            Residents.Recovered = lasd_crop(x, "570x30+590+660", "(?i)recover"),
            Residents.Deaths = lasd_crop(x, "570x30+590+734", "(?i)deaths"),
            Residents.Quarantine = lasd_crop(x, "570x30+590+1055", "(?i)total"),
            drop.neg.asymp = lasd_crop(x, "570x26+590+565", "(?i)negative"),
            drop.neg.symp = lasd_crop(x, "570x26+0+565", "(?i)negative"),
            drop.pos.asymp = lasd_crop(x, "500x26+590+509", "(?i)current"),
            drop.pos.symp = lasd_crop(x, "562x26+0+509", "(?i)current"),
            drop.test.asymp = lasd_crop(x, "562x26+590+590", "(?i)total"),
            drop.test.symp = lasd_crop(x, "562x26+00+590", "(?i)total"),
            Residents.Population = lasd_crop(x, "562x26+00+209", "(?i)jail pop"))
    }
    
    else if (h_ <= 1492){
        out <- tibble(
            Residents.Confirmed = lasd_crop(x, "570x30+580+400", "(?i)total pos"),
            Residents.Recovered = lasd_crop(x, "570x30+590+660", "(?i)recover"),
            Residents.Deaths = lasd_crop(x, "570x30+590+734", "(?i)deaths"),
            Residents.Quarantine = lasd_crop(x, "570x30+590+1055", "(?i)total"),
            drop.neg.asymp = lasd_crop(x, "570x26+590+565", "(?i)negative"),
            drop.neg.symp = lasd_crop(x, "570x26+0+575", "(?i)negative"),
            drop.pos.asymp = lasd_crop(x, "500x26+590+519", "(?i)current"),
            drop.pos.symp = lasd_crop(x, "562x26+0+509", "(?i)current"),
            drop.test.asymp = lasd_crop(x, "562x26+590+590", "(?i)total"),
            drop.test.symp = lasd_crop(x, "562x26+00+590", "(?i)total"),
            Residents.Population = lasd_crop(x, "562x26+00+216", "(?i)jail pop"))
    }
    
    else if (h_ <= 1520){
        out <- tibble(
            Residents.Confirmed = lasd_crop(x, "570x30+580+400", "(?i)total pos"),
            Residents.Recovered = lasd_crop(x, "570x30+590+660", "(?i)recover"),
            Residents.Deaths = lasd_crop(x, "570x30+590+734", "(?i)deaths"),
            Residents.Quarantine = lasd_crop(x, "570x30+590+1075", "(?i)total"),
            drop.neg.asymp = lasd_crop(x, "570x26+590+565", "(?i)negative"),
            drop.neg.symp = lasd_crop(x, "570x26+0+570", "(?i)negative"),
            drop.pos.asymp = lasd_crop(x, "562x26+590+516", "(?i)current"),
            drop.pos.symp = lasd_crop(x, "562x26+0+509", "(?i)current"),
            drop.test.asymp = lasd_crop(x, "562x26+590+590", "(?i)total"),
            drop.test.symp = lasd_crop(x, "562x26+00+590", "(?i)total"),
            Residents.Population = lasd_crop(x, "562x26+00+216", "(?i)jail pop"))
    }
    
    else if (h_ <= 1560){
        out <- tibble(
            Residents.Confirmed = lasd_crop(x, "570x30+600+420", "(?i)total pos"),
            Residents.Recovered = lasd_crop(x, "570x30+620+710", "(?i)recover"),
            Residents.Deaths = lasd_crop(x, "570x30+620+785", "(?i)deaths"),
            Residents.Quarantine = lasd_crop(x, "570x30+620+1105", "(?i)total"),
            drop.neg.asymp = lasd_crop(x, "570x25+620+604", "(?i)negative"),
            drop.neg.symp = lasd_crop(x, "570x25+20+604", "(?i)negative"),
            drop.pos.asymp = lasd_crop(x, "570x25+620+548", "(?i)current"),
            drop.pos.symp = lasd_crop(x, "562x25+20+548", "(?i)current"),
            drop.test.asymp = lasd_crop(x, "562x25+620+630", "(?i)total"),
            drop.test.symp = lasd_crop(x, "562x25+20+630", "(?i)total"),
            Residents.Population = lasd_crop(x, "562x25+20+229", "(?i)jail pop"))
    }
    
    else if (h_ <= 1590){
        out <- tibble(
            Residents.Confirmed = lasd_crop(x, "570x30+600+420", "(?i)total pos"),
            Residents.Recovered = lasd_crop(x, "570x30+620+710", "(?i)recover"),
            Residents.Deaths = lasd_crop(x, "570x30+620+785", "(?i)deaths"),
            Residents.Quarantine = lasd_crop(x, "570x30+620+1125", "(?i)total"),
            drop.neg.asymp = lasd_crop(x, "570x25+620+604", "(?i)negative"),
            drop.neg.symp = lasd_crop(x, "570x25+20+604", "(?i)negative"),
            drop.pos.asymp = lasd_crop(x, "570x25+620+548", "(?i)current"),
            drop.pos.symp = lasd_crop(x, "562x25+20+548", "(?i)current"),
            drop.test.asymp = lasd_crop(x, "562x25+620+630", "(?i)total"),
            drop.test.symp = lasd_crop(x, "562x25+20+630", "(?i)total"),
            Residents.Population = lasd_crop(x, "562x25+20+229", "(?i)jail pop"))
    }
    
    else if (h_ <= 1595){
        out <- tibble(
            Residents.Confirmed = lasd_crop(x, "570x30+600+430", "(?i)total pos"),
            Residents.Recovered = lasd_crop(x, "570x30+620+720", "(?i)recover"),
            Residents.Deaths = lasd_crop(x, "570x30+620+795", "(?i)deaths"),
            Residents.Quarantine = lasd_crop(x, "570x30+620+1145", "(?i)total"),
            drop.neg.asymp = lasd_crop(x, "570x25+620+614", "(?i)negative"),
            drop.neg.symp = lasd_crop(x, "570x25+20+614", "(?i)negative"),
            drop.pos.asymp = lasd_crop(x, "570x25+620+558", "(?i)current"),
            drop.pos.symp = lasd_crop(x, "562x25+20+558", "(?i)current"),
            drop.test.asymp = lasd_crop(x, "562x25+620+640", "(?i)total"),
            drop.test.symp = lasd_crop(x, "562x25+20+640", "(?i)total"),
            Residents.Population = lasd_crop(x, "562x25+20+239", "(?i)jail pop"))
    }
    
    else{
        out <- tibble(
            Residents.Confirmed = lasd_crop(x, "570x30+600+420", "(?i)total pos"),
            Residents.Recovered = lasd_crop(x, "570x30+620+710", "(?i)recover"),
            Residents.Deaths = lasd_crop(x, "570x30+620+785", "(?i)deaths"),
            Residents.Quarantine = lasd_crop(x, "570x30+620+1075", "(?i)total"),
            drop.neg.asymp = lasd_crop(x, "570x25+620+604", "(?i)negative"),
            drop.neg.symp = lasd_crop(x, "570x25+20+604", "(?i)negative"),
            drop.pos.asymp = lasd_crop(x, "570x25+620+548", "(?i)current"),
            drop.pos.symp = lasd_crop(x, "562x25+20+548", "(?i)current"),
            drop.test.asymp = lasd_crop(x, "562x25+620+630", "(?i)total"),
            drop.test.symp = lasd_crop(x, "562x25+20+630", "(?i)total"),
            Residents.Population = lasd_crop(x, "562x25+20+229", "(?i)jail pop"))
    }
    out
}

lasd_extract <- function(x){
    out_df <- x %>%
        mutate(Residents.Negative = drop.neg.asymp + drop.neg.symp) %>%
        mutate(Residents.Active = drop.pos.asymp + drop.pos.symp) %>%
        mutate(Residents.Tadmin = drop.test.asymp + drop.test.symp) %>% 
        select(-starts_with("drop")) %>%
        mutate(Name = "LA Jail")
    
    if(out_df$Residents.Deaths != 17){
        warning("You sure LA shouldnt be 17?")
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

