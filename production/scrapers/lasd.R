source("./R/generic_scraper.R")
source("./R/utilities.R")

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
    w_ <- magick::image_info(x)$width
    h_ <- magick::image_info(x)$height
    # this is the most common shape not sure why it changes almost daily :/
    if(abs(1447 - h_) <= 2 & w_ == 1200){
        out <- tibble(
            Residents.Confirmed = lasd_crop(x, "570x30+620+410", "(?i)total pos"),
            Residents.Recovered = lasd_crop(x, "570x30+620+670", "(?i)recover"),
            Residents.Deaths = lasd_crop(x, "570x30+620+745", "(?i)deaths"),
            Residents.Quarantine = lasd_crop(x, "570x30+620+1015", "(?i)total"),
            drop.neg.asymp = lasd_crop(x, "570x25+620+572", "(?i)negative"),
            drop.neg.symp = lasd_crop(x, "570x25+20+572", "(?i)negative"),
            drop.pos.asymp = lasd_crop(x, "500x25+620+518", "(?i)current"),
            drop.pos.symp = lasd_crop(x, "562x25+20+518", "(?i)current"),
            drop.test.asymp = lasd_crop(x, "562x25+620+600", "(?i)total"),
            drop.test.symp = lasd_crop(x, "562x25+20+600", "(?i)total"),
            Residents.Population = lasd_crop(x, "562x25+20+219", "(?i)jail pop"))
    }
    
    else if(abs(1442 - h_) <= 2 & w_ == 1200){
        out <- tibble(
            Residents.Confirmed = lasd_crop(x, "570x30+620+410", "(?i)total pos"),
            Residents.Recovered = lasd_crop(x, "570x30+620+665", "(?i)recover"),
            Residents.Deaths = lasd_crop(x, "570x30+620+745", "(?i)deaths"),
            Residents.Quarantine = lasd_crop(x, "570x30+620+1015", "(?i)total"),
            drop.neg.asymp = lasd_crop(x, "570x25+620+572", "(?i)negative"),
            drop.neg.symp = lasd_crop(x, "570x25+20+572", "(?i)negative"),
            drop.pos.asymp = lasd_crop(x, "500x25+620+518", "(?i)current"),
            drop.pos.symp = lasd_crop(x, "562x25+20+518", "(?i)current"),
            drop.test.asymp = lasd_crop(x, "562x25+620+600", "(?i)total"),
            drop.test.symp = lasd_crop(x, "562x25+20+600", "(?i)total"),
            Residents.Population = lasd_crop(x, "562x25+20+219", "(?i)jail pop"))
    }
    
    else if(abs(1463 - h_) <= 8 & w_ == 1200){
        out <- tibble(
            Residents.Confirmed = lasd_crop(x, "570x30+620+410", "(?i)total pos"),
            Residents.Recovered = lasd_crop(x, "570x30+620+670", "(?i)recover"),
            Residents.Deaths = lasd_crop(x, "570x30+620+740", "(?i)deaths"),
            Residents.Quarantine = lasd_crop(x, "570x30+620+1035", "(?i)total"),
            drop.neg.asymp = lasd_crop(x, "570x25+620+572", "(?i)negative"),
            drop.neg.symp = lasd_crop(x, "570x25+20+572", "(?i)negative"),
            drop.pos.asymp = lasd_crop(x, "500x25+620+518", "(?i)current"),
            drop.pos.symp = lasd_crop(x, "562x25+20+518", "(?i)current"),
            drop.test.asymp = lasd_crop(x, "562x25+620+600", "(?i)total"),
            drop.test.symp = lasd_crop(x, "562x25+20+600", "(?i)total"),
            Residents.Population = lasd_crop(x, "562x25+20+219", "(?i)jail pop"))
    }
    
    else if(abs(1473 - h_) <= 3 & w_ == 1200){
        out <- tibble(
            Residents.Confirmed = lasd_crop(x, "570x30+620+410", "(?i)total pos"),
            Residents.Recovered = lasd_crop(x, "570x30+620+670", "(?i)recover"),
            Residents.Deaths = lasd_crop(x, "570x30+620+740", "(?i)deaths"),
            Residents.Quarantine = lasd_crop(x, "570x30+620+1035", "(?i)total"),
            drop.neg.asymp = lasd_crop(x, "570x25+620+572", "(?i)negative"),
            drop.neg.symp = lasd_crop(x, "570x25+20+572", "(?i)negative"),
            drop.pos.asymp = lasd_crop(x, "500x25+620+518", "(?i)current"),
            drop.pos.symp = lasd_crop(x, "562x25+20+518", "(?i)current"),
            drop.test.asymp = lasd_crop(x, "562x25+620+600", "(?i)total"),
            drop.test.symp = lasd_crop(x, "562x25+20+600", "(?i)total"),
            Residents.Population = lasd_crop(x, "562x25+20+219", "(?i)jail pop"))
    }
    
    else if(abs(1492 - h_) <= 4 & w_ == 1200){
        out <- tibble(
            Residents.Confirmed = lasd_crop(x, "570x30+620+400", "(?i)total pos"),
            Residents.Recovered = lasd_crop(x, "570x30+620+660", "(?i)recover"),
            Residents.Deaths = lasd_crop(x, "570x30+620+735", "(?i)deaths"),
            Residents.Quarantine = lasd_crop(x, "570x30+620+1058", "(?i)total"),
            drop.neg.asymp = lasd_crop(x, "570x25+620+568", "(?i)negative"),
            drop.neg.symp = lasd_crop(x, "570x25+20+568", "(?i)negative"),
            drop.pos.asymp = lasd_crop(x, "500x25+620+514", "(?i)current"),
            drop.pos.symp = lasd_crop(x, "562x25+20+514", "(?i)current"),
            drop.test.asymp = lasd_crop(x, "562x25+620+592", "(?i)total"),
            drop.test.symp = lasd_crop(x, "562x25+20+592", "(?i)total"),
            Residents.Population = lasd_crop(x, "562x25+20+212", "(?i)jail pop"))
    }
    
    else if(abs(1605 - h_) <= 12 & w_ == 1200){
        out <- tibble(
            Residents.Confirmed = lasd_crop(x, "570x30+620+440", "(?i)total pos"),
            Residents.Recovered = lasd_crop(x, "570x30+620+715", "(?i)recover"),
            Residents.Deaths = lasd_crop(x, "570x30+620+790", "(?i)deaths"),
            Residents.Quarantine = lasd_crop(x, "570x30+620+1145", "(?i)total"),
            drop.neg.asymp = lasd_crop(x, "570x25+620+615", "(?i)negative"),
            drop.neg.symp = lasd_crop(x, "570x25+20+615", "(?i)negative"),
            drop.pos.asymp = lasd_crop(x, "560x25+620+555", "(?i)current"),
            drop.pos.symp = lasd_crop(x, "562x25+20+555", "(?i)current"),
            drop.test.asymp = lasd_crop(x, "562x25+620+640", "(?i)total"),
            drop.test.symp = lasd_crop(x, "562x25+20+640", "(?i)total"),
            Residents.Population = lasd_crop(x, "562x25+20+235", "(?i)jail pop"))
    }
    else if(abs(1605 - h_) <= 19 & w_ == 1200){
        out <- tibble(
            Residents.Confirmed = lasd_crop(x, "570x30+620+425", "(?i)total pos"),
            Residents.Recovered = lasd_crop(x, "570x30+620+690", "(?i)recover"),
            Residents.Deaths = lasd_crop(x, "570x30+620+738", "(?i)deaths"),
            Residents.Quarantine = lasd_crop(x, "570x30+620+1063", "(?i)total"),
            drop.neg.asymp = lasd_crop(x, "570x25+620+572", "(?i)negative"),
            drop.neg.symp = lasd_crop(x, "570x25+20+572", "(?i)negative"),
            drop.pos.asymp = lasd_crop(x, "500x25+620+518", "(?i)current"),
            drop.pos.symp = lasd_crop(x, "562x25+20+518", "(?i)current"),
            drop.test.asymp = lasd_crop(x, "562x25+620+600", "(?i)total"),
            drop.test.symp = lasd_crop(x, "562x25+20+600", "(?i)total"),
            Residents.Population = lasd_crop(x, "562x25+20+219", "(?i)jail pop"))
    }
    else if(abs(1515 - h_) <= 19 & w_ == 1200){
        out <- tibble(
            Residents.Confirmed = lasd_crop(x, "570x30+620+435", "(?i)total pos"),
            Residents.Recovered = lasd_crop(x, "570x30+620+715", "(?i)recover"),
            Residents.Deaths = lasd_crop(x, "570x30+620+790", "(?i)deaths"),
            Residents.Quarantine = lasd_crop(x, "570x30+620+1055", "(?i)total"),
            drop.neg.asymp = lasd_crop(x, "570x25+620+612", "(?i)negative"),
            drop.neg.symp = lasd_crop(x, "570x25+20+612", "(?i)negative"),
            drop.pos.asymp = lasd_crop(x, "545x25+625+558", "(?i)current"),
            drop.pos.symp = lasd_crop(x, "562x25+20+558", "(?i)current"),
            drop.test.asymp = lasd_crop(x, "562x25+620+640", "(?i)total"),
            drop.test.symp = lasd_crop(x, "562x25+20+640", "(?i)total"),
            Residents.Population = lasd_crop(x, "562x25+20+233", "(?i)jail pop"))
    }
    else{
        out <- tibble(
            Residents.Confirmed = lasd_crop(x, "570x30+620+405", "(?i)total pos"),
            Residents.Recovered = lasd_crop(x, "570x30+620+670", "(?i)recover"),
            Residents.Deaths = lasd_crop(x, "570x30+620+738", "(?i)deaths"),
            Residents.Quarantine = lasd_crop(x, "570x30+620+1063", "(?i)total"),
            drop.neg.asymp = lasd_crop(x, "570x25+620+572", "(?i)negative"),
            drop.neg.symp = lasd_crop(x, "570x25+20+572", "(?i)negative"),
            drop.pos.asymp = lasd_crop(x, "500x25+620+518", "(?i)current"),
            drop.pos.symp = lasd_crop(x, "562x25+20+518", "(?i)current"),
            drop.test.asymp = lasd_crop(x, "562x25+620+600", "(?i)total"),
            drop.test.symp = lasd_crop(x, "562x25+20+600", "(?i)total"),
            Residents.Population = lasd_crop(x, "562x25+20+219", "(?i)jail pop"))
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
    
    if(out_df$Residents.Deaths != 13){
        warning("You sure LA shouldnt be 13???")
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
            check_date = NULL,
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

