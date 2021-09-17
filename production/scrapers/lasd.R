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
        magick::image_read() %>% 
        magick::image_trim()
}

lasd_restruct <- function(x){
    
    x <- magick::image_trim(x)
    
    # Extract non-tables (recovered, deaths, asymptomatic total, symptomatic total)
    w_ <- magick::image_info(x)$width
    h_ <- magick::image_info(x)$height
    
    # If image is way too big, resize it down 
    if (h_ > 1700){
        x <- magick::image_scale(x, "1200")
        
        w_ <- magick::image_info(x)$width
        h_ <- magick::image_info(x)$height
    }
    
    # Brightened version works better for tesseract OCR 
    # Un-brightened version works better for ExtractTable 
    x_ <- magick::image_modulate(x, brightness = 120)
    
    if (h_ <= 1520){
        out <- tibble(
            Residents.Recovered = lasd_crop(x_, "570x30+590+660", "(?i)recover"),
            Residents.Deaths = lasd_crop(x_, "570x30+590+736", "(?i)deaths"),
            Symptomatic.Total = lasd_crop(x_, "562x30+00+590", "(?i)total"),
            Asymptomatic.Total = lasd_crop(x_, "562x30+590+590", "(?i)total"))
    }
    
    else if (h_ <= 1590){
        out <- tibble(
            Residents.Recovered = lasd_crop(x_, "570x30+610+705", "(?i)recover"),
            Residents.Deaths = lasd_crop(x_, "570x30+610+780", "(?i)deaths"), 
            Symptomatic.Total = lasd_crop(x_, "562x30+00+630", "(?i)total"),
            Asymptomatic.Total = lasd_crop(x_, "562x30+610+630", "(?i)total"))
    }
    
    else {
        out <- tibble(
            Residents.Recovered = lasd_crop(x_, "570x30+620+710", "(?i)recover"),
            Residents.Deaths = lasd_crop(x_, "570x30+620+785", "(?i)deaths"), 
            Symptomatic.Total = lasd_crop(x_, "562x30+00+638", "(?i)total"),
            Asymptomatic.Total = lasd_crop(x_, "562x30+610+638", "(?i)total"))
    }
    
    out
}

# # Extract other tables 
# ex_ <- ExtractTable(x)
# 
# # If length of (ex_) != 7: 
##    warning()
# 
# num_ <- lapply(ex_, function(z){
#     z <- z %>% 
#         mutate(val = as.numeric(gsub(",", "", `1`))) %>% 
#         select(measure = `0`, val)})
# 
# # Get indices because ExtractTable is not deterministic :( 
# pop_idx <- which(sapply(num_, function(z){
#     any(str_detect(z[,1], "(?i)bookings"))}))
# 
# iso_idx <- which(sapply(num_, function(z){
#     any(str_detect(z[,1], "(?i)pending"))}))
# 
# hist_idx <- which(sapply(num_, function(z){
#     any(str_detect(z[,1], "(?i)total positive"))}))
# 
# iso_fac_idx <- which(sapply(num_, function(z){
#     any(str_detect(z[,1], "(?i)century")) & any(str_detect(z[,1], "(?i)pui"))}))
# 
# quar_fac_idx <- which(sapply(num_, function(z){
#     any(str_detect(z[,1], "(?i)century")) & (!any(str_detect(z[,1], "(?i)pui")))}))
# 
# symp_idx <- which(sapply(num_, function(z){
#     any(z$val == out$Symptomatic.Total)}))
# 
# asymp_idx <- which(sapply(num_, function(z){
#     any(z$val ==  out$Asymptomatic.Total)}))
# 
# # Combine tables 
# parse_table <- function(df, idx){
#     tryCatch(
#         {return (df[[idx]])},
#         error = function(cond){return(data.frame())}
#     )
# }
# 
# tables_ <- do.call(rbind, list(
#     
#     # Population 
#     parse_table(num_, pop_idx) %>% 
#         mutate(measure = paste("Population", measure)), 
#     
#     # Current Isolation Totals 
#     parse_table(num_, iso_idx) %>%
#         mutate(measure = paste("Isolation Total", measure)),
#     
#     # Historical Symptomatic and Asmptomatic Running Totals 
#     parse_table(num_, hist_idx) %>% 
#         mutate(measure = paste("Historical Total", measure)), 
#     
#     # Isolation Totals by Facility 
#     parse_table(num_, iso_fac_idx) %>% 
#         mutate(measure = case_when(
#             str_detect(measure, "(?i)pui") ~ paste("Isolation", lag(measure), measure), 
#             str_detect(measure, "(?i)confirmed") ~ paste("Isolation", lag(measure, n = 2), measure), 
#             TRUE ~ paste("Isolation", measure))), 
#     
#     # Quarentine Totals by Facility 
#     parse_table(num_, quar_fac_idx) %>% 
#         mutate(measure = paste("Quarentine Total", measure)), 
#     
#     # Symptomatic  
#     parse_table(num_, symp_idx) %>% 
#         mutate(measure = paste("Symptomatic", measure)), 
#     
#     # Asympatomatic 
#     parse_table(num_, asymp_idx) %>% 
#         mutate(measure = paste("Asymptomatic", measure))
#     )) %>% 
#     pivot_longer(cols = c(-measure)) %>%
#     pivot_wider(names_from = measure)
# 
# # Combine tables and non-tables 
# bind_cols(out, tables_) %>% 
#     select(starts_with(
#         c("Residents", "Symptomatic", "Asymptomatic", "Population", 
#           "Isolation", "Historical", "Quarentine"))) 
# }

lasd_extract <- function(x){
    x %>%
        select(Residents.Recovered, Residents.Deaths) %>% 
        mutate(Name = "LA Jail") 
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
    lasd$reset_date("2021-06-21")
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
