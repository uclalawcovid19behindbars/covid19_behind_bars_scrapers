source("./R/generic_scraper.R")

cook_county_check_date <- function(x, date = Sys.Date()){
    base_page <- xml2::read_html(x)
    
    base_page %>%
        rvest::html_nodes("p") %>%
        rvest::html_text() %>%
        {.[str_starts(., "(?i)as of")]} %>%
        str_extract("\\d{1,2}/\\d{1,2}/\\d{2,4}") %>%
        lubridate::mdy() %>%
        error_on_date(date)
}

cook_county_restruct <- function(x){
    
    # grab the main div which has covid related data
    covid_section <- x %>% 
        rvest::html_nodes("ul") 
  
    # get the jail detainee information
    covid_sub_text <- covid_section %>%
        rvest::html_nodes("li") %>%
        # hacky way to only get text from parent node
        map_chr(~paste0(rvest::html_text(rvest::html_nodes(
            ., xpath="./text()"), trim=TRUE), collapse=" ")) %>%
        str_remove_all("(?i)covid-19") %>%
        str_replace_all(",([0-9])", "\\1")
    
    # see where the text is
    res_idx <- list(
        Residents.Active = which(
            str_detect(covid_sub_text,"(?i)currently positive") & 
                str_detect(covid_sub_text, "(?i)custody")),
        Residents.Deaths = which(
            str_detect(covid_sub_text,"(?i)die") & 
                str_detect(covid_sub_text, "(?i)custody")))
    
    # make sure we have 1 and only 1 entry for each concept
    if(!all(sapply(res_idx, length) <= 1)){
        stop("Website structure is not as expected please inspect")
    }
    
    # extract the numeric values
    resident_df <- sapply(res_idx, function(x){
        sum_na_rm(as.numeric(
            str_extract(unlist(str_split(covid_sub_text[x], " ")), "[0-9]+")))
    }) %>%
        t() %>%
        as_tibble()
    
    # get the staff data which is not part of a list but rather in the main text
  staff_sub_text <- x %>% 
      rvest::html_nodes("p") %>% 
      rvest::html_text() %>%
      str_remove_all("(?i)covid-19") %>%
      .[(which(str_starts(., "(?i)as of")) + 1):(
        which(str_detect(., "(?i)note:")) - 1)] %>%
      str_replace_all(",([0-9])", "\\1")
        
    # do some sanity checks
    ## there should be three text sections
    if(length(staff_sub_text) != 2){
        stop("Website structure is not as expected please inspect")
    }
    
    ## only the first two have to do with positive staff
    if(any(str_detect(staff_sub_text, "positive") != c(TRUE, FALSE))){
      stop("Website structure is not as expected please inspect")
    }
    
    ## extract the staff data
    staff_df <- tibble(
        Staff.Active = sapply(staff_sub_text[1], function(x){
            sum_na_rm(as.numeric(
                str_extract(unlist(str_split(x, " ")), "[0-9]+")))}) %>%
            sum(),
        
        Staff.Deaths = staff_sub_text[2] %>%
            str_split(" ") %>%
            unlist() %>%
            str_extract("six|seven|eight|nine|ten") %>% 
            .[!is.na(.)] %>% 
            word_to_numeric()
    )
    
    bind_cols(resident_df, staff_df)
    
}

cook_county_extract <- function(x){
    x %>%
        mutate(Name = "Cook County Jail")
        
}

#' Scraper class for general Cook County COVID data
#' 
#' @name cook_county_scraper
#' @description Cook County Jail data is extremely sensitive to changes in the
#' formatting as data is extracted from raw text. Format should be checked
#' frequently. Data only reported for Cook County jail. Note that there is no
#' release numbers so total confirmed may be an under-count because of the way
#' it is calculates. Active + Recovered + Deaths. Historical data present in
#' graph but no way to extract.
#' \describe{
#'   \item{Staff Active}{Staff currently infected by virus.}
#'   \item{Staff Recovered}{Likely cumulative staff recovered.}
#'   \item{Staff Deaths}{Cumulative staff deaths.}
#'   \item{Residents Active}{Residents currently infected by virus.}
#'   \item{Residents Deaths}{Cumulative resident deaths.}
#'   \item{Residents Negative}{Likely cumulative negative cases.}
#' }

cook_county_scraper <- R6Class(
    "cook_county_scraper",
    inherit = generic_scraper,
    public = list(
        log = NULL,
        initialize = function(
            log,
            url = "https://www.cookcountysheriff.org/covid-19-cases-at-ccdoc/",
            id = "cook_county",
            type = "html",
            state = "IL",
            jurisdiction = "county",
            check_date = cook_county_check_date,
            # pull the JSON data directly from the API
            pull_func = xml2::read_html,
            # restructuring the data
            restruct_func = cook_county_restruct,
            # Rename the columns to appropriate database names
            extract_func = cook_county_extract){
            super$initialize(
                url = url, id = id, pull_func = pull_func, type = type,
                restruct_func = restruct_func, extract_func = extract_func,
                log = log, state = state, jurisdiction = jurisdiction,
                check_date = check_date)
        }
    )
)

if(sys.nframe() == 0){
    cook_county <- cook_county_scraper$new(log=FALSE)
    cook_county$run_check_date()
    cook_county$raw_data
    cook_county$pull_raw()
    cook_county$raw_data
    cook_county$save_raw()
    cook_county$restruct_raw()
    cook_county$restruct_data
    cook_county$extract_from_raw()
    cook_county$extract_data
    cook_county$validate_extract()
    cook_county$save_extract()
}
