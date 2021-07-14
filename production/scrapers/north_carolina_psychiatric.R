source("./R/generic_scraper.R")
source("./R/utilities.R")

north_carolina_check_date <- function(x, date = Sys.Date()){
    z <- get_src_by_attr(x, "a", attr = "href", attr_regex = "clf-report") %>%
        magick::image_read_pdf() %>%
        .[1]
    
    z %>%
        magick::image_crop("2550x200+0+550") %>%
        magick::image_ocr() %>%
        str_split("(?i)updated") %>%
        unlist() %>%
        last() %>%
        str_remove_all("\n") %>%
        lubridate::mdy() %>%
        error_on_date(date)
}

north_carolina_psychiatric_pull <- function(x){
    get_src_by_attr(x, "a", attr = "href", attr_regex = "clf-report")
}

north_carolina_psychiatric_restruct <- function(x){
    z <-  magick::image_read_pdf(x)
    
    lapply(2:4, function(i) ExtractTable(z[i]))
}

north_carolina_psychiatric_extract <- function(x){
    main_mat <- x[[1]][[1]]
    
    comb_df <- bind_rows(lapply(x, function(z){
        main_mat <- z[[1]]
        first_name <- unlist(main_mat[1,])
        first_name[which(first_name != "")-1] <- unname(
            first_name[which(first_name != "")])
        second_name <- unlist(main_mat[2,]) %>%
            {ifelse(. == "Cases", "Confirmed", .)} %>%
            str_remove_all("\\*")
        
        names(main_mat) <- str_c(first_name, second_name, sep = ".") %>%
            str_replace(".Facility", "Facility")
        
        main_mat[3:nrow(main_mat),] %>%
            as_tibble()
    }))
    
    comb_df %>%
        filter(`Facility Type` == "Residential Care Facility") %>%
        select(-`Facility Type`, -`Facility County`) %>% 
        select(-Total.Confirmed, -Total.Deaths) %>%
        mutate(across(Staff.Confirmed:Residents.Deaths, ~ as.integer(.x))) %>%
        rename(Name = Facility)
}

#' Scraper class for general North Carolina COVID data
#' 
#' @name north_carolina_psychiatric_scraper
#' @description NC has data for many congregate settings is compiled by DHHS.
#' Here we need to filter down to just psychiatric wards.
#' \describe{
#'   \item{Facility name}{Name}
#'   \item{Residents Cases}{Residents.Confirmed}
#'   \item{Staff Cases}{Staff.Confirmed}
#'   \item{Residents Deaths}{Residents.Deaths}
#'   \item{Staff Deaths}{Staff.Deaths}
#' }

north_carolina_psychiatric_scraper <- R6Class(
    "north_carolina_psychiatric_scraper",
    inherit = generic_scraper,
    public = list(
        log = NULL,
        initialize = function(
            log,
            url = "https://covid19.ncdhhs.gov/dashboard/outbreaks-and-clusters",
            id = "north_carolina_psychiatric",
            type = "pdf",
            state = "NC",
            jurisdiction = "psychiatric",
            check_date = north_carolina_check_date,
            # pull the JSON data directly from the API
            pull_func = north_carolina_psychiatric_pull,
            # restructuring the data means pulling out the data portion of the json
            restruct_func = north_carolina_psychiatric_restruct,
            # Rename the columns to appropriate database names
            extract_func = north_carolina_psychiatric_extract){
            super$initialize(
                url = url, id = id, pull_func = pull_func, type = type,
                restruct_func = restruct_func, extract_func = extract_func,
                log = log, state = state, jurisdiction = jurisdiction,
                check_date = check_date)
        }
    )
)

if(sys.nframe() == 0){
    north_carolina_psychiatric <- north_carolina_psychiatric_scraper$new(log=TRUE)
    north_carolina_psychiatric$run_check_date()
    north_carolina_psychiatric$raw_data
    north_carolina_psychiatric$pull_raw()
    north_carolina_psychiatric$raw_data
    north_carolina_psychiatric$save_raw()
    north_carolina_psychiatric$restruct_raw()
    north_carolina_psychiatric$restruct_data
    north_carolina_psychiatric$extract_from_raw()
    north_carolina_psychiatric$extract_data
    north_carolina_psychiatric$validate_extract()
    north_carolina_psychiatric$save_extract()
}

