source("./R/generic_scraper.R")
source("./R/utilities.R")

florida_deaths_restruct <- function(x){
    npages <- tabulizer::get_n_pages(x)
    lapply(1:npages, function(i){
        ExtractTable(magick::image_read_pdf(x, i))[[1]]
    })
}

florida_deaths_extract <- function(x){
    
    col_name_mat <- matrix(c(
        "Facility", "0", "Name",
        "Total Deaths", "1", "Drop.Total",
        "Inmate Deaths", "2", "Residents.Deaths",
        "Staff Deaths", "3", "Staff.Deaths"
    ), ncol = 3, nrow = 4, byrow = TRUE)
    
    colnames(col_name_mat) <- c("check", "raw", "clean")
    col_name_df <- as_tibble(col_name_mat)
    
    ex_df <- bind_rows(lapply(1:length(x), function(i){
        
        if(i == 1){
            df_ <- as_tibble(x[[i]][-1,])
            check_names_extractable(df_, col_name_df)
            renamed_df <- rename_extractable(df_, col_name_df) %>%
                filter(!str_detect(Name, "(?i)Facility"))
        }
        else{
            renamed_df <- rename_extractable(as_tibble(x[[i]]), col_name_df)
        }

        renamed_df
    }))
    
    ex_df %>%
        clean_scraped_df() %>%
        mutate(Name = str_squish(str_replace(Name, "-", " - "))) %>%
        mutate(Name = str_squish(str_replace(
            Name, "(?i)re - entry", "reentry"))) %>%
        mutate(Name = str_squish(str_split_fixed(Name, "-", 2)[,1])) %>%
        select(-starts_with("Drop"))
}

#' Scraper class for general florida_deaths COVID death data
#' 
#' @name florida_deaths_scraper
#' @description Data comes from a pdf which is uploaded to extractable
#' servers for analysis. Table is often spread across multiple pages
#' \describe{
#'   \item{Facilities}{The faciilty name}
#'   \item{Inmate Deaths}{Cumulative residents who have died.}
#'   \item{Staff Deaths}{Cumulative staff who have died.}
#' }

florida_deaths_scraper <- R6Class(
    "florida_deaths_scraper",
    inherit = generic_scraper,
    public = list(
        log = NULL,
        initialize = function(
            log,
            url = 
                "http://ww11.doh.state.fl.us/comm/_partners/covid19_report_archive/correction-facilities-deaths/fdc_death_latest.pdf",
            id = "florida_deaths",
            state = "FL",
            type = "pdf",
            jurisdiction = "state",
            check_date = NULL,
            pull_func = function(x) x,
            restruct_func = florida_deaths_restruct,
            # Rename the columns to appropriate database names and do some minor
            # minor cleaning
            extract_func = florida_deaths_extract){
            super$initialize(
                url = url, id = id, pull_func = pull_func, type = type,
                restruct_func = restruct_func, extract_func = extract_func,
                log = log, state = state, jurisdiction = jurisdiction,
                check_date = check_date)
        }
    )
)

if(sys.nframe() == 0){
    florida_deaths <- florida_deaths_scraper$new(log=TRUE)
    florida_deaths$run_check_date()
    florida_deaths$perma_save()
    florida_deaths$raw_data
    florida_deaths$pull_raw()
    florida_deaths$save_raw()
    florida_deaths$raw_data
    florida_deaths$restruct_raw()
    florida_deaths$restruct_data
    florida_deaths$extract_from_raw()
    florida_deaths$extract_data
    florida_deaths$validate_extract()
    florida_deaths$save_extract()
}
