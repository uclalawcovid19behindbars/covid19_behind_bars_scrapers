source("./R/generic_scraper.R")
source("./R/utilities.R")

hawaii_staff_date_check <- function(x, date = Sys.Date()){
    img <- get_src_by_attr(
        x, "img", attr = "src", attr_regex = "(?i)active-recovered") %>%
        {gsub("-[0-9]+x[0-9]+", "", .)} %>%
        magick::image_read() 
    
    img %>% 
        magick::image_crop("500x200") %>% 
        magick::image_ocr() %>% 
        str_split("Updated") %>% 
        unlist() %>%
        {.[str_detect(., "(?i)21")]} %>%
        str_squish() %>% 
        str_extract("\\d{1,2}/\\d{1,2}/\\d{1,2}") %>%
        lubridate::mdy() %>%
        error_on_date(date)
}

hawaii_staff_pull <- function(x) {
    get_src_by_attr(
        x, "img", attr = "src", attr_regex = "(?i)active-recovered") %>%
        {gsub("-[0-9]+x[0-9]+", "", .)} %>%
        magick::image_read()
}

hawaii_staff_restruct <- function(x){
    ExtractTable(x)
}

hawaii_staff_extract <- function(x){
    dat_mat <- x[[1]]
    dat_mat[1,] <- last_not_na(
        unlist(ifelse(dat_mat[1,] == "", NA, dat_mat[1,])))
    new_names <- apply(dat_mat[1:2,], 2, function(x){
        str_squish(str_c(na.omit(x), collapse = " "))
    })
    
    sub_mat <- dat_mat[2:nrow(dat_mat),]
    sub_mat[1,] <- new_names
    
    col_name_mat <- matrix(c(
        "CORRECTIONS DIVISION", "0", "Name",
        "STAFF ACTIVE", "1", "Staff.Active",
        "STAFF RECOVERED", "2", "Staff.Recovered",
        "INMATES ACTIVE", "3", "Drop.Dup1",
        "INMATES RECOVERED", "4", "Drop.Dup2"
    ), ncol = 3, nrow = 5, byrow = TRUE)
    
    colnames(col_name_mat) <- c("check", "raw", "clean")
    col_name_df <- as_tibble(col_name_mat)
    df_ <- as_tibble(sub_mat)
    check_names_extractable(df_, col_name_df)
    renamed_df <- rename_extractable(df_, col_name_df)
    
    renamed_df %>%
        select(Name, Staff.Recovered, Staff.Active) %>%
        .[-1,] %>%
        filter(!str_detect(Name, "(?i)total")) %>%
        filter(Staff.Recovered != "") %>%
        clean_scraped_df()
}

#' Scraper class for general hawaii_staff COVID data
#' 
#' @name hawaii_staff_scraper
#' @description Data comes from an image which is uploaded to extractable
#' servers for analysis. We dont need several of these columns in here because
#' it is redundant to the data in the other hawaii scraper.
#' \describe{
#'   \item{CORRECTIONS DIVISION}{The faciilty name}
#'   \item{STAFF ACTIVE}{Staff with active infections}
#'   \item{STAFF RECOVERED}{Staff who have recovered from infection}
#'   \item{INMATES ACTIVE}{Inamtes with active infections redundant column dont need to extract}
#'   \item{INMATES RECOVERED}{Inamtes who have recovered from infection, redundant column dont need to extract}
#' }

hawaii_staff_scraper <- R6Class(
    "hawaii_staff_scraper",
    inherit = generic_scraper,
    public = list(
        log = NULL,
        initialize = function(
            log,
            url = 
                "https://dps.hawaii.gov/blog/2020/03/17/coronavirus-covid-19-information-and-resources/",
            id = "hawaii_staff",
            state = "HI",
            type = "img",
            jurisdiction = "state",
            check_date = hawaii_staff_date_check,
            # restructuring the data means pulling out the data portion of the json
            pull_func = hawaii_staff_pull,
            # TODO: we are not currently extracting the last updated section
            # but it looks somewhat scrape-able.
            restruct_func = hawaii_staff_restruct,
            # Rename the columns to appropriate database names and do some minor
            # minor cleaning
            extract_func = hawaii_staff_extract){
            super$initialize(
                url = url, id = id, pull_func = pull_func, type = type,
                restruct_func = restruct_func, extract_func = extract_func,
                log = log, state = state, jurisdiction = jurisdiction,
                check_date = check_date)
        }
    )
)

if(sys.nframe() == 0){
    hawaii_staff <- hawaii_staff_scraper$new(log=TRUE)
    hawaii_staff$run_check_date()
    hawaii_staff$raw_data
    hawaii_staff$pull_raw()
    hawaii_staff$save_raw()
    hawaii_staff$raw_data
    hawaii_staff$restruct_raw()
    hawaii_staff$restruct_data
    hawaii_staff$extract_from_raw()
    hawaii_staff$extract_data
    hawaii_staff$validate_extract()
    hawaii_staff$save_extract()
}