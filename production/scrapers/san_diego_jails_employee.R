source("./R/generic_scraper.R")
source("./R/utilities.R")

san_diego_jails_employee_pull <- function(x) {
    get_src_by_attr(x, "a", "attr" = "href", attr_regex = "(?i)dsb-covid19-2.pdf") %>%
        magick::image_read_pdf()
}

san_diego_jails_employee_restruct <- function(x) {
    x %>% 
        magick::image_crop("2500x700+0+300") %>% 
        ExtractTable()
}

san_diego_jails_employee_extract <- function(x) {
    
    col_name <- matrix(c(
        "Staff.Confirmed", "0", "CUMULATIVE POSITIVE CASES", 
        "Drop.Staff.Active", "1", "ACTIVE POSITIVE CASES",
        "Staff.Recovered", "2", "RECOVERED POSITIVE CASES", 
        "Staff.Deaths", "3", "DECEASED DUE TO COVID-19"
    ), ncol = 3, nrow = 4, byrow = TRUE)
    
    colnames(col_name) <- c("clean", "raw", "check")
    col_name_df <- as_tibble(col_name)
    
    df_ <- as.data.frame(x[[1]])
    
    check_names_extractable(df_, col_name_df)
    
    rename_extractable(df_, col_name_df) %>% 
        filter(!str_detect(Staff.Confirmed, "(?i)cases")) %>% 
        mutate(Name = "San Diego County Jails") %>% 
        clean_scraped_df() %>%
        select(-starts_with("Drop")) %>%
        as_tibble()
}

#' Scraper class for general San Diego County jails COVID data
#' 
#' @name san_diego_jails_employee_scraper
#' @description San Diego County reports testing and confirmed data for 
#' employees in a pdf hosted on the web page. The data includes all seven San 
#' Diego County detention facilities. Tests administered and cumulative positive 
#' cases are based on data collected from March 16, 2020 to the report date. 
#' \describe{
#'   \item{COVID-19 Tests Administered}{Number of tests administered to residents}
#'   \item{Cumulative Positive Cases}{Number of residents confirmed positive}
#'   \item{Active Cases in Custody}{Number of inmates currently in custody who are confirmed COVID-19 positve}
#'   \item{Recovered/Released Cases}{Number of inmates with a positve COVID-19 test result who later tested 
#'   negative and have been cleared by medical personnel while in custody; and inmates with a positive COVID-19 
#'   test result who were released from custody either before or after clearance from medical isolation.}
#'   \item{Inmates in Isolation for Precautions}{Number of inmates who are being monitored by medical personnel 
#'   either due to presenting COVID-like symptoms or due to possible exposure.}

san_diego_jails_employee_scraper <- R6Class(
    "san_diego_jails_employee_scraper",
    inherit = generic_scraper,
    public = list(
        log = NULL,
        initialize = function(
            log,
            url = "https://www.sdsheriff.net/",
            id = "san_diego_jails_employee",
            type = "img",
            state = "CA",
            jurisdiction = "county",
            # pull the JSON data directly from the API
            pull_func = san_diego_jails_employee_pull,
            # restructuring the data means pulling out the data portion of the json
            restruct_func = san_diego_jails_employee_restruct,
            # Rename the columns to appropriate database names
            extract_func = san_diego_jails_employee_extract){
            super$initialize(
                url = url, id = id, pull_func = pull_func, type = type,
                restruct_func = restruct_func, extract_func = extract_func,
                log = log, state = state, jurisdiction = jurisdiction)
        }
    )
)

if(sys.nframe() == 0){
    san_diego_jails_employee <- san_diego_jails_employee_scraper$new(log=FALSE)
    san_diego_jails_employee$raw_data
    san_diego_jails_employee$pull_raw()
    san_diego_jails_employee$raw_data
    san_diego_jails_employee$save_raw()
    san_diego_jails_employee$restruct_raw()
    san_diego_jails_employee$restruct_data
    san_diego_jails_employee$extract_from_raw()
    san_diego_jails_employee$extract_data
    san_diego_jails_employee$validate_extract()
    san_diego_jails_employee$save_extract()
}
