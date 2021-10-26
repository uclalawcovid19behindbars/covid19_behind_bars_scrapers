source("./R/generic_scraper.R")
source("./R/utilities.R")

west_virginia_check_date <- function(x, date = Sys.Date()){
    
    z <- xml2::read_html(x)
    
    z %>%
        rvest::html_node(xpath ="//font[contains(text(),'as of')]") %>%
        rvest::html_text() %>%
        str_split(",") %>%
        unlist() %>%
        last() %>%
        str_split(";") %>%
        unlist() %>%
        first() %>%
        lubridate::mdy() %>%
        error_on_date(date)
}

west_virginia_pull <- function(x){
    latest_pdf <- get_src_by_attr(
        x, "a", attr = "href", attr_regex = "pdf$") %>%
        {.[!str_detect(., "vaccine")]} %>%
        {.[str_detect(., "https://dhhr.wv.gov/COVID-19/Documents/COVID19_DCR_2021")]} %>%
        last()
    
    return(latest_pdf)
}

west_virginia_restruct <- function(x){
    z <-  magick::image_read_pdf(x, pages = 1) %>%
        magick::image_crop("1800x2220+190+280") %>%
        ExtractTable()
    return(z)
}

west_virginia_extract <- function(x){
    col_name_mat <- matrix(c(
        "Regional jails", "X0", "Name", 
        "County", "X1", "Drop.County", 
        "Pop.", "X2", "Residents.Population", 
        "Active cases", "X3", "Residents.Active", 
        "Recovered", "X4", "Residents.Recovered",
        "Cumulative Positives", "X5", "Residents.Confirmed",
        "Negative", "X6", "Residents.Negative",
        "Results Pending", "X7", "Residents.Pending",
        "Total Tests", "X8", "Residents.Tadmin",
        "Quarantine", "X9", "Residents.Quarantine"
    ), ncol = 3, nrow = 10, byrow = TRUE)
    
    colnames(col_name_mat) <- c("check", "raw", "clean")
    col_name_df <- as_tibble(col_name_mat)
    
    df_ <- as.data.frame(x)
    
    check_names_extractable(df_, col_name_df)
    
    x <- rename_extractable(df_, col_name_df) %>% 
        as_tibble() %>%
        filter(Drop.County != "County")
        
    fac_df <- x %>% 
        filter(
            !is.na(Residents.Population) & 
                !str_detect(Residents.Population, "(?i)total") &
                !str_detect(Name, "(?i)total")) %>%
        select(-Drop.County) %>%
        .[1:(which(str_detect(.$Name, "(?i)employee")) - 1),] %>%
        clean_scraped_df() %>%
        mutate(
            Residents.Quarantine = Residents.Quarantine + Residents.Pending) %>%
        select(-starts_with("Drop")) %>%
        filter(!Name %in% c("Regional jails", "Correctional Centers", "Community Corrections", "Juvenile" ))
    
    # get deaths
    resident_deaths <- x %>%
        filter(
            str_starts(Name, "(?i)deaths")) %>%
        ## the column this is listed in happens to fall within the "Residents.Negative"
        ## NB this is subject to change
        pull(Residents.Negative) %>%
        as.numeric()
    
    if((resident_deaths > 25) | (resident_deaths < 18) | (is.na(resident_deaths))){
        warning("Deaths number seems to have been mis-scraped, please inspect")
    }
    
    # see which row starts employee data
    emp_idx <- first(which(str_detect(x$Name, "(?i)employee")))
    staff_df <- x[emp_idx:(emp_idx+5),] 
    names(staff_df) <- str_replace(names(staff_df), "Residents", "Staff")
    
    staff_out <- staff_df %>% 
        select(-starts_with("Drop"), -Staff.Population, Staff.Active) %>%
        filter(!is.na(Staff.Confirmed)) %>% 
        filter(!str_detect(Staff.Confirmed, "(?i)cumulative")) %>% 
        filter(!str_detect(Name, "(?i)total")) %>% 
        mutate(Name = str_c(clean_fac_col_txt(Name), " staff total")) %>% 
        clean_scraped_df() 
    
    out <- staff_out %>%
        bind_rows(fac_df) %>%
        rename(Staff.Tested = Staff.Tadmin) %>% 
        bind_rows(tibble(Name = "State-Wide", Residents.Deaths = resident_deaths))
    
    return(out)
}

#' Scraper class for general West Virginia COVID data
#' 
#' @name west_virginia_scraper
#' @description WV data resides in a tsv file hosted on the doc website. Staff
#' information is only provided at the state wide table and a number of coding
#' steps are necessary to extract the information.
#' \describe{
#'   \item{Facility}{The facility name}
#'   \item{County}{County location}
#'   \item{Pop}{Resident population}
#'   \item{Negative}{Residents negative}
#'   \item{Positive active}{Residents currently infected}
#'   \item{Pending}{Resident tests pending, all inmates/residents awaiting test results remain in isolation}
#'   \item{Total}{cumulative tests refers to number of tests performed and completed, NOT the number of inmates or residents tested}
#'   \item{Recovered}{Residents recovered}
#'   \item{Cumulative Cases}{Residents cumulative cases}
#'   \item{Quarantine}{Residents in quarantine}
#'   \item{Staff Negative}{Statewide only}
#'   \item{Staff Positive active}{Statewide only}
#'   \item{Staff Pending}{Statewide only}
#'   \item{Staff Total}{Statewide only}
#'   \item{Staff Recovered}{Statewide only}
#'   \item{Staff Cumulative}{Statewide only}
#'   \item{Resident Deaths}{Statewide only, deaths of positive inmates are not included in this data when medical opinion does not conclude COVID-19 caused or contributed to the death, pending any further medical determination. Deaths not included per this criteria: MOCC, 6}
#' }

west_virginia_scraper <- R6Class(
    "west_virginia_scraper",
    inherit = generic_scraper,
    public = list(
        log = NULL,
        initialize = function(
            log,
            url = "https://dhhr.wv.gov/COVID-19/Pages/Correctional-Facilities.aspx",
            id = "west_virginia",
            type = "pdf",
            state = "WV",
            jurisdiction = "state",
            check_date = west_virginia_check_date,
            # pull the JSON data directly from the API
            pull_func = west_virginia_pull,
            # restructuring the data means pulling out the data portion of the json
            restruct_func = west_virginia_restruct,
            # Rename the columns to appropriate database names
            extract_func = west_virginia_extract){
            super$initialize(
                url = url, id = id, pull_func = pull_func, type = type,
                restruct_func = restruct_func, extract_func = extract_func,
                log = log, state = state, jurisdiction = jurisdiction,
                check_date = check_date)
        }
    )
)

if(sys.nframe() == 0){
    west_virginia <- west_virginia_scraper$new(log=TRUE)
    west_virginia$run_check_date()
    west_virginia$raw_data
    west_virginia$pull_raw()
    west_virginia$raw_data
    west_virginia$save_raw()
    west_virginia$restruct_raw()
    west_virginia$restruct_data
    west_virginia$extract_from_raw()
    west_virginia$extract_data
    west_virginia$validate_extract()
    west_virginia$save_extract()
}

