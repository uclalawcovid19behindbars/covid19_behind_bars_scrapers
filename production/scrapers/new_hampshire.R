source("./R/generic_scraper.R")
source("./R/utilities.R")

new_hampshire_pull <- function(x){
    xml2::read_html(x)
}

new_hampshire_restruct <- function(x){
    tab_set <- x %>%
        rvest::html_nodes(xpath="//table[@border=1]")
    
    captions <- tab_set %>%
        rvest::html_text() %>%
        clean_fac_col_txt()
    
    staff_idx <- which(stringr::str_detect(captions, "(?i)staff testing"))
    rez_idx <- which(stringr::str_detect(captions, "(?i)residents testing"))
    
    res_pop <- x %>%
        rvest::html_node(xpath="/html/body/div[2]/div/main/div/div/div/div[2]/article/div/div[1]/div/div/div/div/div[3]/div/div/section[1]/p[5]/strong") %>%
        rvest::html_text()
    
    if(!str_detect(res_pop, "(?i)total resident population")){
        warning("Resident population value expected but not detected. Please inspect")
    }
    
    staff_pop <- x %>%
        rvest::html_node(xpath = "/html/body/div[2]/div/main/div/div/div/div[2]/article/div/div[1]/div/div/div/div/div[3]/div/div/section[2]/p[2]") %>% 
        rvest::html_text()
    
    if(!str_detect(staff_pop, "(?i)total number of nhdoc staff")){
        warning("Staff population value expected but not detected. Please inspect")
    }
    
    list(
        staff = tab_set[[staff_idx]] %>%
            rvest::html_table(header = TRUE) %>%
            as_tibble(),
        resident = tab_set[[rez_idx]] %>%
            rvest::html_table(header = TRUE) %>%
            as_tibble(),
        staff.pop = staff_pop,
        res.pop = res_pop
    )
}

new_hampshire_extract <- function(x){
    staff_df <- x$staff
    rez_df <- x$resident 
    ## pull out population numbers from paragraph text
    staff_pop <- x$staff.pop %>%
        str_extract('\\d+') %>% 
        as.numeric()
    res_pop <- x$res.pop %>%
        str_extract('\\d+') %>%
        as.numeric()
    
    names(staff_df) <- clean_fac_col_txt(names(staff_df))
    names(rez_df) <- clean_fac_col_txt(names(rez_df))
    
    rez_exp <- c(
        Name = "Facility",
        Residents.Tested = "Residents COVID-19 Tests Administered",
        Residents.Active = "Active Residents Positive",
        Residents.Confirmed =
            "Total Residents who have tested positive since March 2020",
        Residents.Deaths = "COVID-19 Deaths"
        )
    
    staff_exp <- c(
        Name = "Worksite",
        Staff.Confirmed = "Staff Positive - Total",
        Staff.Active = "Staff Positive - Active")
    
    check_names(staff_df, staff_exp)
    check_names(rez_df, rez_exp)
    
    names(staff_df) <- names(staff_exp)
    names(rez_df) <- names(rez_exp)
    
    full_join(
        staff_df %>%
            mutate(Name = clean_fac_col_txt(Name)) %>%
            mutate(Name = ifelse(Name == "SPU / RTU", "SPU & RTU", Name)) %>%
            filter(!str_detect(Name, "(?i)total|current|other|request")),
        rez_df %>%
            mutate(Name = clean_fac_col_txt(Name)) %>%
            mutate(Name = ifelse(Name == "SPU / RTU", "SPU & RTU", Name)) %>%
            filter(!str_detect(Name, "(?i)total|current|other|request")),
        by = "Name") %>%
        mutate(Residents.Population = NA,
               Staff.Population = NA) %>% 
        add_row(Name = "State-Wide", 
                Residents.Population = res_pop,
                Staff.Population = staff_pop) %>%
        select(-starts_with("Drop")) %>%
        clean_scraped_df()
}

#' Scraper class for general New Hampshire COVID data
#' 
#' @name new_hampshire_scraper
#' @description NH has two tables one for residents and one for staff. The info
#' provided is minimal but fairly easy to scrape. Both number tested and 
#' confirmed for staff and residents appear to be individuals confirmed
#' and tested rather than tests administered and tests that were positive.
#' \describe{
#'   \item{Name}{The facility name.}
#'   \item{Num Staff Positive}{The staff number testing positive}
#'   \item{Num Residents tested}{Not the number of tests administered}
#'   \item{Num Residents Positive}{Number of residents confirmed}
#'   \item{NHDOC Staff}{State wide staff population}
#'   \item{Total resident population}{State wide incarcerated population}
#' }

new_hampshire_scraper <- R6Class(
    "new_hampshire_scraper",
    inherit = generic_scraper,
    public = list(
        log = NULL,
        initialize = function(
            log,
            url = "https://www.covid19.nhdoc.nh.gov/covid-19-testing-information",
            id = "new_hampshire",
            type = "html",
            state = "NH",
            jurisdiction = "state",
            # pull the JSON data directly from the API
            pull_func = new_hampshire_pull,
            # restructuring the data means pulling out the data portion of the json
            restruct_func = new_hampshire_restruct,
            # Rename the columns to appropriate database names
            extract_func = new_hampshire_extract){
            super$initialize(
                url = url, id = id, pull_func = pull_func, type = type,
                restruct_func = restruct_func, extract_func = extract_func,
                log = log, state = state, jurisdiction = jurisdiction)
        }
    )
)

if(sys.nframe() == 0){
    new_hampshire <- new_hampshire_scraper$new(log=TRUE)
    new_hampshire$raw_data
    new_hampshire$pull_raw()
    new_hampshire$raw_data
    new_hampshire$save_raw()
    new_hampshire$restruct_raw()
    new_hampshire$restruct_data
    new_hampshire$extract_from_raw()
    new_hampshire$extract_data
    new_hampshire$validate_extract()
    new_hampshire$save_extract()
}

