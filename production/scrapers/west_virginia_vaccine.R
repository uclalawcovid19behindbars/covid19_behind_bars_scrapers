source("./R/generic_scraper.R")
source("./R/utilities.R")

west_virginia_vaccine_pull <- function(x){
    tsv_src <- get_src_by_attr(
        x, "a", attr = "href", attr_regex = "txt$") %>%
        {.[str_detect(., "vaccine")]}
    
    dev_null <- suppressWarnings(out <- read_tsv(
        tsv_src[1], skip = 1, col_types = cols()))
    
    out
}

west_virginia_vaccine_restruct <- function(x){
    df_ <- x
    
    if(!str_starts(names(df_)[1], "(?i)for week")){
        warning("Column names are not as expected. Please inspect.")
    }
    
    names(df_)[1] <- "Name"
    
    df_
}

west_virginia_vaccine_extract <- function(x){
    
    emp_idx <- first(which(str_detect(x$Name, "(?i)employee")))
    
    res_df <- x[1:(emp_idx-1),] %>%
        rename(
            Drop.County = "County", 
            Drop.Population = "Pop.",
            Residents.Initiated = "Vaccinated") %>%
        select(!starts_with("Drop")) %>%
        filter(!is.na(Residents.Initiated)) %>%
        filter(!str_detect(Name, "(?i)total")) %>%
        filter(!str_detect(Name, "(?i)residents")) %>%
        # J&J vaccine so these should all be the same
        mutate(Residents.Completed = Residents.Initiated) %>%
        mutate(Residents.Vadmin = Residents.Initiated) %>%
        clean_scraped_df()
    
    staff_df <- x[emp_idx:nrow(x),] %>%
        filter(str_detect(Name, "(?i)total"))
    
    names(staff_df) <- unlist(x[emp_idx,])
    
    staff_df %>%
        select(
            Staff.Initiated = `1st Doses`,
            Staff.Completed = `2nd Doses`,
            Staff.Population = `Staffing*`
            ) %>%
        mutate(Name = "State-Wide") %>%
        clean_scraped_df() %>%
        # moderna so this should work
        mutate(Staff.Vadmin = Staff.Initiated + Staff.Completed) %>%
        bind_rows(res_df)
}

#' Scraper class for general West Virginia COVID data
#' 
#' @name west_virginia_vaccine_scraper
#' @description WV vaccine data resides in a tsv file hosted on the doc website. Staff
#' information is only provided at the state wide table and a number of coding
#' steps are necessary to extract the information.
#' \describe{
#'   \item{Facility}{The facility name}
#'   \item{Staffing}{Staff.Population}
#'   \item{1st Doses}{Staff Initiated}
#'   \item{2nd Doses}{Staff Completed}
#'   \item{Vaccinated}{Residents.Initiated/Completed/Vadmin}
#' }

west_virginia_vaccine_scraper <- R6Class(
    "west_virginia_vaccine_scraper",
    inherit = generic_scraper,
    public = list(
        log = NULL,
        initialize = function(
            log,
            url = "https://dhhr.wv.gov/COVID-19/Pages/Correctional-Facilities.aspx",
            id = "west_virginia_vaccine",
            type = "csv",
            state = "WV",
            jurisdiction = "state",
            # pull the JSON data directly from the API
            pull_func = west_virginia_vaccine_pull,
            # restructuring the data means pulling out the data portion of the json
            restruct_func = west_virginia_vaccine_restruct,
            # Rename the columns to appropriate database names
            extract_func = west_virginia_vaccine_extract){
            super$initialize(
                url = url, id = id, pull_func = pull_func, type = type,
                restruct_func = restruct_func, extract_func = extract_func,
                log = log, state = state, jurisdiction = jurisdiction)
        }
    )
)

if(sys.nframe() == 0){
    west_virginia_vaccine <- west_virginia_vaccine_scraper$new(log=TRUE)
    west_virginia_vaccine$raw_data
    west_virginia_vaccine$pull_raw()
    west_virginia_vaccine$raw_data
    west_virginia_vaccine$save_raw()
    west_virginia_vaccine$restruct_raw()
    west_virginia_vaccine$restruct_data
    west_virginia_vaccine$extract_from_raw()
    west_virginia_vaccine$extract_data
    west_virginia_vaccine$validate_extract()
    west_virginia_vaccine$save_extract()
}

