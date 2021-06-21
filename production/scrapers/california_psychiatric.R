source("./R/generic_scraper.R")
source("./R/utilities.R")

california_psychiatric_pull <- function(x){
    resident <- get_src_by_attr(x, "img", attr = "src", attr_regex = "(?i)patient") %>%
        magick::image_read()
    
    staff <- get_src_by_attr(x, "img", attr = "src", attr_regex = "(?i)staff") %>%
        magick::image_read()
    
    magick::image_append(c(resident, staff), stack = TRUE)
}

california_psychiatric_restruct <- function(x){
    ExtractTable(x)
}

california_psychiatric_extract <- function(x){
    # Staff data
    staff <- as.data.frame(t(x[[1]]))
    
    names(staff) <- staff[1,]
    staff <- staff[-1, ]
    
    staff_expected_names <- c(
        "DSH COVID-19", 
        "Testing: 5/4/2021", 
        "Staff: Positive for COVID-19 Confirmed by Public Health or medical facility (Cumulative # since 3/20/2020)", 
        "Staff: Newly Positive for COVID-19 in the last 14 days", 
        "Other¹: Positive for COVID- 19 Confirmed by Public Health or medical facility (Cumulative # since 5/26/2020)", 
        "Other¹: Newly Positive for COVID-19 in the last 14 days")
    
    check_names(staff, staff_expected_names)
    names(staff) <- c("Drop.Header", "Name", "Staff.Confirmed", "Staff.Active", "Drop.OtherStaffConfirmed", "Drop.OtherStaffActive")
    
    staff <- staff %>%
        select(-starts_with("Drop")) 
    
    # Resident data   
    resident <- as.data.frame(t(x[[2]]))
    
    names(resident) <- resident[1, ]
    resident <- resident[-1, ]
    
    resident_expected_names <- c(
        "DSH COVID-19" ,
        "Testing: 5/4/2021",
        "",
        "Patients: Positive for COVID-19 (Cumulative # since 5/16/2020)",
        "Patients: Newly Positive for COVID-19 in the last 14 days",
        "Patients: Positive for COVID- 19 while at acute hospitalization",
        "Patients: Death² while patient was positive for COVID-19 (Cumulative # since 5/30/2020)",
        "",
        "Tests: Total # of tests administered (Cumulative # since 3/23/2020, includes retesting)")
    
    check_names(resident, resident_expected_names)
    names(resident) <- c("Drop.Header1", "Name", "Drop.Header2", "Residents.Confirmed", "Residents.Active", 
                         "Drop.Hospitalization", "Residents.Deaths", "Drop.Header3", "Residents.Tadmin")
    
    resident <- resident %>%
        select(-starts_with("Drop"))
    
    # Combining the tables and cleaning the final result
    df <- merge(resident, staff, all = TRUE)
    
    # !!! Important Note: values which are '<11' are assigned as NA
    for(i in 1:ncol(df)) { 
        df[[i]][which(df[i] == "<11")] <- NA
    }
    
    clean_scraped_df(df)
    
}

#' Scraper class for general california_psychiatric COVID data
#' 
#' @name california_psychiatric_scraper
#' @description Pull the resident data from the first image. This data is
#' facility specific so we need to grab the facility names from the first row.
#' Only need to grab three rows of relevant data. Any value that is less than
#' 11 should be treated as NA.
#' 
#' \describe{
#'   \item{Facility Name}{Name}
#'   \item{Patients Positive}{Residents.Confirmed}
#'   \iten{Current Positive Residents}{Residents.Active}
#'   \item{Patients Death}{Residents.Deaths}
#'   \item{Tests}{Residents.Tadmin}
#'   \item{Staff Positive}{Staff.Confirmed}
#'   \item{Current Positive Staff}{Staff.Active}
#' }

california_psychiatric_scraper <- R6Class(
    "california_psychiatric_scraper",
    inherit = generic_scraper,
    public = list(
        log = NULL,
        initialize = function(
            log,
            url = "https://www.dsh.ca.gov/COVID-19/Patient_and_Staff_COVID-19_Tracking.html",
            id = "california_psychiatric",
            type = "img",
            state = "CA",
            jurisdiction = "psychiatric",
            check_date = NULL,
            pull_func = california_psychiatric_pull,
            restruct_func = california_psychiatric_restruct,
            extract_func = california_psychiatric_extract){
            super$initialize(
                url = url, id = id, pull_func = pull_func, type = type,
                restruct_func = restruct_func, extract_func = extract_func,
                log = log, state = state, jurisdiction = jurisdiction,
                check_date = check_date)
        }
    )
)

if(sys.nframe() == 0){
    california_psychiatric <- california_psychiatric_scraper$new(log=TRUE)
    california_psychiatric$run_check_date()
    california_psychiatric$perma_save()
    california_psychiatric$raw_data
    california_psychiatric$pull_raw()
    california_psychiatric$raw_data
    california_psychiatric$save_raw()
    california_psychiatric$restruct_raw()
    california_psychiatric$restruct_data
    california_psychiatric$extract_from_raw()
    california_psychiatric$extract_data
    california_psychiatric$validate_extract()
    california_psychiatric$save_extract()
}
