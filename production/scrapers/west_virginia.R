source("./R/generic_scraper.R")
source("./R/utilities.R")

west_virginia_pull <- function(x){
    tsv_src <- get_src_by_attr(
        x, "a", attr = "href", attr_regex = "txt$")
    
    dev_null <- suppressWarnings(out <- read_tsv(
        tsv_src, skip = 1, col_types = cols()))
    
    out
}

west_virginia_restruct <- function(x){
    df_ <- x
    
    replace_col <- unname(unlist(df_[1,]))
    rcol_idx <- which(!is.na(replace_col))
    
    names(df_)[rcol_idx] <- replace_col[rcol_idx]
    
    df_
}

west_virginia_extract <- function(x){
    exp_cols <- c(
        Name = "Facility", 
        Drop.County = "County", 
        Residents.Population = "Pop.",
        Residents.Active = "Active cases",
        Residents.Recovered = "Recovered",
        Residents.Confirmed = "Cumulative Positives",
        Residents.Negative = "Negative",
        Residents.Pending = "Results Pending",
        Residents.Tadmin = "Total Tests",
        Residents.Quarantine = "Quarantine"
    )
    
    check_names(x, exp_cols)
    names(x) <- names(exp_cols)
    
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
        select(-starts_with("Drop"))
    
    # get deaths
    resident_deaths <- x %>%
        filter(str_starts(Name, "(?i)inmate deaths")) %>%
        pull(Name) %>%
        # remove items in parenthesis
        str_remove_all("\\([^)]*\\)") %>%
        # remove commas
        str_remove_all(",") %>%
        str_extract_all("[0-9]+") %>%
        unlist() %>%
        string_to_clean_numeric() %>%
        sum()
    
    # see which row starts employee data
    idb <- str_detect(
        x$Name, "(?i)Employee") & str_detect(x$Name, "(?i)totals")
    staff_df <- x[first(which(idb)),]
    names(staff_df) <- str_replace(names(staff_df), "Residents", "Staff")
    
    staff_df %>%
        mutate(Name = "State-Wide") %>%
        select(-starts_with("Drop"), -Staff.Population, -Staff.Active) %>%
        select(-Staff.Quarantine) %>% 
        clean_scraped_df() %>%
        mutate(Residents.Deaths = resident_deaths) %>%
        bind_rows(fac_df)
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
            type = "csv",
            state = "WV",
            jurisdiction = "state",
            # pull the JSON data directly from the API
            pull_func = west_virginia_pull,
            # restructuring the data means pulling out the data portion of the json
            restruct_func = west_virginia_restruct,
            # Rename the columns to appropriate database names
            extract_func = west_virginia_extract){
            super$initialize(
                url = url, id = id, pull_func = pull_func, type = type,
                restruct_func = restruct_func, extract_func = extract_func,
                log = log, state = state, jurisdiction = jurisdiction)
        }
    )
)

if(sys.nframe() == 0){
    west_virginia <- west_virginia_scraper$new(log=TRUE)
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

