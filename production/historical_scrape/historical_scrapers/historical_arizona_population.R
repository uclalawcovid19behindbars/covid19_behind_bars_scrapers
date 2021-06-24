source("./R/generic_scraper.R")
source("./R/utilities.R")

historical_az_pop_pull <- function(x, date, file = NULL){
    
    if(date > lubridate::ymd("2020-10-19")){
        stop("Historical scraper should not be run past 2020-10-19 to not overlap with regular scraper.")
    }
    
    date <- as.Date(date, format = "%Y-%m-%d")
    month <- format.Date(date, "%b") %>% stringr::str_to_lower()
    year4 <- format.Date(date, "%Y")
    year2 <- format.Date(date, "%y")
    day <- format.Date(date, "%d")

    # July URL is 4 letters, all other months are 3 
    month <- ifelse(month == "jul", "july", month)
    
    stringr::str_c(
        "https://corrections.az.gov/sites/default/files/REPORTS/Monthly_CP/bed_capacity_", 
        year4, "/bed-capacity_", month, year2, ".pdf")
}

historical_az_pop_restruct <- function(x, date = NULL){
    
    # Verify date matches 
    date_pdf <- magick::image_read_pdf(x) %>%  
        magick::image_crop("1300x400+1000x1000") %>% 
        tesseract::ocr(., engine = tesseract::tesseract("eng")) %>% 
        str_match("(?<=MONTH ENDING)(.*)") %>% 
        lubridate::mdy() %>% 
        head(1)
    
    if (date != date_pdf) {
        warning(str_c("Extracted date ", date_pdf, " does not match expected date ", date))
    }
    
    magick::image_read_pdf(x) %>%
        ExtractTable()
}

historical_az_pop_extract <- function(x, date = NULL){
    
    col_name_mat <- matrix(c(
        "Operating Capacity (R+T=OC)", "X0", "Name", 
        "Rated (A)", "X1", "Capacity.Max.Rated", 
        "Temp. (B)", "X2", "Capacity.Max.Temp", 
        "Rated (C)", "X3", "Capacity.Close.Rated", 
        "Temp. (D)", "X4", "Capacity.Close.Temp", 
        "Rated (E)", "X5", "Capacity.Med.Rated", 
        "Temp. (F)", "X6", "Capacity.Med.Temp", 
        "Rated (G)", "X7", "Capacity.Min.Rated", 
        "Temp. (H)", "X8", "Capacity.Min.Temp.", 
        "Capacity Beds (A-H)", "X9", "Capacity.Total",
        "Beds", "X10", "Capacity.Special", 
        "in Operating Beds", "X11", "Population.Operating", 
        "in Special Beds", "X12", "Population.Special", 
        "Inmate Population", "X13", "Residents.Population"
    ), ncol = 3, nrow = 14, byrow = TRUE)
    
    colnames(col_name_mat) <- c("check", "raw", "clean")
    col_name_df <- as_tibble(col_name_mat)
    
    df_ <- as.data.frame(x) %>% 
        tail(-3)
    
    check_names_extractable(df_, col_name_df)
    
    keep_names <- c(
        "Douglas", 
        "Eyman", 
        "Florence", 
        "Lewis", 
        "Perryville Female",
        "Phoenix", 
        "Safford", 
        "Tucson", 
        "Winslow", 
        "Yuma", 
        "Central AZ Correc Facility GEO", 
        "Florence-West GEO", 
        "Phoenix-West GEO", 
        "Kingman GEO", 
        "Marana MTC", 
        "RED ROCK STATE PRISON"
    )
    
    out <- rename_extractable(df_, col_name_df) %>% 
        as_tibble() %>% 
        mutate(Residents.Population = as.numeric(gsub("[^0-9]", "", Residents.Population))) %>% 
        select(Name, Residents.Population) %>% 
        clean_scraped_df() %>% 
        # Aggregate Red Rock facility rows to match COVID data 
        mutate(Name = ifelse(Name %in% c("Red Rock-GP CCA", "Red Rock-Det CCA"), "RED ROCK STATE PRISON", Name)) %>% 
        group_by(Name) %>% 
        summarise(Residents.Population = sum(Residents.Population)) %>% 
        ungroup() %>% 
        filter(Name %in% keep_names) 
    
    if (nrow(out) != 16){
        warning("Length of expected facilities does not match actual length")
    }
    
    out 
}

#' Scraper class for Arizona historical population data 
#' 
#' @name historical_az_pop_scraper
#' @description Arizona's DOC posts month population reports in PDF form. In addition 
#' to facility-level population, these reports also report operating capacity for 
#' maximum, close, medium, and minimum security beds separately, along with the number of 
#' incarcerated people in operating beds and special beds. On 10/19, we began scraping
#' population data directly from the COVID dashboard, so this scraper should not overlap
#' with that. 
#' 
#' \describe{
#'   \item{Max rated beds operating capacity}{}
#'   \item{Max temporary beds operating capacity}{}
#'   \item{Close rated beds operating capacity}{}
#'   \item{Close temporary beds operating capacity}{}
#'   \item{Medium rated beds operating capacity}{}
#'   \item{Medium temporary beds operating capacity}{}
#'   \item{Minimum rated beds operating capacity}{}
#'   \item{Minimum temporary beds operating capacity}{}
#'   \item{Total operating capacity beds}{}
#'   \item{Special use beds}{}
#'   \item{Inmate population in operating beds}{}
#'   \item{Inmate population in special beds}{}
#'   \item{Total inside inmate population}{Residents.Population}
#' }

historical_az_pop_scraper <- R6Class(
    "historical_arizona_pop_scraper",
    inherit = generic_scraper,
    public = list(
        log = NULL,
        initialize = function(
            log,
            url = "https://corrections.az.gov/capacity-custody-level",
            id = "historical_az_pop",
            type = "pdf",
            state = "AZ",
            jurisdiction = "state",
            check_date = NULL,
            pull_func = historical_az_pop_pull,
            restruct_func = historical_az_pop_restruct,
            extract_func = historical_az_pop_extract){
            super$initialize(
                url = url, id = id, pull_func = pull_func, type = type,
                restruct_func = restruct_func, extract_func = extract_func,
                log = log, state = state, jurisdiction  = jurisdiction,
                check_date = check_date)
        }
    )
)

if(sys.nframe() == 0){
    historical_az_pop <- historical_az_pop_scraper$new(log=TRUE)
    historical_az_pop$reset_date("DATE")
    historical_az_pop$raw_data
    historical_az_pop$pull_raw(date = historical_az_pop$date, .dated_pull = TRUE)
    historical_az_pop$raw_data
    historical_az_pop$save_raw()
    historical_az_pop$restruct_raw(date = historical_az_pop$date)
    historical_az_pop$restruct_data
    historical_az_pop$extract_from_raw(date = historical_az_pop$date)
    historical_az_pop$extract_data
    historical_az_pop$validate_extract()
    historical_az_pop$save_extract()
}
