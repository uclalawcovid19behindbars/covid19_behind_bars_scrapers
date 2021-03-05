source("./R/generic_scraper.R")
source("./R/utilities.R")

historical_wi_pop_pull <- function(x, date, file = NULL){
    
    date <- as.Date(date, format = "%Y-%m-%d")
    year4 <- format.Date(date, "%Y")
    month <- format.Date(date, "%m")
    day <- format.Date(date, "%d")
    
    if (date %in% c("2020-01-24", "2020-03-20")){
        stringr::str_c("https://doc.wi.gov/DataResearch/WeeklyPopulationReports/", 
                       month, day, year4, "Corrected.pdf")
    } else{
        stringr::str_c("https://doc.wi.gov/DataResearch/WeeklyPopulationReports/", 
                       month, day, year4, ".pdf")   
    }
}

historical_wi_pop_restruct <- function(x, date = NULL){
    
    out1 <- magick::image_read_pdf(x, pages = 1) %>% 
        magick::image_crop("2550x1900+0+650") %>% 
        ExtractTable()
    
    out2 <- magick::image_read_pdf(x, pages = 2) %>% 
        magick::image_crop("2550x1700+0+200") %>% 
        ExtractTable()
    
    out3 <- magick::image_read_pdf(x, pages = 3) %>% 
        magick::image_crop("2550x700+0+200") %>% 
        ExtractTable()
    
    combined_out <- list()
    
    append(combined_out, c(out1, out2, out3))
}

wi_pop_clean_cols <- function(x) {
    x %>% 
        janitor::row_to_names(row_number = 1) %>% 
        janitor::clean_names(replace = c("[:digit:]" = ""))
}

historical_wi_pop_extract <- function(x, date = NULL){
    
    drop_names <- c(
        "", 
        "ADULT INSTITUTIONS", 
        "INSTITUTIONS CENTERS MSDF AODA INMATES TRANS UNITS", 
        "WRC", 
        "CONTRACT FACILITIES", 
        "SUBTOTAL-MALES ALL LOCATIONS", 
        "MAXIMUM SECURITY INST", 
        "MEDIUM SECURITY INST", 
        "MEDIUM SECURITY INST", 
        "CENTER SYSTEM", 
        "CONTRACT BEDS", 
        "FEMALE", 
        "MALE", 
        "SUB-TOTAL FEMALES ALL LOCATIONS", 
        "MINIMUM SECURITY INST", 
        "DIVISION OF JUVENILE CORRECTIONS", 
        "TOTAL ON-GROUNDS POPULATION 3", 
        "SUBTOTAL-MALES", 
        "SUBTOTAL-FEMALES")
    
        exp_names <- c("x", "design_capacity", "total_population", "dai", "dcc")
        clean_names <- c("Name", "Capacity.Drop", "Residents.Population", "DAI.Drop", "DCC.Drop")
        
        if (length(x) == 4) {
            x1 <- wi_pop_clean_cols(x[[1]])
            check_names(x1, exp_names)
            names(x1) <- clean_names
            
            x2 <- wi_pop_clean_cols(x[[2]])
            check_names(x2, exp_names)
            names(x2) <- clean_names
            
            x3 <- wi_pop_clean_cols(x[[3]]) 
            check_names(x3, exp_names)
            names(x3) <- clean_names
            
            x4 <- wi_pop_clean_cols(x[[4]])
            check_names(x4, c("x", "total_capacity", "total_supervised_population"))
            names(x4) <- c("Name", "Capacity.Drop", "Residents.Population")
        }
        
        if (length(x) == 5){
            x1 <- wi_pop_clean_cols(x[[1]])
            check_names(x1, exp_names)
            names(x1) <- clean_names
            
            x2 <- wi_pop_clean_cols(x[[2]])
            check_names(x2, exp_names)
            names(x2) <- clean_names
            
            x3 <- wi_pop_clean_cols(x[[3]]) 
            check_names(x3, exp_names)
            names(x3) <- clean_names
            
            x_ <- x[[4]]
            if (x_[1,1] == "WRC (DDES FACILITY)"){
                names(x_) <- clean_names
                x2 <- bind_rows(x2, x_)
            }
            
            x4 <- wi_pop_clean_cols(x[[5]])
            check_names(x4, c("x", "total_capacity", "total_supervised_population"))
            names(x4) <- c("Name", "Capacity.Drop", "Residents.Population")
        }
    
        out <- bind_rows(x1, x2, x3, x4) %>% 
            as_tibble() %>%
            select(Name, Residents.Population) %>% 
            mutate(Residents.Population = as.numeric(gsub("[^0-9]", "", Residents.Population)), 
                   Name = clean_fac_col_txt(Name, to_upper = TRUE)) %>% 
            filter(!Name %in% drop_names) %>%
            mutate(Name = case_when(
                Name %in% c("RACINE", "STURTEVANT TRANSITIONAL FACILITY") ~ 
                    "RACINE CORRECTIONAL INSTITUTION STURTEVANT TRANSITIONAL FACILITY",
                Name %in% c("LINCOLN HILLS SCHOOL", "COPPER LAKE SCHOOL") ~ 
                    "COPPER LAKE SCHOOL LINCOLN HILLS SCHOOL", 
                TRUE ~ Name)) %>%
            group_by(Name) %>% 
            summarise(Residents.Population = sum(Residents.Population)) %>% 
            ungroup() %>% 
            clean_scraped_df() 
        
        if (sum(out$Residents.Population) > 26000){
            warning(
                stringr::str_c("Total population ", sum(out$Residents.Population), 
                               " is higher than expected. Inspect for total rows that were not dropped."))
        }
        if (nrow(out) > 59){
            warning(
                stringr::str_c("Number of rows ", nrow(out), " does not match expected. ", 
                               "Inspect for total rows that were not dropped."))
        }
        
        out
}

#' Scraper class for Wisconsin historical population data 
#' 
#' @name historical_wi_pop_scraper
#' @description WI's DOC posts weekly population reports in PDF form. 
#' In addition to facility-level population, these reports also report Design Capacity, 
#' DAI, and DCC, which are not scraped. These reports are posted on Fridays and 
#' archived. The data is spread across multiple pages and formatted in different
#' ways across the pages. Total rows are removed and a few facilities are aggregated to 
#' match the aggregation of the COVID data from WI. The population PDFs also include
#' data on community supervision that is NOT scraped. 
#' \describe{
#'   \item{Design Capacity}{Design capacity is defined as the original design capacity 
#'   of the institution, based on industry standards, plus modifications and expansions. 
#'   It excludes beds and multiple bunking that were instituted to accommodate crowding.}
#'   \item{Total Population}{Population counts include inmates physically present at 
#'   12:00 A.M. plus reported beds held. This reporting policy applies to all DOC adult 
#'   inmate facilities and both in- and out-of-state contracted beds.}
#'   \item{DAI}{}
#'   \item{DCC}{}
#' }

historical_wi_pop_scraper <- R6Class(
    "historical_wisconsin_pop_scraper",
    inherit = generic_scraper,
    public = list(
        log = NULL,
        initialize = function(
            log,
            url = "https://doc.wi.gov/Pages/DataResearch/DataAndReports.aspx",
            id = "historical_wi_pop",
            type = "pdf",
            state = "WI",
            jurisdiction = "state",
            pull_func = historical_wi_pop_pull,
            restruct_func = historical_wi_pop_restruct,
            extract_func = historical_wi_pop_extract){
            super$initialize(
                url = url, id = id, pull_func = pull_func, type = type,
                restruct_func = restruct_func, extract_func = extract_func,
                log = log, state = state, jurisdiction = jurisdiction)
        }
    )
)

if(sys.nframe() == 0){
    historical_wi_pop <- historical_wi_pop_scraper$new(log = TRUE)
    historical_wi_pop$reset_date("DATE") 
    historical_wi_pop$raw_data
    historical_wi_pop$pull_raw(date = historical_wi_pop$date, .dated_pull = TRUE)
    historical_wi_pop$raw_data
    historical_wi_pop$save_raw()
    historical_wi_pop$restruct_raw(date = historical_wi_pop$date)
    historical_wi_pop$restruct_data
    historical_wi_pop$extract_from_raw(date = historical_wi_pop$date)
    historical_wi_pop$extract_data
    historical_wi_pop$validate_extract()
    historical_wi_pop$save_extract()
}
