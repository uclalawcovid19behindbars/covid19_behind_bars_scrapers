source("./R/generic_scraper.R")
source("./R/utilities.R")

wisconsin_population_pull <- function(x){
    get_src_by_attr(x, "a", attr = "href", attr_regex = "(?i)WeeklyPopulationReports/.*.pdf") %>% 
        first()
}

wisconsin_population_restruct <- function(x, exp_date = Sys.Date()){
    date <- x %>% 
        magick::image_read_pdf(pages = 1) %>% 
        magick::image_crop("600x100+650+250") %>% 
        magick::image_ocr() %>% 
        lubridate::mdy()
    
    error_on_date(date, exp_date)
    
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

# Remove footnote numbers in column names 
wi_pop_clean_cols <- function(x) {
    x %>% 
        janitor::row_to_names(row_number = 1) %>% 
        janitor::clean_names(replace = c("[:digit:]" = ""))
}

wisconsin_population_extract <- function(x){
    
    # Specify aggregate total rows to drop 
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
        "3 TOTAL ON-GROUNDS POPULATION", 
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
    
    # ExtractTable occasionally reads 5 tables instead of 4 
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
        
        # Consolidate rows to match COVID reporting aggregation 
        mutate(Name = case_when(
            Name %in% c("RACINE", "STURTEVANT TRANSITIONAL FACILITY") ~ 
                "RACINE CORRECTIONAL INSTITUTION STURTEVANT TRANSITIONAL FACILITY",
            Name %in% c("LINCOLN HILLS SCHOOL", "COPPER LAKE SCHOOL") ~ 
                "COPPER LAKE SCHOOL LINCOLN HILLS SCHOOL", 
            TRUE ~ Name)) %>%
        group_by(Name) %>% 
        summarise(Residents.Population = sum(Residents.Population), 
                  n_facs = n())
    
    # Check facilities being aggregated 
    exp_dupes <- c(
        "RACINE CORRECTIONAL INSTITUTION STURTEVANT TRANSITIONAL FACILITY", 
        "COPPER LAKE SCHOOL LINCOLN HILLS SCHOOL", 
        "ST CROIX")
    
    unexp_dupes <- out %>% 
        filter(n_facs > 1) %>% 
        filter(!Name %in% exp_dupes) 
    
    if (nrow(unexp_dupes) > 0){
        warning(
            stringr::str_c("The following duplicate facilies were not expected: ", 
                           unexp_dupes$Name))
    }

    # Check overall sum to ensure aggregated rows aren't added under new names 
    if (sum(out$Residents.Population) > 26000){
        warning(
            stringr::str_c("Total population ", sum(out$Residents.Population), 
                           " is higher than expected. Inspect for total rows that were not dropped."))
    }
    # Check number of facilities for the same reason 
    if (nrow(out) > 59){
        warning(
            stringr::str_c("Number of rows ", nrow(out), " does not match expected. ", 
                           "Inspect for total rows that were not dropped."))
    }
    
    out %>% 
        select(-n_facs) %>% 
        ungroup() %>% 
        clean_scraped_df() 
}

#' Scraper class for Wisconsin population data 
#' 
#' @name wisconsin_population_scraper
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

wisconsin_population_scraper <- R6Class(
    "wisconsin_population_scraper",
    inherit = generic_scraper,
    public = list(
        log = NULL,
        initialize = function(
            log,
            url = "https://doc.wi.gov/Pages/DataResearch/DataAndReports.aspx",
            id = "wisconsin_population",
            type = "pdf",
            state = "WI",
            jurisdiction = "state",
            check_date = NULL,
            pull_func = wisconsin_population_pull,
            restruct_func = wisconsin_population_restruct,
            extract_func = wisconsin_population_extract){
            super$initialize(
                url = url, id = id, pull_func = pull_func, type = type,
                restruct_func = restruct_func, extract_func = extract_func,
                log = log, state = state, jurisdiction = jurisdiction,
                check_date = check_date)
        }
    )
)

if(sys.nframe() == 0){
    wisconsin_population <- wisconsin_population_scraper$new(log=TRUE)
    wisconsin_population$run_check_date()
    wisconsin_population$raw_data
    wisconsin_population$pull_raw()
    wisconsin_population$raw_data
    wisconsin_population$save_raw()
    wisconsin_population$restruct_raw()
    wisconsin_population$restruct_data
    wisconsin_population$extract_from_raw()
    wisconsin_population$extract_data
    wisconsin_population$validate_extract()
    wisconsin_population$save_extract()
}
