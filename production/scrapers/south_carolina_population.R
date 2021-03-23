source("./R/generic_scraper.R")
source("./R/utilities.R")

south_carolina_population_restruct <- function(x, exp_date = Sys.Date()){
    
    # Check date 
    pdf_date <- x %>% 
        magick::image_read_pdf() %>% 
        magick::image_crop("340x150+1900+100") %>% 
        magick::image_ocr() %>% 
        lubridate::mdy()
    
    error_on_date(pdf_date, exp_date)
    
    # Check header (first row)
    exp_header <- c("Institution or Center", "General Housing", 
                    "Restrictive Housing", "Programs", "Total")
    
    header <- x %>% 
        magick::image_read_pdf() %>% 
        magick::image_crop("2100x100+100+230") %>% 
        magick::image_ocr() %>% 
        stringr::str_split("\\|") %>% 
        .[[1]] %>% 
        matrix(nrow = 1, ncol = 5) %>% 
        as.data.frame() %>% 
        janitor::row_to_names(row_number = 1) 
    
    check_names(header, exp_header)
    
    # Check column names (row below header)
    x_ <- x %>% 
        magick::image_read_pdf() %>% 
        magick::image_crop("3000x2200+0+300") %>% 
        magick::image_convert(type = 'Bilevel') %>% 
        ExtractTable() %>% 
        as.data.frame()
    
    col_name_mat <- matrix(c(
        "Name", "X0", "", 
        "Gen.OpCap.Drop", "X1", "Operating Capacity",
        "Gen.PhysCount.Drop", "X2", "Physical Count",
        "Gen.OOS.Drop", "X3", "Out of Service",
        "Gen.UR.Drop", "X4", "Utilization Rate",
        "RH.OpCap.Drop", "X5", "Operating Capacity",
        "RH.PhysCount.Drop", "X6", "Physical Count",
        "RH.OOS.Drop", "X7", "Out of Service",
        "RH.UR.Drop", "X8", "Utilization Rate",
        "Prog.OpCap.Drop", "X9", "Operating Capacity",
        "Prog.PhysCount.Drop", "X10", "Physical Count",
        "Prog.OOS.Drop", "X11", "Out of Service",
        "Prog.UR.Drop", "X12", "Utilization Rate",
        "Total.OpCap.Drop", "X13", "Operating Capacity",
        "Residents.Population", "X14", "Physical Count",
        "Total.OOS.Drop", "X15", "Out of Service",
        "Total.UR.Drop", "X16", "Utilization Rate",
        "Filled.Triple.Drop", "X17", "Number of Filled Triple Cells"
    ), ncol = 3, nrow = 18, byrow = TRUE)
    
    colnames(col_name_mat) <- c("clean", "raw", "check")
    col_name_df <- as_tibble(col_name_mat)
    
    check_names_extractable(x_, col_name_df)
    rename_extractable(x_, col_name_df) 
}

south_carolina_population_extract <- function(x){
    x %>% 
        select(!ends_with(".Drop")) %>% 
        mutate(Name = clean_fac_col_txt(Name, to_upper = TRUE)) %>% 
        filter(!str_detect(Name, "TOTAL")) %>% 
        filter(!Residents.Population %in% c("Physical Count", ""))  %>% 
        mutate(Name = ifelse(str_detect(Name, "KIRKLAND"), "KIRKLAND", Name), 
               Name = ifelse(str_detect(Name, "GRAHAM"), "GRAHAM", Name)) %>% 
        clean_scraped_df() %>% 
        group_by(Name) %>% 
        summarise(Residents.Population = sum_na_rm(Residents.Population)) %>% 
        ungroup()
}

#' Scraper class for South Carolina population data 
#' 
#' @name south_carolina_population_scraper
#' @description South Carolina posts daily population reports in PDF form 
#' (historical files are NOT archived). These include facility-level population 
#' counts and capacities for general housing, restrictive housing, programs, 
#' totals, and the number of filled triple cells. For each category, the 
#' operating capacity, physical count, out of service count, and utilization 
#' rates are shown. We pull Residents.Population from the Total Physical Count.  
#' To match COVID aggregations, all Kirkland facilities are grouped  and all 
#' Graham facilities are grouped with the main CI. 
#' 
#' The General Housing category includes beds for inmates not 
#' designated/requiring "special" supervision and/or services. The Restrictive 
#' Housing category includes beds for inmates designated/requiring "special" 
#' supervision such as crisis intervention, deathrow, hospital, maximum custody, 
#' mental health, protective custody, pre-hearing detention, security detention, 
#' safekeeper, and temporary holding (transient). The Programs category includes 
#' beds for inmates specific locations for program participation such as assisted 
#' living, addictions treatment, Educational Finance Act eligible inmates, 
#' habilitation, handicap, Youthful Offender Act programs, reception/evaluation, 
#' shock incarceration, transitional care, HIV therapeutic, and sex offender treatment.
#' 
#' \describe{
#'   \item{Operating Capacity}{}
#'   \item{Physical Count}{}
#'   \item{Out of Service}{}
#'   \item{Utilization Rate}{}
#' }

south_carolina_population_scraper <- R6Class(
    "south_carolina_population_scraper",
    inherit = generic_scraper,
    public = list(
        log = NULL,
        initialize = function(
            log,
            url = "http://www.doc.sc.gov/research/SystemOverview/population-report.pdf",
            id = "south_carolina_population",
            type = "pdf",
            state = "SC",
            jurisdiction = "state",
            pull_func = function(x){x},
            restruct_func = south_carolina_population_restruct,
            extract_func = south_carolina_population_extract){
            super$initialize(
                url = url, id = id, pull_func = pull_func, type = type,
                restruct_func = restruct_func, extract_func = extract_func,
                log = log, state = state, jurisdiction  = jurisdiction)
        }
    )
)

if(sys.nframe() == 0){
    south_carolina_population <- south_carolina_population_scraper$new(log=TRUE)
    south_carolina_population$raw_data
    south_carolina_population$pull_raw()
    south_carolina_population$raw_data
    south_carolina_population$save_raw()
    south_carolina_population$restruct_raw()
    south_carolina_population$restruct_data
    south_carolina_population$extract_from_raw()
    south_carolina_population$extract_data
    south_carolina_population$validate_extract()
    south_carolina_population$save_extract()
}
