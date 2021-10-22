source("./R/generic_scraper.R")
source("./R/utilities.R")

virginia_psychiatric_date_check <- function(x, date = Sys.Date()){
    z <- get_src_by_attr(x, "a", attr="href", attr_regex = "(?i)covid-tracker")
    
    z %>%
        str_split("-") %>%
        unlist() %>%
        last() %>%
        str_remove(".pdf") %>%
        lubridate::ymd() %>%
        error_on_date(date)
}

virginia_psychiatric_pull <- function(x){
    get_src_by_attr(x, "a", attr="href", attr_regex = "(?i)covid-tracker")
}

virginia_psychiatric_restruct <- function(x){
    
    z <- magick::image_read_pdf(x)
    
    out1 <- z %>%
        magick::image_crop("4000x300+0+1700") %>%
        ExtractTable()
    
    groups <- z %>%
        magick::image_crop("4000x40+0+1600") %>%
        magick::image_ocr() %>% 
        str_remove_all("\\||\n") %>%
        str_squish() %>%
        str_split(" ") %>%
        unlist() %>%
        {ifelse(. == "R", "P", .)}

    facilties <- z %>%
        magick::image_crop("4000x40+0+1650") %>%
        magick::image_ocr() %>%
        str_remove_all("\\||\n") %>%
        str_squish() %>%
        str_split(" ") %>%
        unlist()
    
    name_vec <- c()
    fac_idx <- 1
    s_count <- 0
    p_count <- 0
    
    for(g in groups){
        
        if(g == "P"){
            if(p_count > 0){
                p_count <- 0
                s_count <- 0
                fac_idx <- fac_idx + 1
            }
            p_count <- 1
            name_vec <- c(name_vec, str_c(facilties[fac_idx], ".P"))
        }
        
        else if(g == "S"){
            if(s_count > 0){
                p_count <- 0
                s_count <- 0
                fac_idx <- fac_idx + 1
            }
            s_count <- 1
            name_vec <- c(name_vec, str_c(facilties[fac_idx], ".S"))
        }
        
        else{
            stop("data structure not as expected please inspect.")
        }
        
    }
    
    out1 %>%
        as.data.frame() %>%
        pivot_longer(-X0) %>%
        rename(Measure = "X0") %>%
        mutate(name = name_vec[as.numeric(str_remove(name, "X"))]) %>%
        mutate(Name = str_split_fixed(name, "\\.", 2)[,1]) %>%
        mutate(Group = str_split_fixed(name, "\\.", 2)[,2]) %>%
        mutate(Group = ifelse(Group == "S", "Staff", "Residents")) %>%
        mutate(Measure = case_when(
            Measure == "Positive" ~ "Active",
            Measure == "Deceased" ~ "Deaths",
            TRUE ~ Measure
        )) %>%
        mutate(col_name = str_c(Group, Measure, sep = ".")) %>%
        select(Name, value, col_name) %T>%
        # dont want warnings here
        {options(warn = -1)} %>%
        mutate(value = as.numeric(value)) %T>%
        {options(warn = 0)} %>% 
        pivot_wider(names_from = "col_name", values_from = "value")
}

virginia_psychiatric_extract <- function(x){
    x %>%
        mutate(Residents.Confirmed = Residents.Active + 
                   Residents.Deaths + Residents.Recovered) %>%
        mutate(Staff.Confirmed = Staff.Active + 
                   Staff.Deaths + Staff.Recovered) %>% 
        mutate(Residents.Tadmin = Residents.Confirmed + 
                   Residents.Negative + Residents.Pending)
}

#' Scraper class for general virginia_psychiatric COVID data
#' 
#' @name virginia_psychiatric_scraper
#' @description Data is stored in a pdf with a graph on top and facility level
#' data below. S indicates staff and P indicates patients and the names of the
#' facility are represented by the acronyms on top of the table. The variables
#' we would like corresponds to the following rows.
#' \describe{
#'   \item{Facility name}{The facility name}
#'   \item{Positive P}{Residents.Confirmed}
#'   \item{Positive S}{Staff.Confirmed}
#'   \item{Negative P}{Residents.Negative}
#'   \item{Negative S}{Staff.Negative}
#'   \item{Recovered P}{Residents.Recovered}
#'   \item{Recovered S}{Staff.Recovered}
#'   \item{Deceased P}{Residents.Deaths}
#'   \item{Deceased S}{Staff.Deaths}
#' }

virginia_psychiatric_scraper <- R6Class(
    "virginia_psychiatric_scraper",
    inherit = generic_scraper,
    public = list(
        log = NULL,
        initialize = function(
            log,
            url = "https://dbhds.virginia.gov/covid19",
            id = "virginia_psychiatric",
            type = "pdf",
            state = "VA",
            jurisdiction = "psychiatric",
            check_date = virginia_psychiatric_date_check,
            pull_func = virginia_psychiatric_pull,
            restruct_func = virginia_psychiatric_restruct,
            extract_func = virginia_psychiatric_extract){
            super$initialize(
                url = url, id = id, pull_func = pull_func, type = type,
                restruct_func = restruct_func, extract_func = extract_func,
                log = log, state = state, jurisdiction = jurisdiction,
                check_date = check_date)
        }
    )
)

if(sys.nframe() == 0){
    virginia_psychiatric <- virginia_psychiatric_scraper$new(log=TRUE)
    virginia_psychiatric$run_check_date()
    virginia_psychiatric$perma_save()
    virginia_psychiatric$raw_data
    virginia_psychiatric$pull_raw()
    virginia_psychiatric$raw_data
    virginia_psychiatric$save_raw()
    virginia_psychiatric$restruct_raw()
    virginia_psychiatric$restruct_data
    virginia_psychiatric$extract_from_raw()
    virginia_psychiatric$extract_data
    virginia_psychiatric$validate_extract()
    virginia_psychiatric$save_extract()
}

