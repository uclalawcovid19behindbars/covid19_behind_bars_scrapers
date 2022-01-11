source("./R/generic_scraper.R")
source("./R/utilities.R")

ohio_population_check_date <- function(x, date = Sys.Date()){
    url_ <- x %>% 
        xml2::read_html() %>%
        rvest::html_nodes("a") %>%
        rvest::html_attr("href")
    
    # Extract all link texts
    link_ <- x %>%
        xml2::read_html() %>%
        rvest::html_nodes("a") %>%
        rvest::html_text()
    
    # Get url for latest pdf 
    tbl <- tibble(link = link_, url = url_) %>% 
        filter(stringr::str_detect(url_, "Portals")) %>% 
        mutate(Date = lubridate::mdy(link))
    
    latest_date <- max(tbl$Date)
    error_on_date(latest_date, expected_date = date)
}

ohio_population_pull <- function(x, exp_date = Sys.Date()){
    # Extract all urls 
    url_ <- x %>% 
        xml2::read_html() %>%
        rvest::html_nodes("a") %>%
        rvest::html_attr("href")
    
    # Extract all link texts
    link_ <- x %>%
        xml2::read_html() %>%
        rvest::html_nodes("a") %>%
        rvest::html_text()
    
    # Get url for latest pdf 
    tbl <- tibble(link = link_, url = url_) %>% 
        filter(stringr::str_detect(url_, "Portals")) %>% 
        mutate(Date = lubridate::mdy(link))
    
    latest_date <- max(tbl$Date)

    stringr::str_c(
        "https://drc.ohio.gov", 
        tbl %>% 
            filter(Date == latest_date) %>% 
            pull(url)
    )
}

.alt_ohio_population_restruct <- function(raw_pdf){
    alt_extract_pg2 <- raw_pdf %>%
        tabulizer::extract_tables(
            pages = 2, area = list(c(45, 50, 345, 565)))
    
    list(
        pg1c1 = cbind(
            raw_pdf %>%
                tabulizer::extract_tables(
                    pages = 1,
                    area = list(c(136.88, 55.0377, 745.5641, 238.3666))) %>%
                unlist(),
            raw_pdf %>%
                tabulizer::extract_tables(
                    pages = 1,
                    area = list(c(134, 252.6058, 750.9034, 306.00264))) %>%
                unlist()
        ),
        pg2c1 = alt_extract_pg2[[2]][,1:2],
        pg1c2 = cbind(
            raw_pdf %>%
                tabulizer::extract_tables(
                    pages = 1,
                    area = list(c(136.88, 308.4626, 745.5641, 505.281))) %>%
                unlist(),
            raw_pdf %>%
                tabulizer::extract_tables(
                    pages = 1,
                    area = list(c(134, 506.9218, 750.9034, 569.2479))) %>%
                unlist()
        ),
        pg2c2 = cbind(str_c(
            alt_extract_pg2[[2]][,3], 
            alt_extract_pg2[[2]][,4]), alt_extract_pg2[[2]][,5])
    )
}

ohio_population_restruct <- function(raw_pdf){
    
    # the resturucture for this code is tricky as the file went through a
    # pretty big change in the way that it was formatted at some point so there
    # are two code paths the screaper could take
    
    # this is the set of code corresponding to the newer file structure
    restruct_out <- list(
        pg1c1 = raw_pdf %>%
             tabulizer::extract_tables(
                 pages = 1,
                 area = list(c(108.81565, 33.26855, 764.22317, 284.02295))) %>%
            .[[1]], 
        pg2c1 = raw_pdf %>% 
             tabulizer::extract_tables(
                 pages = 2,
                 area = list(c(25.87326, 31.64027, 290.97897, 287.27950))) %>%
            .[[1]],
        pg1c1 = raw_pdf %>% 
            tabulizer::extract_tables(
                pages = 1,
                area = list(c(115.7680, 313.4366, 765.1223, 566.0635))) %>%
            .[[1]], 
        pg2c2 = raw_pdf %>% 
            tabulizer::extract_tables(
                pages = 2,
                area = list(c(27.42261, 315.65104, 290.50435, 563.66939))) %>%
            .[[1]]
        )
    
    exp_cols <- c("Institution", "Count")
    obs_cols <- colnames(restruct_out$pg1c1)
    
    # if these checks dont pass then revert to using the old way of extracting
    # the data
    if(ncol(restruct_out$pg1c1)!= 2 | !all(obs_cols == exp_cols)){
        restruct_out <- .alt_ohio_population_restruct(raw_pdf)
    }

    restruct_out
}

ohio_population_extract <- function(x){
    
    binded_mat <- do.call(rbind, x)
    
    if(ncol(binded_mat) != 2){
        warning(
            "Column structure not as expected. ",
            "See if raw file structure changed." 
        )
    }

    exp_cols <- c("Institution", "Count")
    if (!all(exp_cols == binded_mat[1,])){
        warning(
            "Column names not as expected. See if raw file structure changed." 
        )
    }
    
    if (!str_detect(binded_mat[nrow(binded_mat),1], "(?i)total population")){
        warning(
            "Total column on second page not as expected. ",
            "See if raw file structure changed." 
        )
    }
    
    total <- string_to_clean_numeric(binded_mat[nrow(binded_mat),2])
    
    colnames(binded_mat) <- c("Name", "Residents.Population")
    
    out_df <- as_tibble(binded_mat) %>% 
        filter(!str_detect(Name, "(?i)total")) %>% 
        filter(!str_detect(Residents.Population, "(?i)count")) %>% 
        filter(Name != "") %>% 
        clean_scraped_df() 
    
    if (sum_na_rm(out_df$Residents.Population) != total) {
        warning(
            "Total doesn't match sum of facilities. ",
            "See if raw file structure changed." 
        )
    }
    
    out_df
}

#' Scraper class for Ohio population data 
#' 
#' @name ohio_population_scraper
#' @description Ohio posts weekly population reports in PDF form. The reports 
#' also include (not scraped) population by gender and security leve, 
#' \describe{
#'   \item{Institition}{Name}
#'   \item{Count}{Population}
#' }

ohio_population_scraper <- R6Class(
    "ohio_population_scraper",
    inherit = generic_scraper,
    public = list(
        log = NULL,
        initialize = function(
            log,
            url = "https://drc.ohio.gov/reports/population",
            id = "ohio_population",
            type = "pdf",
            state = "OH",
            jurisdiction = "state",
            check_date = ohio_population_check_date,
            pull_func = ohio_population_pull,
            restruct_func = ohio_population_restruct,
            extract_func = ohio_population_extract){
            super$initialize(
                url = url, id = id, pull_func = pull_func, type = type,
                restruct_func = restruct_func, extract_func = extract_func,
                log = log, state = state, jurisdiction  = jurisdiction,
                check_date = check_date)
        }
    )
)

if(sys.nframe() == 0){
    ohio_population <- ohio_population_scraper$new(log=TRUE)
    ohio_population$run_check_date()
    ohio_population$raw_data
    ohio_population$pull_raw()
    ohio_population$raw_data
    ohio_population$save_raw()
    ohio_population$restruct_raw()
    ohio_population$restruct_data
    ohio_population$extract_from_raw()
    ohio_population$extract_data
    ohio_population$validate_extract()
    ohio_population$save_extract()
}

