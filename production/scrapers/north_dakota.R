source("./R/generic_scraper.R")
source("./R/utilities.R")

north_dakota_check_date <- function(x, date = Sys.Date()){
    base_html <- xml2::read_html(x)
    date_txt <- rvest::html_nodes(base_html, 
                                  xpath="/html/body/div[1]/div[5]/div/section/div[2]/article/div/div[2]/div/div[2]/div/div/div/div[1]/h3") %>%
        rvest::html_text()
    
    date_txt %>%
        {.[str_detect(., "(?i)21")]} %>%
        str_extract("\\d{1,2}-\\d{1,2}-\\d{2,4}") %>%
        lubridate::mdy() %>%
        error_on_date(expected_date = date)
}

north_dakota_pull <- function(x){
    remDr <- RSelenium::remoteDriver(
        remoteServerAddr = "localhost",
        port = 4445,
        browserName = "firefox"
    )
    
    del_ <- capture.output(remDr$open())
    remDr$navigate(x)
    
    remDr$getPageSource() %>%
        {xml2::read_html(.[[1]])}
    
}

north_dakota_restruct <- function(x){
    svg_charts <- x %>%
        rvest::html_nodes("svg")
    
    table_names <- x %>%
        rvest::html_nodes(xpath="//h3[@id='']") %>%
        rvest::html_text()

    dat_list <- lapply(1:length(svg_charts), function(i){
    
        sub_svg <- svg_charts[[i]]
    
        group_ <- ifelse(i%%2, "Residents", "Staff")
    
        sub_col_names <- sub_svg %>%
            rvest::html_nodes(".highcharts-legend-item") %>%
            rvest::html_text() %>%
            str_replace_all(" ", ".") %>%
            {str_c(group_, ., sep = ".")}
    
        fac <- sub_svg %>%
            rvest::html_node(".highcharts-xaxis-labels") %>%
            rvest::html_nodes("text") %>%
            rvest::html_text()
    
        data_labels <- sub_svg %>%
            rvest::html_nodes(".highcharts-data-labels")
    
        new_df <- sapply(data_labels, function(d){
            d %>%
                rvest::html_nodes(".highcharts-text-outline") %>%
                rvest::html_text() %>%
                string_to_clean_numeric()}) %>%
            as_tibble(.name_repair = "minimal")
        
        names(new_df) <- sub_col_names
        
        new_df %>%
            mutate(Name = fac)
    })
    
    out_df <- tibble(Name=vector("character"))
    
    for(sdf in dat_list){
        out_df <- full_join(out_df, sdf, by = "Name")
    }
    
    out_df
}

north_dakota_extract <- function(x){
    
    check_names(x, c(
        "Name" = "Name", 
        "Residents.Positive", 
        "Residents.Recovered", 
        "Residents.Deaths", 
        "Staff.Positive", 
        "Staff.Recovered", 
        "Staff.Deaths", 
        "Residents.Total.Tests.Administered", 
        "Residents.Total.Individuals.Tested", 
        "Residents.Total.Individuals.Tested.Twice", 
        "Staff.Total.Tests.Administered", 
        "Staff.Total.Individuals.Tested", 
        "Staff.Total.Individuals.Tested.Twice", 
        "Residents.Total.First.Doses", 
        "Residents.Total.Second.Doses", 
        "Residents.Total.Single.Doses"))
    
    x %>%
        mutate(Residents.Confirmed = Residents.Deaths + Residents.Recovered +
                   Residents.Positive) %>%
        mutate(Staff.Confirmed = Staff.Deaths + Staff.Recovered +
                   Staff.Positive, 
               Residents.Initiated = Residents.Total.First.Doses + Residents.Total.Single.Doses, 
               Residents.Completed = Residents.Total.Second.Doses + Residents.Total.Single.Doses) %>%
        select(
            Name, Residents.Confirmed, Residents.Recovered, Residents.Deaths,
            Staff.Confirmed, Staff.Recovered, Staff.Deaths,
            Residents.Tadmin = Residents.Total.Tests.Administered,
            Staff.Tested = Staff.Total.Individuals.Tested, 
            Residents.Initiated, Residents.Completed, 
            Residents.Active = Residents.Positive, 
            Staff.Active = Staff.Positive
        ) %>% 
        clean_scraped_df()
}

#' Scraper class for general north_dakota COVID data
#' 
#' @name north_dakota_scraper
#' @description Data come from hicharts js loaded data. Scraper could be
#' improved by pulling data directly from tables, however, loading these
#' tables requires mouse clicks which are difficult to locate the location of.
#' Data is updated frequently. Started reporting vaccine data in March 2020. 
#' \describe{
#'   \item{Facility}{The facility name.}
#'   \item{Residents.Positive}{Active positive cases not cumulative}
#'   \item{Residents.Recovered}{}
#'   \item{Residents.Deaths}{}
#'   \item{Staff.Positive}{Active positive cases not cumulative}
#'   \item{Staff.Recovered}{}
#'   \item{Residents.Total.Tests.Administered}{}
#'   \item{Residents.Total.Individials.Tested}{}
#'   \item{Residents.Total.Individuals.Tested.Twice}{}
#'   \item{Staff.Total.Tests.Administered}{}
#'   \item{Staff.Total.Individials.Tested}{}
#'   \item{Staff.Total.Individuals.Tested.Twice}{}
#'   \item{Residents.First.Dose}{}
#'   \item{Residents.Second.Dose}{}
#'   \item{Residents.Single.Dose}{}
#' }

north_dakota_scraper <- R6Class(
    "north_dakota_scraper",
    inherit = generic_scraper,
    public = list(
        log = NULL,
        initialize = function(
            log,
            url = "https://www.docr.nd.gov/covid-19-information",
            id = "north_dakota",
            type = "html",
            state = "ND",
            jurisdiction = "state",
            check_date = north_dakota_check_date,
            # pull the JSON data directly from the API
            pull_func = north_dakota_pull,
            # restructuring the data means pulling out the data portion of the json
            restruct_func = north_dakota_restruct,
            # Rename the columns to appropriate database names
            extract_func = north_dakota_extract){
            super$initialize(
                url = url, id = id, pull_func = pull_func, type = type,
                restruct_func = restruct_func, extract_func = extract_func,
                log = log, state = state, jurisdiction = jurisdiction,
                check_date = check_date)
        }
    )
)

if(sys.nframe() == 0){
    north_dakota <- north_dakota_scraper$new(log=TRUE)
    north_dakota$run_check_date()
    north_dakota$raw_data
    north_dakota$pull_raw()
    north_dakota$raw_data
    north_dakota$save_raw()
    north_dakota$restruct_raw()
    north_dakota$restruct_data
    north_dakota$extract_from_raw()
    north_dakota$extract_data
    north_dakota$validate_extract()
    north_dakota$save_extract()
}
