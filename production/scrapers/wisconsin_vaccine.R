source("./R/generic_scraper.R")
source("./R/utilities.R")

wisconsin_vaccine_check_date <- function(x, date = Sys.Date()){
    app_src <- "https://public.tableau.com/views/WIDOCCOVID19/" %>%
        str_c(
            "COVID-19Vaccinations?%3Aembed=y&%3AshowVizHome=no&%3A",
            "host_url=https%3A%2F%2Fpublic.tableau.com%2F&%3A",
            "embed_code_version=3&%3Atabs=yes&%3Atoolbar=no&%3A",
            "animate_transition=yes&%3Adisplay_static_image=no&%3A",
            "display_spinner=no&%3Adisplay_overlay=yes&%3A",
            "display_count=yes&%3Alanguage=en&%3AloadOrderID=0")
    
    remDr <- initiate_remote_driver()
    del_ <- capture.output(remDr$open())
    remDr$navigate(app_src)
    Sys.sleep(6)
    
    base_html <- remDr$getPageSource()
    
    base_page <- xml2::read_html(base_html[[1]])
    
    base_page %>%
        rvest::html_node(xpath ="//span[contains(text(),'Updated')]") %>%
        rvest::html_text() %>%
        str_split("as of") %>%
        unlist() %>%
        last() %>% 
        str_remove_all("\\)|\\*") %>%
        lubridate::mdy() %>%
        error_on_date(date)
}

# Tableau downloads from Firefox aren't working  
# Download the csv file manually in Chrome and save it in this location 
wisconsin_vaccine_pull <- function(x){
    # if this is giving you trouble, try save-as'ing it in 
    # excel with UTF-8 .csv file encoding
    read.csv("/tmp/sel_dl/PIOC Vaccinated.csv")
}

wisconsin_vaccine_restruct <- function(x, exp_date = Sys.Date()){

    x_ <- x %>% 
        janitor::clean_names()
    
    check_names(x_, c(
        "facility", 
        "as_of_date_vaccinated_pioc", 
        "number_partially_vaccinated",
        "percent_partially_vaccinated", 
        "number_fully_vaccinated", 
        "percent_fully_vaccinated", 
        "number_partially_or_fully_vaccinated", 
        "percent_partially_or_fully_vaccinated", 
        "facility_population"
    ))
    
    x_
}

wisconsin_vaccine_extract <- function(x){
    x %>% 
        select(Residents.Initiated = number_partially_or_fully_vaccinated, 
               Residents.Completed = number_fully_vaccinated, 
               Residents.Initiated.Pct = percent_partially_or_fully_vaccinated,
               Residents.Completed.Pct = percent_fully_vaccinated,
               Name = facility) %>% 
        filter(!str_detect(Name, "(?i)total")) %>% 
        clean_scraped_df() %>%
        mutate(Residents.Initiated.Pct = as.numeric(Residents.Initiated.Pct) / 100,
               Residents.Completed.Pct = as.numeric(Residents.Completed.Pct) / 100 ) 
}

#' Scraper class for general COVID data
#' 
#' @name wisconsin_vaccine_scraper
#' @description Comes from the COVID-19 Vaccines tab in the dashboard. 
#' Firefox/Tableau compatibility is an issue, so the scraper assumes you'll 
#' manually download the csv and put it in /tmp/sel_dl/. 
#' Dashboard says data is updated on Monday of every week. 
#' \describe{
#'   \item{Facility_Name}{The facility name.}
#'   \item{As of Date (Vaccine)}{}
#'   \item{First Doses (Moderna or Pfizer)}{}
#'   \item{Second Doses (Moderna or Pfizer)}{}
#'   \item{Johnson & Johnson Doses}
#'   \item{Total Doses}
#' }

wisconsin_vaccine_scraper <- R6Class(
    "wisconsin_vaccine_scraper",
    inherit = generic_scraper,
    public = list(
        log = NULL,
        initialize = function(
            log,
            url = "https://doc.wi.gov/Pages/COVID19(Coronavirus)/COVID19TestingDashboard.aspx",
            id = "wisconsin_vaccine",
            type = "manual",
            state = "WI",
            jurisdiction = "state",
            check_date = wisconsin_vaccine_check_date,
            # pull the JSON data directly from the API
            pull_func = wisconsin_vaccine_pull,
            # restructuring the data means pulling out the data portion of the json
            restruct_func = wisconsin_vaccine_restruct,
            # Rename the columns to appropriate database names
            extract_func = wisconsin_vaccine_extract){
            super$initialize(
                url = url, id = id, pull_func = pull_func, type = type,
                restruct_func = restruct_func, extract_func = extract_func,
                log = log, state = state, jurisdiction = jurisdiction,
                check_date = check_date)
        }
    )
)

if(sys.nframe() == 0){
    wisconsin_vaccine <- wisconsin_vaccine_scraper$new(log=TRUE)
    wisconsin_vaccine$run_check_date()
    wisconsin_vaccine$raw_data
    wisconsin_vaccine$pull_raw()
    wisconsin_vaccine$raw_data
    wisconsin_vaccine$save_raw()
    wisconsin_vaccine$restruct_raw()
    wisconsin_vaccine$restruct_data
    wisconsin_vaccine$extract_from_raw()
    wisconsin_vaccine$extract_data
    wisconsin_vaccine$validate_extract()
    wisconsin_vaccine$save_extract()
}

