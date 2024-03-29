source("./R/generic_scraper.R")
source("./R/utilities.R")

michigan_date_check <- function(x, date = Sys.Date()){
    base_html <- rvest::read_html(x)
    
    base_html %>% 
        rvest::html_nodes("h2") %>% 
        rvest::html_text() %>% 
        {.[str_detect(., "(?i)updated")]} %>% 
        last() %>% 
        str_split("–") %>% 
        unlist() %>% 
        {.[str_detect(., "(?i)updated")]} %>% 
        lubridate::mdy() %>% 
        error_on_date(date)
}

michigan_pull <- function(url){
    mi_html <- xml2::read_html(url)
    
    data_div <- rvest::html_node(
        mi_html,
        xpath = "//h1[contains(text(), 'Total Confirmed Prisoner')]/parent::div")

    resident_image <- rvest::html_nodes(data_div, "img") %>%
        .[2] %>%
        rvest::html_attr("src") %>%
        magick::image_read()
    
    return(resident_image)
}

michigan_restruct <- function(x){
    out_list <- ExtractTable(x)

    names(out_list[[1]]) <- c("Location", "Prisoners Tested", "Total Prisoners Confirmed",
                              "Prisoners Negative", "Active Positive Cases", "Prisoner Deaths")
    return(out_list)
}

michigan_extract <- function(x){
    
    exp_names <- c(
        Name = "Location",
        Residents.Tested = "Prisoners Tested",
        Residents.Confirmed = "Total Prisoners Confirmed",
        Residents.Negative = "Prisoners Negative",
        Residents.Active = "Active Positive Cases",
        Residents.Deaths = "Prisoner Deaths"
    )
        
    df_ <- x[[1]]
    check_names(df_, exp_names)
    names(df_) <- names(exp_names)
    
    mi1 <- df_
    # remove header row
    mi1 <- filter(
        mi1, Name != "Location" & !grepl(Name, pattern = "Tota"))
    mi1 <- clean_scraped_df(mi1)
    # NA deaths mean 0
    mi1$Residents.Deaths[is.na(mi1$Residents.Deaths)] <- 0
    
    # scraper sometimes spells these wrong for m1
    mi1$Name[mi1$Name == "Detroit Rentry Center"] <- "Detroit Reentry Center"
    mi1$Name[mi1$Name == "Parnal Correctional Facility"] <- 
        "Parnall Correctional Facility"
    
    out_extract <- mi1 %>%
        select(-starts_with("Drop")) %>%
        as_tibble()
    
    return(out_extract)
}

#' Scraper class for general Michigan COVID data
#' 
#' @name michigan_scraper
#' @description MI resident data scraper, this information is pulled from a
#' separate table from staff data and though it says we are missing the
#' staff data below we shouldnt be concerned about that. Note that there does
#' not appear to be any sort of consistency or the position of this image
#' and as such this scraper is extremely error prone. Fixing this should be a
#' priority. Note that the column for Prisoners tested greatly underestimates
#' the number of tests administered which is reported elsewhere in the page.
#' \describe{
#'   \item{Location}{The facility name}
#'   \item{Prisoners Tested}{Residents tested, not tests adminstered}
#'   \item{Total Prisoners Confirmed}{Residents confirmed}
#'   \item{Prisoners Negative}{Residents negative}
#'   \item{Active Positive Cases}{Residents active}
#'   \item{Priosner Deaths}{Residents deaths}
#' }

michigan_scraper <- R6Class(
    "michigan_scraper",
    inherit = generic_scraper,
    public = list(
        log = NULL,
        initialize = function(
            log,
            url = "https://medium.com/@MichiganDOC/mdoc-takes-steps-to-prevent-spread-of-coronavirus-covid-19-250f43144337",
            id = "michigan",
            type = "img",
            state = "MI",
            jurisdiction = "state",
            check_date = michigan_date_check,
            # pull the JSON data directly from the API
            pull_func = michigan_pull,
            # restructuring the data means pulling out the data portion of the json
            restruct_func = michigan_restruct,
            # Rename the columns to appropriate database names
            extract_func = michigan_extract){
            super$initialize(
                url = url, id = id, pull_func = pull_func, type = type,
                restruct_func = restruct_func, extract_func = extract_func,
                log = log, state = state, jurisdiction = jurisdiction,
                check_date = check_date)
        }
    )
)

if(sys.nframe() == 0){
    michigan <- michigan_scraper$new(log=TRUE)
    michigan$run_check_date()
    michigan$raw_data
    michigan$pull_raw()
    michigan$raw_data
    michigan$save_raw()
    michigan$restruct_raw()
    michigan$restruct_data
    michigan$extract_from_raw()
    michigan$extract_data
    michigan$validate_extract()
    michigan$save_extract()
}

