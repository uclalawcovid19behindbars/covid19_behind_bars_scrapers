source("./R/generic_scraper.R")
source("./R/utilities.R")

michigan_pull <- function(x){
    mi_html <- xml2::read_html(x)
    
    img1 <- mi_html %>%
        rvest::html_nodes("img") %>%
        rvest::html_attr("src") %>%
        .[16] %>%
        magick::image_read()
    
    img1
}

michigan_restruct <- function(x){
    ExtractTable(x)
}

michigan_extract <- function(x){
    
    col_name_mat <- matrix(c(
        "Location", "0", "Name",
        "Prisoners Tested", "1", "Residents.Tested",
        "Total Prisoners Confirmed", "2", "Residents.Confirmed",	
        "Prisoners Negative", "3", "Residents.Negative",	
        "Active Positive Cases", "4", "Drop.Residents.Active",
        "Priosner Deaths", "5", "Residents.Deaths"
    ), ncol = 3, nrow = 6, byrow = TRUE)
    
    colnames(col_name_mat) <- c("check", "raw", "clean")
    col_name_df <- as_tibble(col_name_mat)
    
    check_names_extractable(x[[1]], col_name_df)
    
    rename_extractable(x[[1]], col_name_df)
    
    mi1 <- rename_extractable(x[[1]], col_name_df)
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
    
    mi1 %>%
        select(-starts_with("Drop")) %>%
        as_tibble()
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
            # pull the JSON data directly from the API
            pull_func = michigan_pull,
            # restructuring the data means pulling out the data portion of the json
            restruct_func = michigan_restruct,
            # Rename the columns to appropriate database names
            extract_func = michigan_extract){
            super$initialize(
                url = url, id = id, pull_func = pull_func, type = type,
                restruct_func = restruct_func, extract_func = extract_func,
                log = log, state = state)
        }
    )
)

if(sys.nframe() == 0){
    michigan <- michigan_scraper$new(log=TRUE)
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
