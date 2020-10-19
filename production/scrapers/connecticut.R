source("./R/generic_scraper.R")
source("./R/utilities.R")

connecticut_pull <- function(x){
    ct_img2 <- xml2::read_html(x) %>%
        rvest::html_nodes("img") %>%
        rvest::html_attr("src") %>%
        .[4] %>%
        {str_c(x, .)}
    
    magick::image_read(ct_img2)
}

connecticut_restruct <- function(x){
    list(
        et_ct = ExtractTable(x),
        state = x %>%
            magick::image_crop("700x80+0+120") %>%
            magick::image_ocr() %>%
            str_replace_all("Total", "\nTotal") %>%
            str_replace_all("Covid-19", "\nCovid-19") %>%
            str_split("\\n") %>%
            unlist() %>%
            .[. != ""] %>%
            str_squish() %>%
            str_split_fixed(": ", 2) %>%
            as.data.frame()
    )
}

connecticut_extract <- function(x){

    st_exp <- c(
        Drop.Pos = "Total Offender Positives",
        Residents.Recovered = "Total Offenders Recovered",
        Drop.Hosp = "Covid-19 Offenders in Hospital",
        Residents.Deaths = "Covid-19 Offender Deaths")
    basic_check(x$state[,1], st_exp)
    
    sw_df <- as.data.frame(t(x$state[,2]))
    names(sw_df) <- names(st_exp)
    
    col_name_mat <- matrix(c(
        "FACILITY", "0", "Name",
        "TOTAL POSITIVES", "1", "Residents.Confirmed"
    ), ncol = 3, nrow = 2, byrow = TRUE)
    
    colnames(col_name_mat) <- c("check", "raw", "clean")
    col_name_df <- as_tibble(col_name_mat)
    
    check_names_extractable(x$et_ct[[1]], col_name_df)
    
    rename_extractable(x$et_ct[[1]], col_name_df) %>%
        filter(!str_detect(Name, "(?i)facility")) %>%
        full_join(
            sw_df %>%
                mutate(Name = "State-Wide"),
            by = "Name"
        ) %>%
        clean_scraped_df() %>%
        as_tibble() %>%
        select(-starts_with("Drop"))

}

#' Scraper class for general Connecticut COVID data
#' 
#' @name connecticut_scraper
#' @description This will be a description of Connecticut data and what the scraper
#' does
#' \describe{
#'   \item{Name}{The facility name.}
#'   \item{Total Positives}{Residents positives}
#'   \item{Offenders Recovered}{Residents recovered}
#'   \item{Offenders in Hospital}{Residents who are in the hospital not neccisarily quarantine}
#'   \item{Offenders Deaths}{Residents who have died with some connection to covid-19}
#' }

connecticut_scraper <- R6Class(
    "connecticut_scraper",
    inherit = generic_scraper,
    public = list(
        log = NULL,
        initialize = function(
            log,
            url = "https://portal.ct.gov/DOC/Common-Elements/Common-Elements/Health-Information-and-Advisories",
            id = "connecticut",
            type = "img",
            state = "CT",
            # pull the JSON data directly from the API
            pull_func = connecticut_pull,
            # restructuring the data means pulling out the data portion of the json
            restruct_func = connecticut_restruct,
            # Rename the columns to appropriate database names
            extract_func = connecticut_extract){
            super$initialize(
                url = url, id = id, pull_func = pull_func, type = type,
                restruct_func = restruct_func, extract_func = extract_func,
                log = log, state = state)
        }
    )
)

if(sys.nframe() == 0){
    connecticut <- connecticut_scraper$new(log=TRUE)
    connecticut$raw_data
    connecticut$pull_raw()
    connecticut$raw_data
    connecticut$save_raw()
    connecticut$restruct_raw()
    connecticut$restruct_data
    connecticut$extract_from_raw()
    connecticut$extract_data
    connecticut$validate_extract()
    connecticut$save_extract()
}

