source("./R/generic_scraper.R")
source("./R/utilities.R")

getmode <- function(v){
    uniqv <- unique(v)
    uniqv[which.max(tabulate(match(v, uniqv)))]
}

sum_slashes <- function(col) {
    for (i in 1:length(col)) {
        cell <- col[i]
        cell <- str_remove_all(cell, "\\*")
        if(grepl(cell, pattern = "/")) {
            tmp <- suppressWarnings(
                num1 <- as.numeric(str_split(cell, "/")[[1]][1]))
            tmp <- suppressWarnings(
                num2 <- as.numeric(str_split(cell, "/")[[1]][2]))
            cell  <- as.character(num1 + num2)
        }
        col[i] <- cell
    }
    return(col)
}

oklahoma_pull <- function(x){
    stop_defunct_scraper(x)
}

oklahoma_restruct <- function(ok_html){
    # p_elements <- ok_html %>%
    #     rvest::html_nodes("body") %>%
    #     rvest::html_nodes("p")
    # 
    # p_df <- p_elements %>%
    #     rvest::html_attr("style") %>%
    #     stringr::str_split_fixed(pattern="left", n = 2) %>%
    #     tibble::as_tibble(.name_repair = "minimal") %>%
    #     magrittr::set_colnames(c("V1", "V2")) %>%
    #     dplyr::group_by(V1) %>%
    #     dplyr::mutate(gcount = dplyr::n()) %>%
    #     dplyr::ungroup() %>%
    #     dplyr::mutate(is_row = gcount == getmode(gcount)) %>%
    #     dplyr::mutate(p_id = 1:dplyr::n()) %>%
    #     dplyr::filter(is_row) %>%
    #     dplyr::mutate(row_id = dplyr::group_indices(., V1)) %>%
    #     dplyr::mutate(row_id = as.numeric(factor(row_id, unique(row_id)))) %>%
    #     dplyr::group_by(row_id) %>%
    #     dplyr::mutate(col_id = 1:dplyr::n()) %>%
    #     dplyr::ungroup() %>%
    #     dplyr::mutate(text = rvest::html_text(p_elements[.$p_id]))
    # 
    # p_colnames_df <- p_elements %>%
    #     rvest::html_text() %>%
    #     {which(. == "Institution")} %>%
    #     dplyr::first() %>%
    #     .:(dplyr::first(p_df$p_id) - 1) %>%
    #     {p_elements[.]} %>%
    #     {tibble::tibble(
    #         text = rvest::html_text(.),
    #         style = rvest::html_attr(., "style"))} %>%
    #     dplyr::mutate(top_pos = stringr::str_split_fixed(
    #         style, "px;", 3)[,1]) %>%
    #     dplyr::mutate(top_pos = as.numeric(
    #         stringr::str_split_fixed(top_pos, "top:", 2)[,2])) %>%
    #     dplyr::mutate(col_id = cumsum(
    #         top_pos <= dplyr::lag(top_pos, default = Inf))) %>%
    #     dplyr::group_by(col_id) %>%
    #     dplyr::summarize(col_name = stringr::str_c(text, collapse = " ")) %>%
    #     dplyr::mutate(col_name = stringr::str_trim(
    #         stringr::str_replace_all(col_name, "#", "Number")))
    # 
    # ok_df <- p_df %>%
    #     dplyr::left_join(p_colnames_df, by = "col_id") %>%
    #     dplyr::select(col_name, text, row_id) %>%
    #     tidyr::pivot_wider(names_from = col_name, values_from = text) %>%
    #     dplyr::select(-row_id)
    # 
    # names(ok_df) <- stringr::str_squish(names(ok_df))
    # 
    # ok_df
}

oklahoma_extract <- function(x){
    # expected_names <- str_squish(c(
    #     "Institution", "Number of  Positive  Inmates",
    #     "Number of  Hospitalized  Inmates", "Number of  Recovered Inmates 3",
    #     "Number of Inmate  Deaths  Possibly  Related to  COVID-19 4", 
    #     "Number of  Confirmed  COVID-19  Related  Inmate  Deaths", 
    #     "Number of  Inmates in  Quarantine", 
    #     "Housing  Type (cell,  open bay,  combo)", 
    #     "Number of  Inmates in  Isolation", 
    #     "Number of Staff  Currently  Reporting  Positive  Tests 5", 
    #     "Number of  Recovered  Staff 5",
    #     "Number of Staff  Deaths  Possibly  Related to  COVID-19"))
    # check_names(x, expected_names)
    # 
    # names(x) <- c(
    #     "Name", "Residents.Confirmed", "C3", "Residents.Recovered",
    #     "Resident.Deaths.Presumed", "Resident.Deaths.Confirmed",
    #     "Residents.Quarantine", "C8", "Residents.Isolation",
    #     "Staff.Confirmed", "Staff.Recovered", "Staff.Deaths")
    # 
    # ok <- map_dfc(x, sum_slashes) %>%
    #     filter(Name != "Totals") %>%
    #     select(-C3, -C8)
    # 
    # ok <- clean_scraped_df(ok)
    # 
    # ok$Residents.Deaths <- ok$Resident.Deaths.Presumed + 
    #     ok$Resident.Deaths.Confirmed
    # ok$Residents.Quarantine <- ok$Residents.Quarantine + 
    #     ok$Residents.Isolation
    # 
    # ok <- select(
    #     ok, -Resident.Deaths.Presumed, -Resident.Deaths.Confirmed,
    #     -Residents.Isolation)
    # 
    # as_tibble(ok) %>%
    #     mutate(Residents.Confirmed = Residents.Confirmed +
    #                Residents.Recovered + Residents.Deaths) %>%
    #     mutate(Staff.Confirmed = Staff.Confirmed + Staff.Recovered +
    #                Staff.Deaths)
    NULL
}

#' Scraper class for general Oklahoma COVID data
#' 
#' @name oklahoma_scraper
#' @description Scraper needs to be rewritten to accomadet new structure of the
#' data.

oklahoma_scraper <- R6Class(
    "oklahoma_scraper",
    inherit = generic_scraper,
    public = list(
        log = NULL,
        initialize = function(
            log,
            url = 
                "http://doc.publishpath.com/Default.aspx?shortcut=covid-19-stats-report",
            id = "oklahoma",
            state = "OK",
            type = "html",
            jurisdiction = "state",
            # pull the JSON data directly from the API
            pull_func = oklahoma_pull,
            # 
            restruct_func = oklahoma_restruct,
            # Rename the columns to appropriate database names and do some minor
            # minor cleaning
            extract_func = oklahoma_extract){
            super$initialize(
                url = url, id = id, pull_func = pull_func, type = type,
                restruct_func = restruct_func, extract_func = extract_func,
                log = log, state = state, jurisdiction = jurisdiction)
        })
)

if(sys.nframe() == 0){
    oklahoma <- oklahoma_scraper$new(log=TRUE)
    oklahoma$raw_data
    oklahoma$pull_raw()
    oklahoma$raw_data
    oklahoma$save_raw()
    oklahoma$restruct_raw()
    oklahoma$restruct_data
    oklahoma$extract_from_raw()
    oklahoma$extract_data
    oklahoma$validate_extract()
    oklahoma$save_extract()
}