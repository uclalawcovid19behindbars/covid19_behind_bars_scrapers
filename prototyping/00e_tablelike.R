rm(list=ls())
library(magrittr)

getmode <- function(v){
    uniqv <- unique(v)
    uniqv[which.max(tabulate(match(v, uniqv)))]
}

ok_url <- stringr::str_c(
    "http://doc.publishpath.com/Default.aspx?shortcut=covid-19-stats-report")

ok_html <- xml2::read_html(ok_url)

p_elements <- ok_html %>%
    rvest::html_nodes("body") %>%
    rvest::html_nodes("p")
    
p_df <- p_elements %>%
    rvest::html_attr("style") %>%
    stringr::str_split_fixed(pattern="left", n = 2) %>%
    tibble::as_tibble(.name_repair = "minimal") %>%
    magrittr::set_colnames(c("V1", "V2")) %>%
    dplyr::group_by(V1) %>%
    dplyr::mutate(gcount = dplyr::n()) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(is_row = gcount == getmode(gcount)) %>%
    dplyr::mutate(p_id = 1:dplyr::n()) %>%
    dplyr::filter(is_row) %>%
    dplyr::mutate(row_id = dplyr::group_indices(., V1)) %>%
    dplyr::mutate(row_id = as.numeric(factor(row_id, unique(row_id)))) %>%
    dplyr::group_by(row_id) %>%
    dplyr::mutate(col_id = 1:dplyr::n()) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(text = rvest::html_text(p_elements[.$p_id]))

p_colnames_df <- p_elements %>%
    rvest::html_text() %>%
    {which(. == "Institution")} %>%
    dplyr::first() %>%
    .:(dplyr::first(p_df$p_id) - 1) %>%
    {p_elements[.]} %>%
    {tibble::tibble(
        text = rvest::html_text(.),
        style = rvest::html_attr(., "style"))} %>%
    dplyr::mutate(top_pos = stringr::str_split_fixed(
        style, "px;", 3)[,1]) %>%
    dplyr::mutate(top_pos = as.numeric(
        stringr::str_split_fixed(top_pos, "top:", 2)[,2])) %>%
    dplyr::mutate(col_id = cumsum(
        top_pos <= dplyr::lag(top_pos, default = Inf))) %>%
    dplyr::group_by(col_id) %>%
    dplyr::summarize(col_name = stringr::str_c(text, collapse = " ")) %>%
    dplyr::mutate(col_name = stringr::str_trim(
        stringr::str_replace_all(col_name, "#", "Number")))

out_df <- p_df %>%
    dplyr::left_join(p_colnames_df, by = "col_id") %>%
    dplyr::select(col_name, text, row_id) %>%
    tidyr::pivot_wider(names_from = col_name, values_from = text) %>%
    dplyr::select(-row_id)

out_df