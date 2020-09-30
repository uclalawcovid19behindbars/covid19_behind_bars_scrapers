library(magrittr)

get_src_by_attr <- function(
    base, css, xpath, attr, attr_regex, date_regex = NULL, date_format = "mdy"){
    
    html_src <- xml2::read_html(base)
    
    web_page_imgs <- rvest::html_nodes(html_src, css, xpath)
    
    srcs <- rvest::html_attr(web_page_imgs, attr)
    
    if(is.null(date_regex)){
        
        url_portion <- srcs[[grepl(attr_regex, srcs)]]
    }
    
    else{
        condition_df <- tibble::tibble(
            src_string = srcs,
            match_grep = grepl(attr_regex, srcs),
            date = lubridate::parse_date_time(
                stringr::str_extract(srcs, date_regex), date_format)) %>%
            dplyr::mutate(val = as.numeric(date)) %>%
            dplyr::mutate(val = ifelse(match_grep, val, -Inf))
        
        im_pos <- which(condition_df$val == max(condition_df$val))
        url_portion <- srcs[[im_pos]]
    }

    
    xml2::url_absolute(url_portion, base)
    
}

# For Arkansas
get_src_by_attr(
    base = "https://adc.arkansas.gov/coronavirus-covid-19-updates",
    css = "a",
    attr = "href",
    attr_regex = "(?i)stats_update",
    date_regex = "\\d+-\\d+-\\d+"
)
