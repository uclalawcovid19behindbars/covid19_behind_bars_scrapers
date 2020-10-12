library(tidyverse)

#' Gets a web element based on a specific attribute that matches regex
#'
#' @param base character string of the URL
#' @param css css option to search html nodes
#' @param xpath xpath option to search html nodes
#' @param attr character the attribute to pull a string from
#' @param attr_regex character regex to search in attributes
#' @param date_regex character string to extract date from attribute
#' @param date_fromat character the format to convert the date from
#' 
#' @return character of the url specified by the search

get_src_by_attr <- function(
    base, css, xpath, attr, attr_regex, date_regex = NULL, date_format = "mdy"){
    
    html_src <- xml2::read_html(base)
    
    web_page_imgs <- rvest::html_nodes(html_src, css, xpath)
    
    srcs <- rvest::html_attr(web_page_imgs, attr)
    
    if(is.null(date_regex)){
        
        url_portion <- srcs[grepl(attr_regex, srcs)]
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

clean_fac_col_txt <- function(x){
    # get rid of excessive white space
    str_squish(x) %>%
        # remove trailing stars
        str_remove("[\\*]+$") %>%
        # remove leading stars
        str_remove("^[\\*]+") %>%
        # capitalize COVID wherever its found
        str_replace_all("(?i)covid", "COVID") %>%
        # replace COVID - 19 with  some form of spaces with COVID-19
        str_replace_all("COVID[ ]*-[ ]*19", "COVID-19")
}


basic_check <- function(true_names, expected_names){
    if(length(true_names) != length(expected_names)){
        warning("Length of expected names does not match actual names")
    }
    
    for(i in 1:length(expected_names)){
        if(!is.na(expected_names[i]) & length(true_names) >= i){
            cexp <- clean_fac_col_txt(expected_names[i])
            ctrue <- clean_fac_col_txt(true_names[i])
            if(cexp != ctrue){
                warning(str_c(
                    "Extracted column ", i, " does not match expected name. ",
                    "Expected: ", cexp, " Received: ", ctrue))
            }
        }
    }
}

check_names <- function(DF, expected_names){
    true_names <- names(DF)
    basic_check(true_names, expected_names)
}

check_names_extractable <- function(df_, col_name_df){
    
    check_df <- full_join(
        col_name_df,
        tibble(
            raw = names(df_),
            true_ = clean_fac_col_txt(unname(unlist(df_[1,])))
        ),
        by = "raw")
    
    for(i in 1:nrow(check_df)){
        if(!is.na(check_df$check[i])){
            if(is.na(check_df$true_[i])){
                warning(str_c(
                    "Extracted column ", i, " does not match expected name. ",
                    "Expected: ", check_df$check[i], " Received: ", check_df$true_[i]))
            }
            else if(clean_fac_col_txt(check_df$check[i]) != check_df$true_[i]){
                warning(str_c(
                    "Extracted column ", i, " does not match expected name. ",
                    "Expected: ", check_df$check[i], " Received: ", check_df$true_[i]))
            }
        }
    }
}

rename_extractable <- function(df_, col_name_df){
    check_df <- full_join(
        col_name_df,
        tibble(
            raw = names(df_),
            true_ = clean_fac_col_txt(unname(unlist(df_[1,])))
        ),
        by = "raw")
    
    new_name_vec <- check_df$clean
    names(new_name_vec) <- check_df$raw
    
    renamed_df <- df_
    
    for(i in 1:ncol(renamed_df)){
        if(names(renamed_df)[i] %in% names(new_name_vec)){
            names(renamed_df)[i] <- new_name_vec[names(renamed_df)[i]]
        }
    }
    
    renamed_df
}

parse_response <- function(server_resp){
    jsonlite::fromJSON(httr::content(server_resp, "text", encoding = "UTF-8"))
}

## Some function from Cooper's code
replace_os <- function(cell){
    if (cell=="O") return("0")
    else return (cell)
}

## Function to Check credits usage
check_credits <- function(api_key=NULL) {
    if(is.null(api_key)){
        api_key <- Sys.getenv("EXTRACTABLE_API_KEY")
    }
    
    validate_endpoint = 'https://validator.extracttable.com'
    httr::content(httr::GET(
        url = validate_endpoint, httr::add_headers(`x-api-key` = api_key)), 
        as = 'parsed', type = 'application/json')
}

## Function to Retrieve the result by JobId
retrieve_result <- function(api_key, job_id) {
    retrieve_endpoint = "https://getresult.extracttable.com"
    return(
        httr::GET(
            url = paste0(retrieve_endpoint, "/?JobId=", job_id),
            httr::add_headers(`x-api-key` = api_key)
        )
    )
}

## Function to trigger a file for extraction
proces_file <- function(api_key, filepath) {
    trigger_endpoint = "https://trigger.extracttable.com"
    return (
        httr::POST(
            url = trigger_endpoint,
            httr::add_headers(
                `Content-Type`="multipart/form-data", `x-api-key` = api_key),
            body = list(input = httr::upload_file(filepath))
        )
    )
}

## Function to extract all tables from the input file
ExtractTable <- function(img, file_type = ".png", api_key = NULL) {
    if(is.null(api_key)){
        api_key <- Sys.getenv("EXTRACTABLE_API_KEY")
    }
    
    # TODO: this cant be the right way to check this? right
    if(class(img) == "magick-image"){
        f_ <- tempfile(fileext = file_type)
        magick::image_write(img, f_)
        img <- f_
    }
    
    server_response <- proces_file(api_key, img)
    parsed_resp <- parse_response(server_response)
    # Wait for a maximum of 5 minutes to finish the trigger job
    # Retries every 20 seconds
    max_wait_time = 5*60
    retry_interval = 20
    while (parsed_resp$JobStatus == 'Processing' & max_wait_time >= 0) {
        max_wait_time = max_wait_time - retry_interval
        message(paste0(
            "Job is still in progress. Let's wait for ", retry_interval, " seconds"))
        Sys.sleep(retry_interval)
        server_response <- retrieve_result(api_key, job_id=parsed_resp$JobId)
        parsed_resp <- parse_response(server_response)
    }
    ### Parse the response for tables
    et_tables <- httr::content(
        server_response, as = 'parsed', type = 'application/json')
    all_tables <- list()
    if (tolower(parsed_resp$JobStatus) != "success") {
        print(paste0("The processing was NOT SUCCESSFUL Below is the complete response from the server"))
        print(parsed_resp)
        return(all_tables)
    }
    ### Convert the extracted tabular JSON data as a dataframe for future use
    ### Each data frame represents one table
    for (i in 1:length(et_tables$Table)) {
        all_tables[[i]] <- sapply(et_tables$Tables[[i]]$TableJson, unlist) %>% 
            t() %>% as.data.frame()
    }
    return(all_tables)
} 

start_splash <- function(
    host = "splash.hrbrmstr.de", port = 8050,
    user = Sys.getenv("SPLASH_NAME"), pass = Sys.getenv("SPLASH_PASS")){
    splashr::splash(host = host, port = port, user = user, pass = pass)
    
}

Caps <- function(x) {
    s <- strsplit(x, " ")[[1]]
    paste(toupper(substring(s, 1,1)), substring(s, 2),
          sep="", collapse=" ")
}

numeric_from_css <- function(web, css_selector) {
    out <- web %>%
        html_nodes(., css_selector) %>%
        html_text() %>%
        string_to_clean_numeric()
    return(out)
}

clean_string <- function(string) {
    
    clean_string <- string %>% 
        gsub("[^[:alnum:][:blank:]?&/\\-]", "", .) %>%
        str_trim(., side=c("both"))
    Encoding(clean_string) <- "latin1" 
    clean_string <- iconv(clean_string, from = "latin1", "ASCII", sub=" ")
    
    # remove any double spaces
    clean_string <- str_replace_all(clean_string, pattern = " +", replacement = " ")
    
    # often, scraping misses "l"s, add them back
    clean_string <- clean_string %>%
        str_replace(pattern = "Correctiona[^l]", replacement = "Correctional ") %>% 
        str_replace(pattern = "Centra[^l]", replacement = "Central ") %>% 
        trimws()
    
    return(clean_string)
}

string_from_css <- function(web, css) {
    text <- html_nodes(web, css) %>% 
        html_text() %>% 
        clean_string()
    
    return(text)
}


# a function to clean scraped numbers
# removes commas
# converts letters o and O to the number zero 0
# strips any decimal points and numbers that follow
# removes unicode
# converts to numeric 

string_to_clean_numeric <- function(column) {
    
    if (all(is.na(column)) | is.numeric(column)) { return(column) }
    
    if (is.factor(column)) { column <- as.character(column) }
    # check if there are any nonnumeric characters we didn't expect
    expected.nonnumeric.values <- 
        c("-", "N/A", "na", "n/a", "NA", "", " +",
          NA, "T", "[", "]", "o", "O",
          "PENDING DOH RESULTS", "S", "")
    
    unexpected.nonnumeric.values <- column[
        !(grepl(column, pattern = "[0-9]+") | 
              column == "" | 
              column %in% expected.nonnumeric.values)
    ] %>% unique()
    
    if(length(unexpected.nonnumeric.values) != 0) {
        bad_str <- str_c(unexpected.nonnumeric.values, collapse = ", ")
        warning(str_c(
            "Unexpected nonnumeric values were scraped that we haven't",
            " seen before. They are... ", bad_str))
    }
    
    # clean
    
    # this sets up the unicode stripping later
    Encoding(column) <- "latin1" 
    
    column_clean <- column %>% 
        str_replace_all(., "o", "0") %>%
        str_replace_all(., "O", "0") %>%
        str_replace_all(., "^S$", "5") %>%
        str_replace_all(., "^s$", "5") %>%
        # a single "]" "[" and "T" are commonly scraped as 1
        str_replace(., "^\\]$", "1") %>%
        str_replace(., "^\\[$", "1") %>%
        str_replace(., "^T$", "1") %>%
        # remove any decimal points and numbers that follow
        str_remove_all(., "\\.[0-9]+") %>%
        # remove any nonnumeric chars
        str_remove_all( ., "[^0-9]") %>%
        # remove unicode
        iconv(., from = "latin1", "ASCII", sub="") %>%
        as.numeric()
    
    return(column_clean)
}

clean_scraped_df <- function(df) {
    
    # remove empty rows
    df <- df[rowSums(is.na(df)) < ncol(df), ]
    
    
    # if there are other string columns, ignore them
    cols_to_bind_back_on <- df %>%
        select_at(vars(matches("Facility"), matches("State"), matches("Date"), matches("Website"), matches("Notes"), matches("Coder"), matches("Address"), matches("City")))
    
    df <- df %>%
        select_at(vars(-matches("Facility"), -matches("State"), -matches("Date"), -matches("Website"), -matches("Notes"), -matches("Coder"), -matches("Address"), -matches("City")))
    
    
    df_clean <- df %>%
        mutate_at(vars(-Name), string_to_clean_numeric) %>%
        mutate(Name       = clean_string(Name))
    
    out <- bind_cols(df_clean, cols_to_bind_back_on)
    
    return(out)
}

info_scraper <- function(scraper){
    c(
        id = scraper$id,
        url = scraper$url,
        type = scraper$type,
        last_update = scraper$last_update()
    )
}

get_scraper_vec <- function(){
    # initiate all scrapers in the production folders
    sapply(list.files("production/scrapers", full.names = TRUE), source)
    
    # grab only the scrapers and put their names in a vector
    obj_space <- sapply(sapply(ls(), get), class)
    scraper_space <- sapply(obj_space, function(x) "R6ClassGenerator" %in% x)
    scraper_name_vec <- names(scraper_space[scraper_space]) %>%
        .[. != "generic_scraper"]
    names(scraper_name_vec) <- str_remove(scraper_name_vec, "_scraper")
    scraper_name_vec
}

document_scrapers <- function(){
    scraper_name_vec <- get_scraper_vec()
    bind_rows(lapply(scraper_name_vec, function(x) NULL))
}
