library(tidyverse)
library(behindbarstools)

basic_check <- function(true_names, expected_names){
    if(length(true_names) != length(expected_names)){
        warning("Length of expected names does not match actual names")
    }
    
    for(i in 1:length(expected_names)){
        if(!is.na(expected_names[i]) & length(true_names) >= i){
            if(is.na(true_names[i])){
                warning(str_c(
                    "Extracted column ", i, " has name NA"))
            }
            else{
                cexp <- clean_fac_col_txt(expected_names[i])
                ctrue <- clean_fac_col_txt(true_names[i])
                if(cexp != ctrue){
                    warning(str_c(
                        "Extracted column ", i, " does not match expected ",
                        "name. Expected: ", cexp, " Received: ", ctrue))
                }
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

Caps <- function(x) {
    s <- strsplit(x, " ")[[1]]
    paste(toupper(substring(s, 1,1)), substring(s, 2),
          sep="", collapse=" ")
}

numeric_from_css <- function(web, css_selector) {
    out <- web %>%
        rvest::html_nodes(., css_selector) %>%
        rvest::html_text() %>%
        string_to_clean_numeric()
    return(out)
}

clean_string <- function(string) {
    
    clean_string <- string %>% 
        gsub("[^[:alnum:][:blank:]?&/\\-]", "", .) %>%
        stringr::str_trim(., side=c("both"))
    Encoding(clean_string) <- "latin1" 
    clean_string <- iconv(clean_string, from = "latin1", "ASCII", sub=" ")
    
    # remove any double spaces
    clean_string <- stringr::str_replace_all(
        clean_string, pattern = " +", replacement = " ")
    
    # often, scraping misses "l"s, add them back
    clean_string <- clean_string %>%
        stringr::str_replace(pattern = "Correctiona[^l]", replacement = "Correctional ") %>% 
        stringr::str_replace(pattern = "Centra[^l]", replacement = "Central ") %>% 
        trimws()
    
    return(clean_string)
}

string_from_css <- function(web, css) {
    text <- rvest::html_nodes(web, css) %>% 
        rvest::html_text() %>% 
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
          "PENDING DOH RESULTS", "PENDING DOH RESULT$", "S", "")
    
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

get_last_run <- function(file_name){
    scraper_name <- str_remove(str_split_fixed(file_name, "/", n = 3)[,3], ".R")
    
    out_files <- list.files(
        "results/extracted_data", full.names = TRUE,
        pattern = str_c("\\d+-\\d+-\\d+_", scraper_name, ".csv"))
    
    run_dates <- lubridate::ymd(str_extract(out_files, "\\d+-\\d+-\\d+"))
    
    last_file <- out_files[which.max(run_dates)]
    
    df_ <- read_csv(last_file, col_types = cols())
    
    list(cols = names(df_), date = max(run_dates))
}

build_notes_vec <- function(file_name){
    deets <- get_last_run(file_name)
    
    missing_vars <- setdiff(UCLABB_MAIN_VARIABLES, deets$cols)
    
    c("#'", str_c(
        "#' @details The last run of the scraper was on ", deets$date,
        " and contained the extracted columns: ",
        str_c(deets$cols, collapse = ", "), 
      ". We are missing the following core variables for the analysis: ",
      str_c(missing_vars, collapse = ", ")))
}

inject_notes <- function(roxygen_code, file_name){
    injection <- build_notes_vec(file_name)
    
    # find the last line of the documentation that has documentation notes
    last_comment_line <- max(which(str_starts(roxygen_code, "#'")))
    
    c(
        roxygen_code[1:last_comment_line],
        injection,
        roxygen_code[(last_comment_line + 1):length(roxygen_code)]
    )
    
}

fake_package <- function(
    file_name, working_directory = NULL, dependencies = NULL, ...){
    checkmate::assertCharacter(dependencies, null.ok = TRUE)
    if (is.null(working_directory)){ 
        working_directory <- file.path(
            tempdir(), basename(tempfile(pattern = "")))
    }
    package_name <- gsub("_", ".", sub(".[rRS]$|.Rnw$", "", basename(file_name), 
                                       perl = TRUE))
    package_directory <- file.path(working_directory, package_name)
    man_directory <- file.path(package_directory, "man")
    dir.create(working_directory, showWarnings = FALSE, recursive = TRUE)
    roxygen_code <- document::get_lines_between_tags(file_name, ...)
    if (is.null(roxygen_code) || !any(grepl("^#+'", roxygen_code))) {
        warning("Couldn't find roxygen comments in file ", file_name, 
                ".")
    }
    # remove lines that start with source
    roxygen_code <- roxygen_code[!str_starts(roxygen_code, "source\\(")]
    roxygen_code <- inject_notes(roxygen_code, file_name)
    code_file <- file.path(working_directory, "code.R")
    writeLines(roxygen_code, con = code_file)
    suppressMessages(utils::package.skeleton(code_files = code_file, 
                                             name = package_name, path = working_directory, force = TRUE))
    file.remove(code_file)
    file.remove(list.files(man_directory, full.names = TRUE))
    file.remove(file.path(package_directory, "NAMESPACE"))
    dev_null <- utils::capture.output(roxygen2::roxygenize(
        package.dir = package_directory))
    if (!is.null(dependencies)) {
        dependency_data <- data.frame(type = "Depends", package = dependencies, 
                                      version = "*")
        d <- desc::description$new(package_directory)
        d$set_deps(dependency_data)
        d$write()
    }
    return(package_directory)
}

document_scraper <- function(
    file_name, working_directory = NULL, dependencies = NULL,
    output_directory = "./documentation/scraper_documentation/", 
    sanitize_Rd = TRUE, runit = FALSE, check_package = FALSE, 
    check_as_cran = check_package, stop_on_check_not_passing = check_package, 
    clean = FALSE, debug = TRUE, ...){
    if (is.null(working_directory)){
        working_directory <- file.path(
            tempdir(), paste0("document_", basename(tempfile(pattern = ""))))
    }
    checkmate::assertFile(file_name, access = "r")
    checkmate::assertDirectory(output_directory, access = "r")
    checkmate::qassert(check_package, "B1")
    checkmate::qassert(working_directory, "S1")
    dir.create(working_directory, showWarnings = FALSE, recursive = TRUE)
    if (isTRUE(clean)) 
        on.exit({
            unlink(working_directory, recursive = TRUE)
            options(document_package_directory = NULL)
        })
    package_directory <- fake_package(
        file_name, working_directory = working_directory, 
        dependencies = dependencies, ...)
    
    file_name2 <- str_c(
        package_directory, "/R/",
        str_split_fixed(file_name, "/", n = 3)[,3]
    )
    
    status <- document:::write_the_docs(
        package_directory = package_directory, 
        file_name = file_name2, output_directory = output_directory, 
        dependencies = dependencies, sanitize_Rd = sanitize_Rd, 
        runit = runit)
    if (check_package) {
        check <- document:::check_package(
            package_directory = package_directory, 
            working_directory = working_directory,
            check_as_cran = check_as_cran, debug = debug,
            stop_on_check_not_passing = stop_on_check_not_passing)
        status[["check_result"]] <- check
    }
    return(status)
}

document_all_scrapers <- function(){
    sc_files <- list.files("production/scrapers", full.names = TRUE)
    sapply(sc_files, function(x){
        tryCatch(document_scraper(x), error=function(e) NULL)
    })
}

get_covid_perma_id <- function(api){
    envelop <- str_c("https://api.perma.cc/v1/user/?api_key=", api)
    out <- jsonlite::read_json(envelop, simplifyVector = TRUE)
    out$top_level_folders %>%
        filter(name == "COVID-19 Behind Bars") %>%
        pull(id) %>%
        as.character()
}

get_date_perma_id <- function(api, date_){
    # get parent id
    topid <- get_covid_perma_id(api)
    # get subfolders
    dirb <- str_c(
        "https://api.perma.cc/v1/folders/", topid, "/folders?api_key=", api)
    sub_dir <- jsonlite::read_json(dirb, simplifyVector = TRUE)
    
    if(!(date_ %in% sub_dir$objects$name)){
        setting <- curl::new_handle()
        curl::handle_setopt(setting, customrequest = "POST")
        curl::handle_setform(setting, name = as.character(date_))
        r <- curl::curl_fetch_memory(dirb, setting)
        reply <- jsonlite::parse_json(
            rawToChar(r$content), simplifyVector = TRUE)
    }
    
    new_sub_dir <- jsonlite::read_json(dirb, simplifyVector = TRUE)
    
    new_sub_dir$objects %>%
        filter(name == as.character(date_)) %>%
        pull(id) %>%
        as.character()
}

save_perma_cc <- function(arc_url, scraper_id, state_, date_, api = NULL){
    if(is.null(api)){
       api <- Sys.getenv("PERMACC_API_KEY")
    }

    # get today's date folder id
    fold_id <- get_date_perma_id(api, date_)
  
    # get the contents of this folder
    dirb <- str_c(
        "https://api.perma.cc/v1/folders/", fold_id, "/archives?api_key=", api)
    sub_dir <- jsonlite::read_json(dirb, simplifyVector = TRUE)
    arc_name <- str_c(scraper_id, "_", date_, "_", state_)
    
    # save the websites permalink if it doesnt already exist
    if(!(arc_name %in% sub_dir$objects$title)){
        api_url <- paste0("https://api.perma.cc/v1/archives/?api_key=", api)
        setting <- curl::new_handle()
        curl::handle_setopt(setting, customrequest = "POST")
        curl::handle_setform(
            setting, url = arc_url, folder = fold_id, title = arc_name)
        r <- curl::curl_fetch_memory(api_url, setting)
        reply <- jsonlite::parse_json(
            rawToChar(r$content), simplifyVector = TRUE)
    }
    
    new_sub_dir <- jsonlite::read_json(dirb, simplifyVector = TRUE)
    
    new_sub_dir$objects %>%
        filter(title == arc_name) %>%
        as_tibble() %>%
        pull(guid)
}

read_historical_data <- function(){
    ind_vars <- c("Date", "Name", "State")
    
    list.files("./results/extracted_data", full.names = TRUE) %>%
        lapply(function(x){
            df_ <- read_csv(x, col_types = cols())
            if("Date" %in% names(df_)){
              df_ <- df_ %>%
                mutate(Date = lubridate::as_date(Date))
            }
            df_
        }) %>%
        bind_rows() %>%
        select(-Resident.Deaths) %>%
        # remove values if they are missing a data name or state
        filter(!is.na(Date) & !is.na(Name) & State != "") %>%
        # order the names alphabetically
        select(!!sort(names(.))) %>%
        # put the indicator variables first
        select(!!ind_vars, !!(names(.)[!(names(.) %in% ind_vars)])) %>%
        filter(!is.na(id), !is.na(jurisdiction))
}

write_historical_data <- function(){
    read_historical_data() %>%
        write_csv("./results/summary_data/aggregated_data.csv")
}

translate_state <- function(x, reverse = FALSE){
    state_vec <- c(state.name, "DC", "Federal")
    names(state_vec) <- c(state.abb, "DC", "federal")
    
    if(reverse){
        state_vec <- c(state.abb, "DC", "Federal")
        names(state_vec) <- c(state.name, "DC", "federal")
    }

    state_vec[x]
}

vector_sum_na_rm <- function(...){
    d <- rbind(...)
    apply(rbind(...), 2, function(x){
        if(all(is.na(x))){
            NA
        }
        sum(x, na.rm = TRUE)
    })
}

sum_na_rm <- function(x){
    if(all(is.na(x))){
        return(NA)
    }
    sum(x, na.rm = TRUE)
}

#' A re-coding of the coalesce function to include warnings when multiple
#' values are given which are not NA and are different
#' 
#' @param ... vectors of equal length and type to coalesce
#' @return vector of coalesced values
#' 
#' @examples 
#' coalesce_with_warnings(1:3, 4:6)
#' coalesce_with_warnings(1:3, c(1:2, NA))

coalesce_with_warnings <- function(...){
    d <- cbind(...)
    
    sapply(1:nrow(d), function(i){
        x <- d[i,]
        if(all(is.na(x))){
            out <- NA
        }
        else{
            xbar <- unique(as.vector(na.omit(x)))
            if(length(xbar) != 1){
                warning(paste0(
                  "Row ", i, " has multiple values that do not match."))
            }
            # only grab the first one
            out <- xbar[1]
        }
    out
    })
}

load_latest_data <- function(
    all_dates = FALSE, coalesce = TRUE, fill = FALSE, debug = TRUE){
  
    scrapers <- str_remove(list.files("./production/scrapers"), ".R")
    
    facd_df <- "https://raw.githubusercontent.com/uclalawcovid19behindbars" %>%
        str_c("/facility_data/master/data_sheets/fac_data.csv") %>%
        read_csv(col_types = cols()) %>%
        select(
            ID = Count.ID, State, Name, Address, Zipcode, City, County, 
            Latitude, Longitude, County.FIPS, hifld_id) %>%
        mutate(Name = clean_fac_col_txt(str_to_upper(Name))) %>%
        unique()
    
    facdf_df <- "https://raw.githubusercontent.com/uclalawcovid19behindbars" %>%
      str_c("/facility_data/master/data_sheets/fac_data.csv") %>%
      read_csv(col_types = cols()) %>%
      filter(str_detect(Jurisdiction, "(?i)federal")) %>%
      select(
        ID = Count.ID, State, Name, Address, Zipcode, City, County, 
        Latitude, Longitude, County.FIPS, hifld_id, hifld_pop = POPULATION) %>%
      mutate(Name = clean_fac_col_txt(str_to_upper(Name))) %>%
      unique()
    
    facn_df <- "https://raw.githubusercontent.com/uclalawcovid19behindbars" %>%
        str_c("/facility_data/master/data_sheets/fac_spellings.csv") %>%
        read_csv(col_types = cols()) %>%
        select(
          ID = Count.ID, State, Name = facility_name_clean,
          Facility = facility_name_raw) %>%
        mutate(Name = clean_fac_col_txt(str_to_upper(Name))) %>%
        mutate(Facility = clean_fac_col_txt(str_to_upper(Facility))) %>%
        unique()
    
    if(!all_dates){
        dat_df <- bind_rows(lapply(scrapers, function(i){
            sub_files <- list.files(
                "results/extracted_data", full.names = TRUE,
                pattern = str_c("\\d+-\\d+-\\d+_", i, ".csv"))
            if(length(sub_files) == 0){
                return(tibble())
            }
            date_vec <- as.Date(str_extract(sub_files, "\\d+-\\d+-\\d+"))
            f_ <- sub_files[which(date_vec == max(date_vec))]
            
            sub_df <- read_csv(f_, col_types = cols())
            if("Residents.Confirmed" %in% names(sub_df)){
                test_class <- class(sub_df$Residents.Confirmed)
                if(test_class != "numeric"){
                    print(f_)
                    print(test_class)
                }
            }
            return(sub_df)
        }))
    }
    
    else{
        dat_df <- read_historical_data() %>%
            filter(!is.na(jurisdiction), !is.na(id))
    }
    
    raw_df <- dat_df %>%
        select(-starts_with("Resident.Deaths")) %>%
        rename(Facility = Name) %>%
        mutate(Facility = clean_fac_col_txt(str_to_upper(Facility))) %>%
        mutate(State = translate_state(State))
    
    if(debug){
        message(str_c("Base data frame contains ", nrow(raw_df), " rows."))
    }
    
    nonfederal_unname <- raw_df %>%
        filter(State != "Federal") %>%
        left_join(facn_df, by = c("Facility", "State"))
        
    nonfederal <- nonfederal_unname %>%
        mutate(Name = ifelse(is.na(Name), Facility, Name)) %>%
        select(-Facility) %>%
        left_join(facd_df,  by = c("Name", "State", "ID"))
    
    federal_unname <- facn_df %>%
        filter(State == "Federal") %>%
        group_by(Facility) %>%
        mutate(tmp = 1:n()) %>%
        filter(tmp == 1) %>%
        select(-tmp) %>%
        ungroup() %>%
        right_join(
            raw_df %>%
                filter(jurisdiction == "federal") %>%
                select(-State) %>%
                group_by(Date, Facility, jurisdiction, id, source) %>%
                summarize_all(sum_na_rm) %>%
                ungroup(),
            by = "Facility")
    
    if(debug){
        bind_rows(nonfederal_unname, federal_unname) %>%
            filter(is.na(Name)) %>%
            select(State, jurisdiction, raw_name = Facility) %>%
            write_csv("./prototyping/unmatched_names.csv")
    }

    federal <- federal_unname %>%
        mutate(Name = ifelse(is.na(Name), Facility, Name)) %>%
        select(-Facility, -State) %>%
        left_join(facdf_df,  by = c("Name", "ID")) %>%
        mutate(State = ifelse(is.na(State), "Not Available", State))
    
    full_df <- bind_rows(federal, nonfederal)
    
    if(debug){
        message(str_c("Named data frame contains ", nrow(full_df), " rows."))
    }
    
    out_df <- full_df %>%
        mutate(Residents.Released = NA, Notes = NA)
    
    if(coalesce){
        out_df <- out_df %>%
            select(-id) %>%
            group_by_coalesce(
              Date, Name, State, jurisdiction,
              .ignore = "source", .method = "sum")
        
        if(debug){
          message(str_c(
              "Coalesced data frame contains ", nrow(out_df), " rows."))
        }
    }
    
    pop_df <- out_df  %>%
        left_join(
            read_pop_data(),
            by = c("Name", "State")
        )
    
    if(debug){
        message(str_c("Pop data frame contains ", nrow(pop_df), " rows."))
    }
    
    pop_df %>%
        mutate(Residents.Population = Population) %>%
        # fill in HIFLD pop where no alternative exists
        mutate(Residents.Population = ifelse(
            is.na(Residents.Population), hifld_pop, Residents.Population)) %>%
        mutate(Residents.Confirmed = ifelse(
            is.na(Residents.Confirmed) & fill,
            vector_sum_na_rm(
               Residents.Active, Residents.Deaths, Residents.Recovered),
            Residents.Confirmed
        )) %>%
        # mutate(Residents.Tadmin = ifelse(
        #     is.na(Residents.Tadmin),
        #     Residents.Tested,
        #     Residents.Tadmin
        # )) %>%
        # Select the order for names corresponding to Public facing google sheet
        select(
            ID, jurisdiction, State, Name, Date, source,
            Residents.Confirmed, Staff.Confirmed,
            Residents.Deaths, Staff.Deaths, Residents.Recovered,
            Staff.Recovered, Residents.Tadmin, Staff.Tested, Residents.Negative,
            Staff.Negative, Residents.Pending, Staff.Pending,
            Residents.Quarantine, Staff.Quarantine, Residents.Active, 
            Residents.Population, Address, Zipcode, City, County, Latitude,
            Longitude, County.FIPS, hifld_id, Notes) %>%
        arrange(State, Name) %>%
        mutate(Date = str_c(
            month.name[lubridate::month(Date)], " ", lubridate::day(Date),
            ", ", lubridate::year(Date)))
}

write_latest_data <- function(coalesce = TRUE, fill = FALSE){
    
    out_df <- load_latest_data(coalesce = coalesce, fill = fill)
    
    out_df %>%
        select(
            Residents.Confirmed, Residents.Deaths, Residents.Recovered,
            Residents.Tadmin, Residents.Negative, Residents.Pending,
            Residents.Quarantine, Residents.Population, Staff.Confirmed,
            Staff.Deaths, Staff.Recovered, Staff.Tested, Staff.Negative,
            Staff.Pending) %>%
          summarize_all(sum_na_rm) %>%
          pivot_longer(
              Residents.Confirmed:Staff.Pending, names_to = "Variable",
              values_to = "Count") %>%
      print()
  
    write_csv(
        out_df,
        str_c(
            "./data/Adult Facility Counts/",
            "adult_facility_covid_counts_today_latest.csv"), 
      na="")
}

get_latest_manual <- function(state){
    manual_files <- list.files(
        "./results/manual_data", full.names = TRUE,
        pattern = str_c(state, "_\\d\\d\\d\\d\\d\\d", ".xlsx"))
    
    dates <- lubridate::mdy(str_extract(manual_files, "\\d\\d\\d\\d\\d\\d"))
    
    readxl::read_excel(manual_files[which.max(dates)])
}

coalesce_by_column <- function(df) {
    return(coalesce(df[1], df[2]))
}

