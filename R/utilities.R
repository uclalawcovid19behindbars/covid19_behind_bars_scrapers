library(tidyverse)
library(R6)
library(tryCatchLog)
library(behindbarstools)
library(futile.logger)
source("R/generic_scraper.R")

basic_check <- function(true_names, expected_names, detect = FALSE){
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
                
                # Exact match 
                if (!detect){
                    if(cexp != ctrue){
                        warning(str_c(
                            "Extracted column ", i, " does not match expected ",
                            "name. Expected: ", cexp, " Received: ", ctrue))
                    }
                }
                
                # Case-insensitive string contains match 
                if (detect){
                    if(!str_detect(ctrue, paste0("(?i)", cexp))){
                          warning(str_c(
                            "Extracted column ", i, " does not contain expected string. ",
                            "Expected: ", cexp, " Received: ", ctrue))
                    }
                }
            }
        }
    }
}

check_names <- function(DF, expected_names, detect = FALSE){
    true_names <- names(DF)
    basic_check(true_names, expected_names, detect)
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


error_on_date <- function(date, expected_date, days = 30){
    days_late <- abs(as.numeric(date - expected_date))
    fail <- days_late > days

    if(fail){
        warning(str_c("Date is more than ", days, " different from expected"))
    }

    return(days_late)
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
check_extractable_credits <- function(api_key=NULL, verbose=FALSE) {
    if(is.null(api_key)){
        api_key <- Sys.getenv("EXTRACTABLE_API_KEY")
    }
    
    validate_endpoint = 'https://validator.extracttable.com'
    content <- httr::content(httr::GET(
        url = validate_endpoint, httr::add_headers(`x-api-key` = api_key)), 
        as = 'parsed', type = 'application/json')
    
    credits <- content[1]$usage$credits
    used <- content[1]$usage$used
    remaining <- credits - used 
    
    # By default: only throw a warning if remaining credits fall below 200 
    if (remaining < 200) {warning(paste("Running low on ExtractTable credits. Remaining:", remaining))}
  
    if (verbose) {content}
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
        c("-", "N/A", "na", "n/a", "NA", "", " +", "n/a*",
          NA, "T", "[", "]", "o", "O", "Unknown", "*", "pending", 
          "PENDING DOH RESULTS", "PENDING DOH RESULT$", "S", "", "PENDING")
    
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

# Borrowed from here! https://stackoverflow.com/a/18333845 
# ex: word_to_numeric("six") -> 6 
word_to_numeric <- function(word){
    wsplit <- strsplit(tolower(word)," ")[[1]]
    one_digits <- list(zero=0, one=1, two=2, three=3, four=4, five=5,
                       six=6, seven=7, eight=8, nine=9)
    teens <- list(eleven=11, twelve=12, thirteen=13, fourteen=14, fifteen=15,
                  sixteen=16, seventeen=17, eighteen=18, nineteen=19)
    ten_digits <- list(ten=10, twenty=20, thirty=30, forty=40, fifty=50,
                       sixty=60, seventy=70, eighty=80, ninety=90)
    doubles <- c(teens,ten_digits)
    out <- 0
    i <- 1
    while(i <= length(wsplit)){
        j <- 1
        if(i==1 && wsplit[i]=="hundred")
            temp <- 100
        else if(i==1 && wsplit[i]=="thousand")
            temp <- 1000
        else if(wsplit[i] %in% names(one_digits))
            temp <- as.numeric(one_digits[wsplit[i]])
        else if(wsplit[i] %in% names(teens))
            temp <- as.numeric(teens[wsplit[i]])
        else if(wsplit[i] %in% names(ten_digits))
            temp <- (as.numeric(ten_digits[wsplit[i]]))
        if(i < length(wsplit) && wsplit[i+1]=="hundred"){
            if(i>1 && wsplit[i-1] %in% c("hundred","thousand"))
                out <- out + 100*temp
            else
                out <- 100*(out + temp)
            j <- 2
          }
        else if(i < length(wsplit) && wsplit[i+1]=="thousand"){
            if(i>1 && wsplit[i-1] %in% c("hundred","thousand"))
                out <- out + 1000*temp
            else
                out <- 1000*(out + temp)
            j <- 2
        }
        else if(i < length(wsplit) && wsplit[i+1] %in% names(doubles)){
            temp <- temp*100
            out <- out + temp
        }
        else{
            out <- out + temp
        }
        i <- i + j
      }
      return(out)
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
    sc_vec <- list.files("production/scrapers", full.names = FALSE, pattern = ".R") %>%
        str_replace(".R", "_scraper")

    names(sc_vec) <- str_remove(sc_vec, "_scraper")

    sc_vec
}

get_last_run <- function(file_name){
    scraper_name <- str_remove(str_split_fixed(file_name, "/", n = 3)[,3], ".R")
    
    out_files <- list_remote_data("extracted_data", scraper_name)

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

    if(!dir.exists(output_directory)){
        dir.create(output_directory)
    }
    if(is.null(working_directory)){
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
        cat("Documenting scraper file", x, "\n")
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

COVID_SUFFIXES <- c(
    ".Confirmed", ".Deaths", ".Tadmin", ".Tested", ".Active",
    ".Initiated", ".Initiated.Pct", ".Completed", ".Completed.Pct", ".Vadmin"
)

rowAny <- function(x) rowSums(x) > 0

write_national_agg_data <- function(write_historical = TRUE){
    # The order and selection of cols corresponds to values in the Google sheet
    # https://docs.google.com/spreadsheets/d/1MCiyyaz1PtQX_AZ5sMRUOIOttbi8nqIvXCcPt4j4RIo
    sel_vars <- c(
        "Residents.Confirmed", "Staff.Confirmed",
        "Residents.Deaths", "Staff.Deaths",
        "Residents.Tadmin",
        "Residents.Initiated", "Staff.Initiated"
    )
  
    latest_national <- calc_aggregate_counts(all_dates = FALSE)
    latest_national %>%
        filter(Measure %in% sel_vars) %>%
        mutate(Measure = factor(Measure, sel_vars)) %>%
        arrange(Measure) %>%
        mutate(Missing = gsub("((?:[^,]+, ){4}[^,]+),", "\\1\n", Missing)) %>%
        write_csv("./data/latest-data/latest_national_counts.csv", na = "")
  
    if (write_historical){
        historical_national <- calc_aggregate_counts(all_dates = TRUE)
        historical_national %>%
            filter(Measure %in% sel_vars) %>%
            mutate(Measure = factor(Measure, sel_vars)) %>%
            arrange(Measure, Date) %>%
            mutate(Missing = gsub("((?:[^,]+, ){4}[^,]+),", "\\1\n", Missing)) %>%
            write_csv("./data/historical-data/historical_national_counts.csv", na = "")
    }
}

write_state_agg_data <- function(write_historical = TRUE){
    latest_state <- calc_aggregate_counts(state = TRUE, all_dates = FALSE) 
    aggregate_pop <- read_aggregate_pop_data() 

    latest_state_tmp <- latest_state %>%
        filter(!is.na(Val)) %>%
        select(State, Measure, Val) %>%
        pivot_wider(names_from = "Measure", values_from = "Val") %>%
        arrange(State) %>%
        select(State, ends_with(COVID_SUFFIXES)) %>% 
        select(-Staff.Tested) %>% 
        left_join(aggregate_pop, by = "State") 
    # Create vaccine rate variables:
    # coalesce vaccine rates (scraped) and vaccine rates (calculated)
    latest_state_tmp %>%
      ## add vax rate cols if none reported for a given day
      mutate(Residents.Initiated.Pct = if("Residents.Initiated.Pct" %in% names(latest_state_tmp)) 
        Residents.Initiated.Pct else NA_real_,
        Residents.Completed.Pct = if("Residents.Completed.Pct" %in% names(latest_state_tmp)) 
          Residents.Completed.Pct else NA_real_,
        Staff.Initiated.Pct = if("Staff.Initiated.Pct" %in% names(latest_state_tmp)) 
          Staff.Initiated.Pct else NA_real_,
             ) %>% 
      ## prioritize reported rates
      mutate(Residents.Initiated.Pct = coalesce(Residents.Initiated.Pct,
                                                (Residents.Initiated / Residents.Population)),
             Residents.Completed.Pct = coalesce(Residents.Completed.Pct,
                                                (Residents.Completed / Residents.Population)),
             Staff.Initiated.Pct = coalesce(Staff.Initiated.Pct,
                                            (Staff.Initiated / Staff.Population))
        ) %>%
        select(-Date) %>% 
        write_csv("./data/latest-data/latest_state_counts.csv", na = "")
    
    if (write_historical){
        historical_state <- calc_aggregate_counts(state = TRUE, all_dates = TRUE)
        historical_state %>%
            filter(!is.na(Val)) %>%
            select(Date, State, Measure, Val) %>%
            pivot_wider(names_from = "Measure", values_from = "Val") %>%
            arrange(State, Date) %>%
            select(Date, State, ends_with(COVID_SUFFIXES)) %>% 
            select(-Staff.Tested) %>% 
            write_csv("./data/historical-data/historical_state_counts.csv", na = "")
    }
}

write_facility_data <- function(write_historical = TRUE){
    fac_sel_vars <- c(
        "Facility.ID", "Jurisdiction", "State", "Name", "Date", "source",
        "Residents.Confirmed", "Staff.Confirmed",
        "Residents.Deaths", "Staff.Deaths", 
        "Residents.Tadmin", "Residents.Tested", 
        "Residents.Active", "Staff.Active",
        "Population.Feb20", "Residents.Population", 
        "Residents.Initiated", "Staff.Initiated", 
        "Residents.Initiated.Pct", "Staff.Initiated.Pct",
        "Residents.Completed", "Staff.Completed", 
        "Residents.Completed.Pct",
        "Residents.Vadmin", "Staff.Vadmin", "Web.Group", 
        "Address", "Zipcode", "City", "County", "Latitude",
        "Longitude", "County.FIPS", "ICE.Field.Office"
    )
  
    lastest_fac <- read_scrape_data(all_dates = FALSE)
    lastest_fac %>%
        filter(rowAny(across(ends_with(COVID_SUFFIXES), ~ !is.na(.x)))) %>%
        filter(!is.na(Facility.ID)) %>% 
        filter(!(stringr::str_detect(Name, "(?i)state|county") & stringr::str_detect(Name, "(?i)wide"))) %>%
        select(fac_sel_vars) %>% 
        filter(Web.Group != "Psychiatric") %>% 
        write_csv("./data/latest-data/latest_facility_counts.csv", na = "")
  
    if (write_historical){
        historical_fac <- read_scrape_data(all_dates = TRUE)
        historical_fac %>%
            filter(rowAny(across(ends_with(COVID_SUFFIXES), ~ !is.na(.x)))) %>%
            filter(!is.na(Facility.ID)) %>% 
            filter(!(stringr::str_detect(Name, "(?i)state|county") & stringr::str_detect(Name, "(?i)wide"))) %>%
            select(fac_sel_vars) %>% 
            filter(Web.Group != "Psychiatric") %>% 
            write_csv("./data/historical-data/historical_facility_counts.csv", na = "") 
    }
}

write_state_jurisdiction_data <- function(write_historical = TRUE){
    alt_sel_vars <- c ("State", "Web.Group", "Measure", "Val", "Rate", "Date")
    
    latest_alt <- alt_aggregate_counts(all_dates = FALSE)
    latest_alt %>% 
        filter(str_detect(Measure, paste(COVID_SUFFIXES, collapse = "|"))) %>% 
        select(alt_sel_vars) %>% 
        filter(!Web.Group %in% c("County", "Psychiatric")) %>% 
        write_csv("./data/latest-data/latest_state_jurisdiction_counts.csv", na = "")
      
    if (write_historical){
        historical_alt <- alt_aggregate_counts(all_dates = TRUE)
        historical_alt %>% 
            filter(str_detect(Measure, paste(COVID_SUFFIXES, collapse = "|"))) %>% 
            select(alt_sel_vars) %>% 
            filter(!Web.Group %in% c("County", "Psychiatric")) %>% 
            write_csv("./data/historical-data/historical_state_jurisdiction_counts.csv", na = "")
    }
}

write_latest_data <- function(write_historical = TRUE){
    write_facility_data(write_historical = write_historical)
    write_state_agg_data(write_historical = write_historical)
    write_national_agg_data(write_historical = write_historical)
    write_state_jurisdiction_data(write_historical = write_historical)
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

sync_remote_files <- function(raw = FALSE){
    system(str_c(
        "rsync --perms --chmod=u+rwx -rtvu --progress results/extracted_data/ ",
        "ucla:/srv/shiny-server/scraper_data/extracted_data/"))

    system(str_c(
        "rsync --perms --chmod=u+rwx -rtvu --progress results/log_files/ ",
        "ucla:/srv/shiny-server/scraper_data/log_files/"))

    system(str_c(
      "rsync --perms --chmod=u+rwx -rtvu --progress results/last_update/ ",
      "ucla:/srv/shiny-server/scraper_data/last_update/"))

    if(raw){
        system(str_c(
            "rsync --perms --chmod=u+rwx -rtvu --progress results/raw_files/ ",
            "ucla:/srv/shiny-server/scraper_data/raw_files/"))
    }
  
}

sync_diagnostic_files <- function() {
    system(str_c(
        "rsync --perms --chmod=u+rwx -rtvu --progress results/diagnostic_files/ ",
        "ucla:/srv/shiny-server/scraper_data/diagnostic_files/"))
}

generate_diagnostics <- function() {
    if (!dir.exists("./results/diagnostic_files/")) {
        dir.create("./results/diagnostic_files/")
    }
  
    date <- Sys.Date()
    rmarkdown::render("./reports/post_diagnostics.Rmd", 
                      output_file = paste0("../results/diagnostic_files/", date, "_post_diagnostics.html"), 
                      quiet = TRUE)
}

stop_defunct_scraper <- function(url){
    stop(paste0(
        "This scraper is not currently functional. Please occasionally check ",
        "the following URL to see if data may now be scraped: ", url))
}

hist_config_update <- function(df){
    tf <- tempfile(fileext = ".csv")
    old_records <- read_csv(
        "http://104.131.72.50:3838/scraper_data/summary_data/hist_records.csv",
        col_types = c(Scraper = "c", Date = "D", File = "c"))
    old_records %>%
        bind_rows(df) %>%
        unique() %>%
        write_csv(tf)
    system(str_c(
      "rsync --perms --chmod=u+rwx -rtvu --progress ", tf,
      " ucla:/srv/shiny-server/scraper_data/summary_data/hist_records.csv"))
}

summarize_remote_data <- function(){
  system("ssh ucla '/srv/shiny-server/scraper_utilities/summarize.R'")
}

## calculate latest facility-level and state increases

#' @param scrape_df facility-level data to use
#' @param outbreaks_sheet_loc optional, google sheets ID to use if writing to google sheets
#' @param metric character string of the metric to calc 
#' @param delta_days integer, number of days to calculate increase over  
#' @param num_fac integer, number of facilities to plot 
#' @param overwrite_data T/F, write output data to google sheet specified in outbreaks_sheet_loc
#' 
#' @return dataframe with highest state and facility-level outbreaks

## code borrowed from: https://github.com/uclalawcovid19behindbars/behindbarstools/blob/master/R/plot_recent_fac_increases.R
track_recent_covid_increases <- function(
  scrape_df, 
  outbreaks_sheet_loc,
  metric = "Residents.Confirmed", 
  delta_days, 
  num_fac = 5,
  overwrite_data = TRUE) {
  ## define inputs for data filtering
  latest_scrape_date <-  max(scrape_df$Date)
  delta_start_date <- latest_scrape_date - lubridate::days(delta_days)

  ## get state-wide data 
  latest_state <- calc_aggregate_counts(state = TRUE, all_dates = FALSE) %>%
    filter(!is.na(Val)) %>%
    select(State, Measure, Val) %>%
    pivot_wider(names_from = "Measure", values_from = "Val") %>%
    arrange(State) %>%
    select(State, ends_with(c(".Confirmed", ".Deaths", ".Active"))) %>%
    mutate(Date = latest_scrape_date)
  historical_state <- read_csv("https://media.githubusercontent.com/media/uclalawcovid19behindbars/data/master/historical-data/historical_state_counts.csv")  
  n_days_closest_deltastart <- as.integer(min(abs(delta_start_date - historical_state$Date)))
  lookaround_delta_start_date <- c(delta_start_date, 
                                   (delta_start_date + lubridate::days(n_days_closest_deltastart)),
                                   (delta_start_date - lubridate::days(n_days_closest_deltastart)))
  historical_state <- historical_state %>%
    filter(Date %in% lookaround_delta_start_date) %>%
    select(State, ends_with(c(".Confirmed", ".Deaths", ".Active")), Date)
  state_df <- bind_rows(latest_state, historical_state) %>%
    arrange(State, Date)
  
  fac_data <- scrape_df %>%
    filter(!(stringr::str_detect(Name, "(?i)state") & stringr::str_detect(Name, "(?i)wide"))) %>%
    filter(Date >= delta_start_date) %>%
    group_by(Name, State) %>%
    mutate(start_val = first(!!sym(metric)),
           last_val = last(!!sym(metric)),
           raw_change = last_val - start_val,
           pct_increase = (raw_change / start_val)*100) %>%
    distinct(Facility.ID, Name, State, start_val, last_val, raw_change, pct_increase) %>% 
    filter(raw_change > 0) 
  if(!str_detect(metric, ".Deaths")) {
    fac_data <- fac_data %>%
      filter(start_val > 0)
  }
  keep_facs_pct_increase <- fac_data %>%
    arrange(desc(pct_increase), Name) %>% 
    mutate(metric_arrange = "pct_increase") %>% 
    head(num_fac) 
  keep_facs_raw_increase <- fac_data %>%
    arrange(desc(raw_change), Name) %>% 
    mutate(metric_arrange = "raw_increase") %>% 
    head(num_fac) 
  ## bind dfs together to get both % increase and raw number jump
  keep_facs <- keep_facs_pct_increase %>%
    bind_rows(keep_facs_raw_increase) %>%
    distinct(Name, State, .keep_all = TRUE) %>%
    mutate(Name = str_to_title(Name))
  # ## do the same for state
  state_data <- state_df %>%
    group_by(State) %>%
    mutate(start_val = first(!!sym(metric)),
           last_val = last(!!sym(metric)),
           raw_change = last_val - start_val,
           pct_increase = (raw_change / start_val)*100) %>%
    distinct(State, start_val, last_val, raw_change, pct_increase) %>%
    filter(raw_change > 0)
  if(!str_detect(metric, ".Deaths")) {
    state_data <- state_data %>%
      filter(start_val > 0)
  }
  keep_states_pct_increase <- state_data %>%
    arrange(desc(pct_increase), State) %>%
    mutate(metric_arrange = "pct_increase") %>%
    head(num_fac)
  keep_states_raw_increase <- state_data %>%
    arrange(desc(raw_change), State) %>%
    mutate(metric_arrange = "raw_increase") %>%
    head(num_fac)
  keep_states <- keep_states_pct_increase %>%
    bind_rows(keep_states_raw_increase) %>%
    distinct(State, .keep_all = TRUE) %>%
    mutate(Facility.ID = NA,
           Name = "Statewide") %>%
    relocate(Facility.ID, State, Name)
  
  out <- bind_rows(keep_facs, keep_states) %>%
    mutate(pct_increase = na_if(pct_increase, Inf),
           pct_increase = na_if(pct_increase, -Inf))
  if(overwrite_data){
    range_write(
      data = out, 
      ss = outbreaks_sheet_loc, 
      sheet = glue("{metric}"), 
      reformat = FALSE)
  }
  return(out)
} 

calc_national_agg_diff <- function(delta_days) {
  ## using local data here because this function now happens after write_latest_data gets called
  ## means that someone MUST have up-to-date local data if running this without having run the scrapers
  historical_ntl_agg <- read_csv("./data/historical-data/historical_national_counts.csv")
  latest_scrape_date <-  max(historical_ntl_agg$Date)
  delta_start_date <- latest_scrape_date - lubridate::days(delta_days)
  n_days_closest_deltastart <- as.integer(min(abs(delta_start_date - historical_ntl_agg$Date)))
  lookaround_delta_start_date <- c((delta_start_date + lubridate::days(n_days_closest_deltastart)),
                                   (delta_start_date - lubridate::days(n_days_closest_deltastart)))
  
  ntl_comparison <- historical_ntl_agg %>% 
    filter(Date == latest_scrape_date |
             Date %in% lookaround_delta_start_date) %>%
    arrange(Date) %>%
    select(Date, Measure, Count) %>%
    pivot_wider(names_from = Date, values_from = Count, names_glue = "Count_{Date}") %>%
    mutate(Difference = .[[3]] - .[[2]],
           Pct_Increase = (Difference / .[[2]] )*100) %>% 
    select(-starts_with("Missing"),
           -starts_with("Reporting"))
  return(ntl_comparison)
}

update_fac_outbreaks_sheet <- function(
  outbreaks_sheet_loc,
  delta_days
  ) {
  scrape_df <- behindbarstools::read_scrape_data(all_dates = TRUE)
  metrics <- c("Residents.Confirmed", "Residents.Active", 
               "Residents.Deaths", "Staff.Confirmed", "Staff.Deaths")
  out <- metrics %>%
    map(~ track_recent_covid_increases(metric = .x, 
                                       scrape_df = scrape_df,
                                       outbreaks_sheet_loc = outbreaks_sheet_loc,
                                       delta_days = delta_days))
  ## create some metadata 
  all_states <- purrr::transpose(out) %>% 
    .[["State"]] %>%
    unlist() %>%
    as_tibble() %>%
    group_by(value) %>%
    summarise(n = n()) %>%
    arrange(-n) %>%
    filter(n > 1) %>%
    rename(State = value,
           times_flagged = n)
  
  ## write the metadata to google sheet
  range_write(
    data = all_states, 
    ss = outbreaks_sheet_loc, 
    sheet = "Top states", 
    reformat = FALSE)
  
  ## run function that calculates diff between latest ntl aggregate and one that was delta_days ago
  ntl_comparison <- calc_national_agg_diff(delta_days = delta_days)
  
  ## write the ntl agg comparison to google sheet
  range_write(
    data = ntl_comparison, 
    ss = outbreaks_sheet_loc, 
    sheet = "National aggregates", 
    reformat = FALSE)
}
