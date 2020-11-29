source("./R/generic_scraper.R")
source("./R/utilities.R")

maryland_pull <- function(x){
    md_img <- get_src_by_attr(
        x, "img", attr = "src", attr_regex = "(?i)pubic") %>%
        str_remove_all("-\\d{3,4}x\\d*")

    magick::image_read(md_img)
}

maryland_restruct <- function(x){
    ExtractTable(x)
}

maryland_extract <- function(x){
    # remove the title
    df_ <- x[[1]]
    
    if(!str_detect(df_[1,1], "(?i)region")){
        # sometimes need to remove title
        df_ <- df_[2:nrow(df_),]
    }
    
    col_name_mat <- matrix(c(
        "Region", "0", "Region",
        "Facility", "1", "Name",
        "Staff Tests", "2", "Staff.Tested",	
        "Staff Positive", "3", "Staff.Confirmed",	
        "Staff Recovered", "4", "Staff.Recovered",
        "Staff Deaths", "5", "Staff.Deaths",
        "Inmates Tested", "6", "Residents.Tested",
        "Inmates Positive", "7", "Residents.Confirmed",
        "Inmates Recovered", "8", "Residents.Recovered",
        "Inmate Deaths", "9",  "Residents.Deaths"
        ), ncol = 3, nrow = 10, byrow = TRUE)
    
    colnames(col_name_mat) <- c("check", "raw", "clean")
    col_name_df <- as_tibble(col_name_mat)
    
    check_names_extractable(df_, col_name_df)
    
    rename_extractable(df_, col_name_df) %>%
        filter(Name != "Facility" & Name != "") %>%
        select(-Region) %>%
        mutate_at(vars(-Name), ~ str_replace(., pattern = "^-$", "0")) %>%
        mutate_at(vars(-Name), function(x) ifelse(x == "", "0", x)) %>%
        clean_scraped_df() %>%
        as_tibble() %>%
        mutate(Name = case_when(
            Name=="BCBIC" ~ "BCBIC-Baltimore Central Booking & Intake Center",
            Name=="BCCC" ~ "BCCC-Baltimore City CC",
            Name=="CDF" ~ "CDF-Chesapeake Detention Facility",
            Name=="MRDCC" ~ "MRDCC-Maryland Reception Diagnostics and Classiciation Center",
            Name=="MTC" ~ "MTC-Metropolitan Transition Center",
            Name=="YDC" ~ "YDC-Youth Detention Center",
            Name=="ECI" ~ "ECI-Eastern CI",
            Name=="EPRU" ~ "EPRU-Eastern Pre-Release Unit",
            Name=="MCI-H" ~ "MCI-H-Maryland CI - Hagerstown",
            Name=="MCTC" ~ "MCTC-Maryland Correctional Training Center",
            Name=="RCI" ~ "RCI-Roxbury CI",
            Name=="CMCF" ~ "CMCF-Central Maryland CF",
            Name=="DRCF" ~ "DRCF-Dorsey Run CF",
            Name=="JCI" ~ "JCI-Jessup CI",
            Name=="MCI-J" ~ "MCI-J-Maryland CI - Jessup",
            Name=="MCI-W" ~ "MCI-W-Maryland CI for Women",
            Name=="PATX" ~ "PATX-Patuxent Institution",
            Name=="SMPRU" ~ "SMPRU-Southern Maryland Pre-Release Unit",
            Name=="NBCI" ~ "NBCI-North Branch CI",
            Name=="WCI" ~ "WCI-Western CI",
            Name=="Non-residential location" ~ "Non-residential Location",
            TRUE ~ Name
        ))
}

#' Scraper class for general Maryland COVID data
#' 
#' @name maryland_scraper
#' @description Data from MD is pulled from a image hosted in the DOC website
#' which is run the OCR. The data posted has been fairly consistent.
#' \describe{
#'   \item{Facility}{The facility name}
#'   \item{Region}{Greater region of facility}
#'   \item{Staff Tests}{Tests administered to staff not sure if test administered or individuals tested}
#'   \item{Staff Positive}{Number of confirmed staff}
#'   \item{Staff Recovered}{Number of recovered staff}
#'   \item{Staff Deaths}{Number of staff deaths}
#'   \item{Inmates Tested}{Residents tested not sure if test administered or individuals tested}
#'   \item{Inmates Positive}{Number of confirmed residents}
#'   \item{Inmates Recovered}{Number of recovered residents}
#'   \item{Inmates Deaths}{Number of resident deaths}
#' }

maryland_scraper <- R6Class(
    "maryland_scraper",
    inherit = generic_scraper,
    public = list(
        log = NULL,
        initialize = function(
            log,
            url = "https://news.maryland.gov/dpscs/covid-19/",
            id = "maryland",
            type = "img",
            state = "MD",
            jurisdiction = "state",
            # pull the JSON data directly from the API
            pull_func = maryland_pull,
            # restructuring the data means pulling out the data portion of the json
            restruct_func = maryland_restruct,
            # Rename the columns to appropriate database names
            extract_func = maryland_extract){
            super$initialize(
                url = url, id = id, pull_func = pull_func, type = type,
                restruct_func = restruct_func, extract_func = extract_func,
                log = log, state = state, jurisdiction = jurisdiction)
        }
    )
)

if(sys.nframe() == 0){
    maryland <- maryland_scraper$new(log=TRUE)
    maryland$raw_data
    maryland$pull_raw()
    maryland$raw_data
    maryland$save_raw()
    maryland$restruct_raw()
    maryland$restruct_data
    maryland$extract_from_raw()
    maryland$extract_data
    maryland$validate_extract()
    maryland$save_extract()
}

