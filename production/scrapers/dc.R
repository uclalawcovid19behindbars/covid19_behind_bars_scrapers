source("./R/generic_scraper.R")

dc_check_date <- function(x, date = Sys.Date()){
    stringr::str_c(
        "https://em.dcgis.dc.gov/dcgis/rest/services/COVID_19/",
        "OpenData_COVID19/FeatureServer/10/query?where=1%3D1&",
        "objectIds=&time=&geometry=&geometryType=esriGeometryEnvelope&",
        "inSR=&spatialRel=esriSpatialRelIntersects&distance=&",
        "units=esriSRUnit_Foot&relationParam=&outFields=*&",
        "returnGeometry=true&maxAllowableOffset=&geometryPrecision=&",
        "outSR=&having=&gdbVersion=&historicMoment=&",
        "returnDistinctValues=false&returnIdsOnly=false&",
        "returnCountOnly=false&returnExtentOnly=false&orderByFields=&",
        "groupByFieldsForStatistics=&outStatistics=&returnZ=false&",
        "returnM=false&multipatchOption=xyFootprint&",
        "returnTrueCurves=false&returnCentroid=false&sqlFormat=none&",
        "resultType=&featureEncoding=esriDefault&f=pjson") %>%
        jsonlite::read_json(simplifyVector = TRUE) %>%
        {.$features$attributes} %>%
        filter(!is.na(TOTAL_POSITIVE_PSDP)) %>%
        pull(DATE_REPORTED) %>%
        {lubridate::as_datetime(./1000)} %>%
        lubridate::as_date() %>%
        max(na.rm=TRUE) %>% 
        error_on_date(date)
}

dc_pull <- function(x){
    stringr::str_c(
        "https://em.dcgis.dc.gov/dcgis/rest/services/COVID_19/",
        "OpenData_COVID19/FeatureServer/10/query?where=1%3D1&",
        "objectIds=&time=&geometry=&geometryType=esriGeometryEnvelope&",
        "inSR=&spatialRel=esriSpatialRelIntersects&distance=&",
        "units=esriSRUnit_Foot&relationParam=&outFields=*&",
        "returnGeometry=true&maxAllowableOffset=&geometryPrecision=&",
        "outSR=&having=&gdbVersion=&historicMoment=&",
        "returnDistinctValues=false&returnIdsOnly=false&",
        "returnCountOnly=false&returnExtentOnly=false&orderByFields=&",
        "groupByFieldsForStatistics=&outStatistics=&returnZ=false&",
        "returnM=false&multipatchOption=xyFootprint&",
        "returnTrueCurves=false&returnCentroid=false&sqlFormat=none&",
        "resultType=&featureEncoding=esriDefault&f=pjson") %>%
        jsonlite::read_json(simplifyVector = TRUE)
}

dc_restruct <- function(x){
    as_tibble(x$features$attributes)
}

dc_extract <- function(x){
    x %>%
        # the format is in unix time with extra precision not readable
        # by default in the R package lubridate
        mutate(Date = lubridate::as_datetime(DATE_REPORTED/1000)) %>%
        mutate(Date = lubridate::as_date(Date)) %>%
        filter(!is.na(TOTAL_POSITIVE_PSDP)) %>%
        filter(Date == max(Date)) %>%
        mutate(Staff.Confirmed = TOTAL_POSITIVE_PSDP) %>%
        mutate(Staff.Recovered = RECOVRD_RETURND_TO_WORK_PSDP) %>%
        mutate(Staff.Deaths = LIVE_LOST_PERSONNEL_PSDP) %>%
        mutate(Residents.Confirmed = TOTAL_POSITIVE_PSDR) %>%
        mutate(Residents.Recovered = RECOVRD_RES_PSDR) %>%
        mutate(Residents.Quarantine = 
                   TOTAL_POSITIVE_ISO_PSDR + RES_QUARANTINE_PSDR) %>%
        mutate(Residents.Deaths = LIVE_LOST_RES_PSDR) %>%
        select(starts_with("Residents"), starts_with("Staff")) %>%
        mutate(Name = "COUNTY WIDE")
}


#' Scraper class for general dc COVID data
#' 
#' @name dc_scraper
#' @description DC data pulled from an api call. Minimal cleaning needed.
#' \describe{
#'   \item{DATE_REPORTED}{Date}
#'   \item{TOTAL_POSITIVE_PSDP}{Staff positive.}
#'   \item{TOTAL_POSITIVE_OUT_PSDP}{Staff positive needs examining.}
#'   \item{RECOVRD_RETURND_TO_WORK_PSDP}{Staff recovered}
#'   \item{PERSON_QUARANTINE_PSDP}{Staff quarantined.}
#'   \item{OUT_POSITIVE_QUARANTINE_PSDP}{Staff quarantined needs examining.}
#'   \item{RETURND_TO_WORK_PSDP}{Staff at work after leaving.}
#'   \item{LIVE_LOST_PERSONNEL_PSDP}{Staff deaths.}
#'   \item{TOTAL_POSITIVE_PSDR}{Cumulative residents confirmed.}
#'   \item{TOTAL_POSITIVE_ISO_PSDR}{Residents isolated}
#'   \item{RECOVRD_RES_PSDR}{Residents recovered, cumulative.}
#'   \item{RES_QUARANTINE_PSDR}{Residents quarantined}
#'   \item{OUT_POSITIVE_QUARANTINE_PSDR}{Residents quarantined needs examining.}
#'   \item{RETURND_TO_GP_PSDR}{Residents released.}
#'   \item{LIVE_LOST_RES_PSDR}{Resident deaths.}
#' }

dc_scraper <- R6Class(
    "dc_scraper",
    inherit = generic_scraper,
    public = list(
        log = NULL,
        initialize = function(
            log,
            url = 
                "https://opendata.dc.gov/datasets/dc-covid-19-department-of-corrections/data?orderBy=DATE_REPORTED&orderByAsc=false",
            id = "dc",
            type = "json",
            state = "DC",
            jurisdiction = "county",
            check_date = dc_check_date,
            # pull the JSON data directly from the API
            pull_func = dc_pull,
            # restructuring the data means pulling out the data portion of the json
            restruct_func = dc_restruct,
            # Rename the columns to appropriate database names
            extract_func = dc_extract){
            super$initialize(
                url = url, id = id, pull_func = pull_func, type = type,
                restruct_func = restruct_func, extract_func = extract_func,
                log = log, state = state, jurisdiction = jurisdiction,
                check_date = check_date)
        }
    )
)

if(sys.nframe() == 0){
    dc <- dc_scraper$new(log=FALSE)
    dc$run_check_date()
    dc$raw_data
    dc$pull_raw()
    dc$raw_data
    dc$restruct_raw()
    dc$restruct_data
    dc$extract_from_raw()
    dc$extract_data
    dc$validate_extract()
}

