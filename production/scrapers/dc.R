source("./R/generic_scraper.R")

dc_extract <- function(x){
    x %>%
        # the format is in unix time with extra precision not readable
        # by default in the R package lubridate
        mutate(Date = lubridate::as_datetime(DATE_REPORTED/1000)) %>%
        mutate(Date = lubridate::as_date(Date)) %>%
        filter(Date == max(Date)) %>%
        rename(Staff.Confirmed = TOTAL_POSITIVE_PSDP) %>%
        rename(Staff.Confirmed = TOTAL_POSITIVE_PSDP)
    
    NULL
}

#' Scraper class for general dc COVID data
#' 
#' @name dc_scraper
#' @description This will be a description of dc data and what the scraper
#' does
#' \describe{
#'   \item{Facility_Name}{The facility name.}
#' }

dc_scraper <- R6Class(
    "dc_scraper",
    inherit = generic_scraper,
    public = list(
        log = NULL,
        initialize = function(
            log,
            url = stringr::str_c(
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
                "resultType=&featureEncoding=esriDefault&f=pjson"),
            id = "dc",
            type = "json",
            state = "DC",
            # pull the JSON data directly from the API
            pull_func = function(...) jsonlite::read_json(..., simplifyVector = TRUE),
            # restructuring the data means pulling out the data portion of the json
            restruct_func = function(x) as_tibble(x$features$attributes),
            # Rename the columns to appropriate database names
            extract_func = function(x){NULL}){
            super$initialize(
                url = url, id = id, pull_func = pull_func, type = type,
                restruct_func = restruct_func, extract_func = extract_func,
                log = log, state = state)
        }
    )
)

if(sys.nframe() == 0){
    dc <- dc_scraper$new(log=FALSE)
    dc$raw_data
    dc$pull_raw()
    dc$raw_data
    dc$restruct_raw()
    dc$restruct_data
    dc$extract_from_raw()
    dc$extract_data
}

