source("./R/generic_scraper.R")

georgia_scraper <- R6Class(
    "georgia_scraper",
    inherit = generic_scraper,
    public = list(
        log = NULL,
        initialize = function(
            log,
            url = stringr::str_c(
                "https://services5.arcgis.com/mBtYHKRd2hqJxboF/arcgis/rest/services/",
                "COVID19Statewide/FeatureServer/0/query?f=json&where=1%3D1&",
                "returnGeometry=false&spatialRel=esriSpatialRelIntersects&outFields=*&",
                "resultOffset=0&resultRecordCount=1000&resultType=standard",
                "&cacheHint=true"),
            id = "georgia",
            type = "json",
            # pull the JSON data directly from the API
            pull_func = function(...) jsonlite::read_json(..., simplifyVector = TRUE),
            # restructuring the data means pulling out the data portion of the json
            restruct_func = function(x) as_tibble(x$features$attributes),
            # Rename the columns to appropriate database names
            extract_func = function(x){
                x %>% 
                    select(
                        Name = Facility_Name, Staff.Confirmed = Staff_Confirmed, 
                        Staff.Recovered = Staff_Recovered, Staff.Deaths = Staff_Deaths, 
                        Residents.Confirmed = Inmate_Confirmed, 
                        Residents.Recovered = Inmate_Recovered, 
                        Residents.Deaths = Inmate_Deaths)}){
            super$initialize(
                url, id, pull_func, type, restruct_func, extract_func, log)
        }
    )
)

if(sys.nframe() == 0){
    georgia <- georgia_scraper$new(log=FALSE)
    georgia$raw_data
    georgia$pull_raw()
    georgia$raw_data
    georgia$restruct_raw()
    georgia$restruct_data
    georgia$extract_from_raw()
    georgia$extract_data
}

