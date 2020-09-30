# when there is a janky undocumented api
rm(list = ls())
library(httr)
library(tidyverse)
library(jsonlite)

al_data_url <- str_c(
    "https://services7.arcgis.com/jF2q3LPxL7PETdYk/arcgis/rest/services/", 
    "esriMapData/FeatureServer/0/query?f=json&",
    "where=State%20%3D%20%27AL%27&returnGeometry=false&",
    "spatialRel=esriSpatialRelIntersects&outFields=*",
    "&orderByFields=ShortName%20asc&outSR=102100&resultOffset=0&",
    "resultRecordCount=100&resultType=standard&cacheHint=true")

as_tibble(fromJSON(al_data_url)$features$attributes)

nm_data_url <- str_c(
    "https://e7p503ngy5.execute-api.us-west-2.amazonaws.com/prod/",
    "GetCorrectionalFacilities"
)

as_tibble(fromJSON(nm_data_url)$data)
