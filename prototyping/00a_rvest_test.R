# for when the tables are tidy
rm(list=ls())
library(tidyverse)
library(rvest)
library(httr)
library(xml2)
library(V8)

web_page_source <- read_html(
    "https://corrections.az.gov/adcrr-covid-19-dashboard")

web_page_source %>%
    html_nodes("table") %>%
    .[[2]] %>%
    html_table() %>%
    as_tibble()

web_page_source <- read_html(
    "https://www.idoc.idaho.gov/content/careers/covid-19")

web_page_source %>%
    html_nodes("[class='covid-table']") %>%
    .[[1]] %>%
    html_table() %>%
    as_tibble() %>%
    .[1:2,]
