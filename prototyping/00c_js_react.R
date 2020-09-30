# for when you need to do some JS things
rm(list=ls())
library(tidyverse)
library(rvest)
library(httr)
library(xml2)
library(RSelenium)

remDr <- remoteDriver(
    remoteServerAddr = "localhost",
    port = 4445,
    browserName = "firefox"
)

remDr$getStatus()
remDr$open()
remDr$navigate("http://www.doc.alabama.gov/covid19news")

new_frame <- remDr$getPageSource() %>%
    {read_html(.[[1]])} %>%
    html_node("iframe") %>%
    html_attr("src")

remDr$navigate(new_frame)

all_spans <- remDr$getPageSource() %>%
    {read_html(.[[1]])} %>%
    html_nodes("margin-container") %>%
    .[[length(.)]] %>%
    html_nodes("span")

col_names <- all_spans[html_attr(all_spans, "style")==""] %>%
    .[[1]] %>%
    html_nodes("strong") %>%
    html_text()

col_vals <- all_spans[html_attr(all_spans, "style")==""] %>%
    .[[1]] %>%
    html_nodes("span") %>%
    html_text() %>%
    as.numeric()

