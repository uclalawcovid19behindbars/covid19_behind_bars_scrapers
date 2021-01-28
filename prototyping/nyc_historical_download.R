rm(list = ls())
library(tidyverse)
library(lubridate)
library(tryCatchLog)

base <- "https://hhinternet.blob.core.windows.net/uploads/%Y/%M/" %>%
    str_c("CHS-COVID-19-data-snapshot-%Y%M%D.pdf")


end_date <- lubridate::ymd("2021-01-25")
start_date <- lubridate::ymd("2020-11-05")
new_date <- start_date

while(new_date <= end_date){
    durl <- base %>%
        str_replace_all("%Y", sprintf("%04d", year(new_date))) %>%
        str_replace_all("%M", sprintf("%02d", month(new_date))) %>%
        str_replace_all("%D", sprintf("%02d", day(new_date)))
    
    out_file <- str_c(
        "results/raw_files/", as.character(new_date), "_nyc_jails.pdf")
    
    tryCatch(download.file(durl, out_file), error = function(e) e, finally = "")
    
    new_date <- new_date + 1
}
