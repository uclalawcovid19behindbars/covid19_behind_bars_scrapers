library(tidyverse)

df <- list.files("~/Desktop/lasd_extract/extracted_data/", full.names = T) %>%
    lapply(function(x){
        df_ <- fread(x)
        if(nrow(df_) == 0){
            df_ <- data.table()
        }
        if("Date" %in% names(df_)){
            df_[,Date := lubridate::as_date(Date)]
        }
        df_
    }) %>%
    rbindlist(fill=TRUE, use.names = TRUE) 

write_csv(df, "~/Desktop/lasd_extract/lasd_rescrape.csv")

