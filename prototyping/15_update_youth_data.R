rm(list=ls())
library(tidyverse)
library(behindbarstools)
library(googlesheets4)
gs4_auth("ucla.law.covid.staff@gmail.com")


fac_sheet <- "1AfqaEPZTMy1hMdnZC8UYMP_JTJrnDlLi_6uh8sxmQWM"

# fac_sheet_df <- read_sheet(fac_sheet, sheet = "Facility TAB") %>%
#     filter(!is.na(`Facility ID`)) %>%
#     select(-starts_with("...") , -ends_with("(Detainees)")) %>%
#     select(-ends_with("Staff)"), -Address, -State)

fac_sheet_df <- behindbarstools::read_fac_info()

all_dat <- read_scrape_data(all_dates = FALSE) 
youth_df <- all_dat %>%
    filter(Age == "Juvenile")

new_df <- youth_df %>%
    # save only the latest data
    group_by(Facility.ID) %>%
    filter(Date == max(Date)) %>%
    ungroup() %>%
    select(
        `Facility ID` = Facility.ID, 
         Name, Address, City, State,
        `TOTAL Confirmed Cases - YOUTH` = Residents.Confirmed,
        `ACTIVE Confirmed Cases - YOUTH` = Residents.Active,
        `Confirmed Deaths - YOUTH` = Residents.Deaths,
        Date
    ) %>%
    left_join(fac_sheet_df, by = "Facility ID") %>%
    select(-`Facility ID`)

update_sheet <- bind_rows(
    data.frame(V1 = c("", ""), Name = c("", "")),
    as.data.frame(new_df)
)

sum_cols <- c(
    "TOTAL Confirmed Cases - YOUTH",
    "ACTIVE Confirmed Cases - YOUTH",
    "Confirmed Deaths - YOUTH"
)

update_sheet$Name[2] <- "Total:"

for(sc in sum_cols){
    update_sheet[[sc]][2] <- sum_na_rm(update_sheet[[sc]])
}

names(update_sheet)[1] <- ""

range_write(
    data = update_sheet, 
    ss = fac_sheet, 
    sheet = "main", 
    reformat = FALSE)
