rm(list=ls())
library(tidyverse)
library(behindbarstools)
library(googlesheets4)
gs4_auth("ucla.law.covid.staff@gmail.com")


fac_sheet <- "17hHakJXVooK_9OTW0Uk5ftqOSjLZTt_moq1iumi_J7M"
fac_sheet_df <- read_sheet(fac_sheet, sheet = "Facility TAB") %>%
    filter(!is.na(`Facility ID`)) %>%
    select(-starts_with("...") , -ends_with("(Detainees)")) %>%
    select(-ends_with("Staff)"), -Address, -State)

imm_df <- read_scrape_data(all_dates = FALSE) %>%
    filter(Jurisdiction == "immigration")

new_df <- imm_df %>%
    # remove not available locations and NA locations
    filter(!is.na(Facility.ID) & State != "Not Available") %>%
    # save only the latest data
    group_by(Facility.ID) %>%
    filter(Date == max(Date)) %>%
    ungroup() %>%
    select(
        `Facility ID` = Facility.ID, Name, Address, City, State,
        `TOTAL Confirmed Cases - ICE DATA (Detainees)` = Residents.Confirmed,
        `ACTIVE Confirmed Cases - ICE DATA\n(Detainees)` = Residents.Active,
        `Confirmed Deaths - ICE DATA (Detainees)` = Residents.Deaths,
        Date
        ) %>%
    left_join(fac_sheet_df, by = "Facility ID") %>%
    select(-`Facility ID`)

update_sheet <- bind_rows(
    data.frame(V1 = c("", ""), Name = c("", "")),
    as.data.frame(new_df)
)

sum_cols <- c(
    "TOTAL Confirmed Cases - ICE DATA (Detainees)",
    "ACTIVE Confirmed Cases - ICE DATA\n(Detainees)",
    "Confirmed Deaths - ICE DATA (Detainees)"
)

update_sheet$Name[2] <- "Total:"

for(sc in sum_cols){
    update_sheet[[sc]][2] <- sum_na_rm(update_sheet[[sc]])
}

names(update_sheet)[1] <- ""

range_write(
    data = update_sheet, ss = fac_sheet, 
    sheet = "MAIN PAGE [Linked to UCLA Database]", reformat = FALSE)
