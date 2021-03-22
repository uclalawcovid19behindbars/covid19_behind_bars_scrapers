rm(list=ls())
library(tidyverse)
library(behindbarstools)
library(googlesheets4)
gs4_auth("ucla.law.covid.staff@gmail.com")

sheet_ <- "1QGaFRQwppmZJoAc7QdgH9pY5cKOyv6gd_Q6lFMgwc4k"

imm_df <- read_scrape_data(TRUE) %>%
    filter(Jurisdiction == "immigration")

new_df <- imm_df %>%
    # remove not available locations and NA locations
    filter(!is.na(Facility.ID) & State != "Not Available") %>%
    # save only the latest data
    group_by(Facility.ID) %>%
    filter(Date == max(Date)) %>%
    ungroup() %>%
    select(
        `Facility ID` = Facility.ID, Address, State, Name, County, Date, City,
        `ICE Field Office` = ICE.Field.Office,
        `TOTAL Confirmed Cases - ICE DATA (Detainees)` = Residents.Confirmed,
        `ACTIVE Confirmed Cases - ICE DATA\n(Detainees)` = Residents.Active,
        `Confirmed Deaths - ICE DATA (Detainees)` = Residents.Deaths
        )

update_cols <- names(new_df)[names(new_df) != "Facility ID"]

old_sheet <- read_sheet(sheet_)
old_order <- names(old_sheet)

new_facilities <- new_df$`Facility ID`[!(
    new_df$`Facility ID` %in% old_sheet$`Facility ID`)] %>%
    {tibble(`Facility ID` = .)}

update_sheet <- old_sheet %>%
    bind_rows(new_facilities) %>%
    select(-!!update_cols) %>%
    left_join(new_df, by = "Facility ID") %>%
    select(!!old_order)

sum_cols <- c(
    "TOTAL Confirmed Cases - ICE DATA (Detainees)",
    "ACTIVE Confirmed Cases - ICE DATA\n(Detainees)",
    "Confirmed Deaths - ICE DATA (Detainees)"
)

update_sheet$Name[2] <- "Total:"

for(sc in sum_cols){
    update_sheet[[sc]][2] <- sum_na_rm(update_sheet[[sc]])
}

range_write(data = update_sheet, ss = sheet_, sheet = 1, reformat = FALSE)
