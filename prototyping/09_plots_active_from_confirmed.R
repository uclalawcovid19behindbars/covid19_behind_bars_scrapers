library(behindbarstools)
library(tidyverse)
library(lubridate)

# rolling sum window for n days
rsum.cumsum <- function(x, n = 3L) {
    cumsum(x) - cumsum(c(rep(0, n), head(x, -n)))
}

# define an activitve window
days_active <- 14
# choose a facility to examine
Facility <- "NORTH CAROLINA CORRECTIONAL INSTITUTION FOR WOMEN"

# choose historical data to pull
nc_data <- "https://raw.githubusercontent.com/uclalawcovid19behindbars/" %>%
    str_c("historical-data/main/data/NC-historical-data.csv") %>%
    read_csv()

est_active <- nc_data %>%
    filter(Name == Facility) %>%
    select(Residents.Confirmed, Date) %>%
    arrange(Date) %>%
    filter(!is.na(Residents.Confirmed) & !is.na(Date)) %>%
    mutate(New.Cases = Residents.Confirmed - lag(Residents.Confirmed)) %>%
    mutate(Delta = as.numeric(Date - lag(Date))) %>%
    filter(!is.na(New.Cases)) %>%
    right_join(tibble(Date = seq(min(.$Date), max(.$Date), 1)), by = "Date") %>%
    arrange(Date) %>%
    mutate(New.Cases = ifelse(is.na(New.Cases), 0, New.Cases)) %>%
    mutate(Estimated.Active = rsum.cumsum(New.Cases, n = days_active))

if(max(est_active$Delta, na.rm = TRUE) > days_active/2){
    warning(
        "Because of inconsitent data reporting these estimates should be",
        "taken with caution.")
}

active_nc_plot <- est_active %>%
    mutate(Facility = Facility) %>%
    ggplot(aes(x = Date, y = Estimated.Active, color = Facility, fill = Facility)) +
    geom_line(size = 1.5) +
    geom_area(alpha = .5) +
    theme_behindbars() +
    scale_color_bbdiscrete() +
    scale_fill_bbdiscrete() +
    theme(legend.position = "none") +
    labs(y = "Estimated Active Cases") +
    ggtitle("Monitoring Facility Outbreaks", str_to_title(Facility))


ggsave("Downloads/nc_backcalc_active.svg", plot = active_nc_plot)
