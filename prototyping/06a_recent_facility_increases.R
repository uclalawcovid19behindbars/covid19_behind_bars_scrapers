source("./R/utilities.R")

hist_data <- load_latest_data(TRUE, TRUE)


fac_delta_df <- hist_data %>%
    mutate(Date = lubridate::mdy(Date)) %>%
    filter(!(str_detect(Name, "(?i)state") & str_detect(Name, "(?i)wide"))) %>%
    filter(Date >= (lubridate::as_date((Sys.Date())) - lubridate::days(7))) %>%
    group_by(Name, State, jurisdiction) %>%
    mutate(delta = last(Residents.Confirmed) - first(Residents.Confirmed)) %>%
    ungroup() %>%
    filter(delta %in% head(sort(unique(delta), decreasing = TRUE), n = 4)) %>%
    select(jurisdiction, State, Name) %>%
    unique() %>%
    left_join(hist_data, by = c("jurisdiction", "State", "Name"))

out_plot <- fac_delta_df %>%
    mutate(Date = lubridate::mdy(Date)) %>%
    mutate(Title = str_c(State, ": ", Name)) %>%
    ggplot(aes(x = Date, y = Residents.Confirmed)) +
    geom_line() +
    facet_wrap(~Title) +
    labs(y = "Residents Confirmed") +
    theme_bw() +
    ggtitle(
        "High Outbreak Facilities",
        "Carceral Facilities with Largest Increase In Resident Cases in Past 7 Days"
    ) +
    theme(
        legend.text = element_text(size=13),
        legend.title = element_text(size=15),
        axis.text = element_text(size=13),
        axis.title = element_text(size=15),
        title =  element_text(size=17))


ggsave("./data/Adult Facility Counts/weekly_facility_monitor.png", out_plot)
