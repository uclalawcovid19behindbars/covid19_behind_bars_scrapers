library(behindbarstools)
library(tidyverse)
library(lubridate)
library(plotly)

read_fac_data <- read_scrape_data(all_dates = FALSE)

read_fac_data %>%
    filter(Jurisdiction == "state") %>%
    select(Capacity, Residents.Population, Residents.Confirmed) %>%
    mutate(Conf = Residents.Confirmed / Residents.Population) %>%
    mutate(Dens = Residents.Population / Capacity) %>%
    ggplot(aes(x = Dens, y = Conf)) +
    geom_point()

cap_data <- read_fac_data %>%
    filter(Jurisdiction == "state") %>%
    select(Capacity, Residents.Population, Residents.Confirmed) %>%
    filter(Residents.Population != 0 & Capacity != 0) %>%
    mutate(Conf = Residents.Confirmed / Residents.Population) %>%
    mutate(Dens = Residents.Population / Capacity) %>%
    mutate(DensC = ifelse(Dens > 1, "Above\nCapacity", "Below\nCapacity")) %>%
    na.omit() %>%
    group_by(DensC) %>%
    summarize(
        PC = mean(Conf)*1000, se = sd(Conf)/sqrt(n())*1000, N= n()) %>%
    mutate(DensC = fct_rev(DensC)) %>%
    mutate(lwr = PC - 1.98 * se, upr = PC + 1.98 * se)

cap_data

cap_data2 <- read_fac_data %>%
    filter(Jurisdiction == "state") %>%
    select(Capacity, Residents.Population, Residents.Deaths) %>%
    filter(Residents.Population != 0 & Capacity != 0) %>%
    mutate(Deaths = Residents.Deaths / Residents.Population) %>%
    mutate(Dens = Residents.Population / Capacity) %>%
    mutate(DensC = ifelse(Dens > 1, "Above\nCapacity", "Below\nCapacity")) %>%
    na.omit() %>%
    group_by(DensC) %>%
    summarize(
        DC = mean(Deaths)*1000, se = sd(Deaths)/sqrt(n())*1000, N= n()) %>%
    mutate(DensC = fct_rev(DensC)) %>%
    mutate(lwr = DC - 1.98 * se, upr = DC + 1.98 * se)

cap_data %>%
    ggplot(aes(
        x = DensC, y = PC, ymin = lwr, ymax = upr,
        fill = DensC, color = DensC)) +
    geom_col(width = .5, alpha = .5, color = NA) +
    geom_errorbar(width = .3) +
    theme_behindbars() +
    scale_fill_bbdiscrete() +
    scale_color_bbdiscrete() +
    labs(y="Infection Rate\nPer 1,000") +
    scale_y_continuous(labels = scales::comma) +
    theme(legend.position = "none")

mp_df <- read_mpap_data(all_dates = FALSE)

pop_df <- "https://raw.githubusercontent.com/themarshallproject/" %>%
    str_c("COVID_prison_data/master/data/prison_populations.csv") %>%
    read_csv() %>%
    mutate(Date = mdy(as_of_date)) %>%
    rename(State = name) %>%
    group_by(State) %>%
    filter(Date == max(Date)) %>%
    filter(1:n() == 1) %>%
    ungroup() %>%
    select(State, Population = pop)

wide_rate_df <- mp_df %>%
    select(State, Residents.Confirmed, Residents.Deaths, Residents.Tadmin) %>%
    pivot_longer(-State) %>%
    left_join(pop_df, by = "State") %>%
    mutate(value = value / Population * 100000) %>%
    select(-Population) %>%
    pivot_wider()

td_plot <- wide_rate_df %>%
    ggplot(aes(x = Residents.Tadmin, y = Residents.Deaths)) +
    geom_point(aes(text = State)) +
    geom_smooth(method="lm") +
    labs(x = "Testing Rate per 100,000", y = "Death Rate per 100,000")

ggplotly(td_plot)

summary(lm(Residents.Deaths ~ Residents.Tadmin, data = wide_rate_df))

wide_rate_df %>%
    mutate(More4x = Residents.Tadmin > 400000) %>%
    select(State, More4x, Residents.Deaths) %>%
    na.omit() %>%
    group_by(More4x) %>%
    summarize(D = mean(Residents.Deaths, na.rm=T), N=n())
