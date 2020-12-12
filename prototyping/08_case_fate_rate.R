library(tidyverse)

pop_df <- "https://raw.githubusercontent.com/themarshallproject/" %>% 
    str_c("COVID_prison_data/master/data/prison_populations.csv") %>%
    read_csv() %>%
    select(name, march_pop)

stat_df <- "https://raw.githubusercontent.com/themarshallproject/" %>%
    str_c("COVID_prison_data/master/data/covid_prison_cases.csv") %>%
    read_csv() %>%
    select(name, total_prisoner_cases, total_prisoner_deaths, prisoner_tests) %>%
    left_join(pop_df) %>%
    group_by(name) %>%
    mutate(N = 1:n()) %>%
    filter(N == 1) %>%
    ungroup() %>%
    mutate(death_rate = total_prisoner_deaths/march_pop) %>%
    mutate(inf_rate = total_prisoner_cases/march_pop)

test <- stat_df %>%
    ggplot(aes(x=inf_rate, y = death_rate, text = name)) +
    geom_point()

library(plotly)
ggplotly(test)
