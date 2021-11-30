rm(list = ls())

library(tidyverse)
library(behindbarstools)

'%!in%' <- function(x,y)!('%in%'(x,y))

# ------------------------------------------------------------------------------

## load data from fac vetting process
facs_from_vetting <- tibble(Facility.ID = c(2336,
                       2039,
                       619,
                       1788,
                       1805,
                       1997,
                       2021,
                       2186,
                       611,
                       1771,
                       1770,
                       1671,
                       1670,
                       1878)) ## rm all ice because it comes from state agg counts

fac_info <- read_fac_info()
tx_ids <- fac_info %>% 
    filter(State == "Texas") %>% 
    select(Facility.ID, State, Name)

tx_plus_fac_vetting <- bind_rows(tx_ids, facs_from_vetting)

# ------------------------------------------------------------------------------

# Only flag when cumulative counts drop by DIFF_THRESHOLD
DIFF_THRESHOLD <- 5 

# AND that decline is more than PCT_THRESHOLD percent 
PCT_THRESHOLD <- 0.1

# ------------------------------------------------------------------------------

GITHUB_PATH <- "https://raw.githubusercontent.com/uclalawcovid19behindbars/"
FAC_PATH <- "data/master/historical-data/historical_facility_counts.csv"
STATE_PATH <- "data/master/historical-data/historical_state_counts.csv"

# Read raw data 
state_df <- read.csv(str_c(GITHUB_PATH, STATE_PATH))
fac_df <- read.csv(str_c(GITHUB_PATH, FAC_PATH))

# Combine facility and statewide data 
combined_df <- bind_rows(
    fac_df %>% 
        select(Facility.ID, Jurisdiction, State, Name, Date, ends_with(".Confirmed")),
    state_df %>% 
        mutate(Name = "STATEWIDE", 
               Jurisdiction = ifelse(State %in% c("federal", "ice"), State, "state")) %>% 
        select(Jurisdiction, State, Name, Date, ends_with(".Confirmed")) 
)

# Reshape long (each row is a facility + date + metric combination)
long_df <- combined_df %>% 
    pivot_longer(!(c(Facility.ID, Jurisdiction, State, Name, Date)), 
                 names_to = "Measure", 
                 values_to = "Value") %>% 
    filter(!is.na(Value)) %>% 
    arrange(Facility.ID, Jurisdiction, State, Name, Measure, Date) %>% 
    group_by(Facility.ID, Jurisdiction, State, Name, Measure) %>% 
    mutate(Diff = Value - lag(Value), 
           Pct.Diff = Diff / Value) %>% 
    ungroup()
    
# Flag issues 
issues_df <- long_df %>% 
    filter(Facility.ID %!in% tx_plus_fac_vetting) %>% 
    # Multiply by -1 because we want to flag drops 
    filter((Diff < (DIFF_THRESHOLD * -1)) & (Pct.Diff < (PCT_THRESHOLD * -1))) 

# Write to csv
write.csv(issues_df, "facility-data-issues.csv",  row.names = FALSE, na = "")

# Create updated list of facility IDs to exclude
facs_bad_drop <- issues_df %>% 
    filter(Diff < -100) %>%
    select(Facility.ID, State, Name) %>%
    unique() %>% 
    bind_rows(tx_plus_fac_vetting)

# Write to csv
write.csv(facs_bad_drop, file.path("prototyping", "exclude_dashboard_facilities.csv"), 
                                   row.names = FALSE, na = "")
