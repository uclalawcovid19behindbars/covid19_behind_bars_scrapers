# singularity run --app Rscript init/singularity-r.simg production/post_run.R
rm(list=ls())
library(googlesheets4)
library(glue)
source("./R/utilities.R")

# Sync raw, extracted, and log files
sync_remote_files(TRUE)
# summarize the remote files
summarize_remote_data()

# Generate and sync diagnostics
generate_diagnostics()

# Update facility outbreaks sheet
update_fac_outbreaks_sheet(
    outbreaks_sheet_loc = "1I7oubSBZT1GnDL30f4jHzIQwQGso5RulrrBUgxFfRAM",
    delta_days = 7
    )



# Sync diagnotics files
sync_diagnostic_files()

# Write latest csv 
write_latest_data()
