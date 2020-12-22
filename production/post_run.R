# singularity run --app Rscript init/singularity-r.simg production/post_run.R
rm(list=ls())
library(tidyverse)
source("./R/utilities.R")

sync_remote_files(TRUE)
write_latest_data()

