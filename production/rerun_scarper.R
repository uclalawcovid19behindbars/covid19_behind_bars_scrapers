# singularity run --app Rscript init/singularity-r.simg production/main.R
rm(list=ls())
library(tidyverse)
library(R6)
library(tryCatchLog)
library(futile.logger)
options(tryCatchLog.include.full.call.stack = FALSE)

