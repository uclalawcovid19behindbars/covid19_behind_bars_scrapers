# Pre run script to make sure that you have the necessary libraries installed
# to run the scraper, update to the latest version of our teams package, and
# make sure that you have enough extractable credits to run the files

required_packages <- c(
    "rgl", "tidyverse", "rvest", "httr", "xml2", "magick", "pdftools",
    "jsonlite", "lubridate", "R6", "futile.logger", "tryCatchLog", "argparse",
    "tesseract", "splashr", "tabulizer", "RSelenium", "remotes", "ggrepel",
    "googlesheets4", "rmarkdown", "kableExtra", "janitor", "glue"
)

your_packages <- row.names(installed.packages())
missing_packages <- setdiff(required_packages, your_packages)

if(length(missing_packages) == 0){
    cat("You have all the required packages installed.\n")
}

if(length(missing_packages) != 0){
    for(i in 1:length(missing_packages)){
        mpkg <- missing_packages[i]
        cat("You are missing the ", mpkg, "package which will be installed.\n")
        install.packages(mpkg, repos = "http://cran.us.r-project.org")
    }
}

# delete local folders if they exist
sc_dirs <- paste0(
    "results/",
    c("log_files/", "extracted_data/", "raw_files/", "last_update/"))
for(di in sc_dirs){
    if(dir.exists(di)){
        unlink(di, recursive = TRUE)
    }
}

# always want to update barstools to the latest and greatest
remotes::install_github("uclalawcovid19behindbars/behindbarstools")

# check your credits before we begin
source("./R/utilities.R")
check_extractable_credits()
