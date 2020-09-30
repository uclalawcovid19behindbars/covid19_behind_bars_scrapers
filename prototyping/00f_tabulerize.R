rm(list=ls())
# library(tabulizer)
# library(tidyverse)
# library(rvest)
# library(httr)
# library(xml2)
# library(RSelenium)
library(magrittr)


get_WI_resident <- function(out_file = "~/Downloads/wi_res.pdf"){
    tmp_dir <- "~/tmp/"

    fprof <- RSelenium::makeFirefoxProfile(list(
        browser.startup.homepage = "about:blank",
        startup.homepage_override_url = "about:blank",
        startup.homepage_welcome_url = "about:blank",
        startup.homepage_welcome_url.additional = "about:blank",
        browser.download.dir = "/home/seluser/Downloads",
        browser.download.folderList = 2L,
        browser.download.manager.showWhenStarting = FALSE,
        browser.download.manager.focusWhenStarting = FALSE,
        browser.download.manager.closeWhenDone = TRUE,
        browser.helperApps.neverAsk.saveToDisk = 
            "application/pdf, application/octet-stream",
        pdfjs.disabled = TRUE,
        plugin.scan.plid.all = FALSE,
        plugin.scan.Acrobat = 99L))
    
    remDr <- RSelenium::remoteDriver(
        remoteServerAddr = "localhost",
        port = 4445,
        browserName = "firefox",
        extraCapabilities=fprof
    )
    del_ <- capture.output(remDr$open())

    "https://public.tableau.com/views/WIDOCCOVID19/COVID-19Table" %>% 
        stringr::str_c(
            "?%3Aembed=y&%3AshowVizHome=no&%3Ahost_url=https%3A%2F%2F",
            "public.tableau.com%2F&%3Aembed_code_version=3&%3Atabs=yes&%3A",
            "toolbar=no&%3Aanimate_transition=yes&%3Adisplay_static_image=no&%3A",
            "display_spinner=no&%3Adisplay_overlay=yes&%3Adisplay_count=yes&%3A",
            "language=en&%3AloadOrderID=0") %>%
        remDr$navigate()
    Sys.sleep(1)

    remDr$findElement(
        "css", "[id='download-ToolbarButton']")$clickElement()
    Sys.sleep(1)
    remDr$findElement(
        "css", "[data-tb-test-id='DownloadPdf-Button']")$clickElement()
    Sys.sleep(2)
    remDr$findElement(
        "css", "[data-tb-test-id='PdfDialogCreatePdf-Button']")$clickElement()
    Sys.sleep(2)

    file.copy(stringr::str_c(tmp_dir, "/COVID-19 Table.pdf"), out_file)
}

get_WI_resident()

library(tabulizer)
library(tidyverse)
# this was found by first running
pdf_area <- list(c(104.64249, 43.36788, 431.90674, 566.58031))

raw_ex_mat <- extract_tables(
    "~/Downloads/wi_res.pdf", pages = 1, area = pdf_area)[[1]]

col_names <- apply(raw_ex_mat[1:2,], 2, str_c, collapse=" ") %>%
    str_trim() %>%
    # Facility doesnt have a name
    {ifelse(. == "", "Facility", .)} %>%
    str_replace_all(" ", ".")

colnames(raw_ex_mat) <- col_names

wi_res_df <- raw_ex_mat[-(1:2),] %>%
    as_tibble()
