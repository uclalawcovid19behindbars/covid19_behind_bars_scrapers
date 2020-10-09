rm(list=ls())
library(tidyverse)

# docker run -d -p 4445:4444 -p 5901:5900 \\
# -v /tmp/sel_dl:/home/seluser/Downloads \\
# selenium/standalone-firefox-debug:2.53.1

get_WI_resident <- function(){
    
    out_file <- str_c(
        "/srv/shiny-server/WI_covid_files/", Sys.Date(), "WI_covid.pdf")
    
    tmp_dir <- "/tmp/sel_dl"

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
