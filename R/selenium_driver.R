require(RSelenium)
loadNamespace("RSelenium")

create_selenium_driver <- function(browserName = "chrome"){
    profile <- setProfile(browserName)
    remDr <- remoteDriver(
        remoteServerAddr = "localhost",
        port = 4445,
        browserName = browserName,
    )
    
    return(remDr)
}

setProfile <- function(browserName){
    if(browserName == "firefox"){
        return(firefox_profile)
    }
    if(browserName == "chrome"){
        return(chrome_profile)
    }
    
}

chrome_profile <- list(
    chromeOptions = 
        list(prefs = list(
            "profile.default_content_settings.popups" = 0L,
            "download.prompt_for_download" = FALSE,
            "download.default_directory" = "/home/seluser/Downloads",
            "disable-dev-shm-usage" = TRUE
        )
        )
)

# need this string to tell selenium we just want to download the csv
csv_no_display_str <- str_c(
    "application/csv,application/excel,application/vnd.ms-excel,",
    "application/vnd.msexcel,text/anytext,text/comma-separated-values,",
    "text/csv,text/plain,text/x-csv,application/x-csv,",
    "text/x-comma-separated-values,text/tab-separated-values,",
    "data:text/csv,application/xml,text/plain,text/xml,image/jpeg,",
    "application/octet-stream,data:text/csv"
)

browser_preferences <- list(
    browser.startup.homepage = "about:blank",
    startup.homepage_override_url = "about:blank",
    startup.homepage_welcome_url = "about:blank",
    startup.homepage_welcome_url.additional = "about:blank",
    browser.download.dir = "/home/seluser/Downloads",
    browser.download.folderList = 2L,
    browser.download.manager.showWhenStarting = FALSE,
    browser.download.manager.focusWhenStarting = FALSE,
    browser.download.manager.closeWhenDone = TRUE,
    browser.helperApps.neverAsk.saveToDisk = csv_no_display_str,
    browser.helperApps.neverAsk.openFile = csv_no_display_str,
    pdfjs.disabled = TRUE,
    plugin.scan.plid.all = FALSE,
    plugin.scan.Acrobat = "99.0")

firefox_profile <- RSelenium::makeFirefoxProfile(browser_preferences)


