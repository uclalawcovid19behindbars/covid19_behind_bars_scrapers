check_browser_name <- function(browserName){
    if(!(browserName %in% c("firefox", "chrome"))){
        stop("browserName must either be 'firefox' or 'chrome'")
    }
}

build_profile <- function(browserName){
    no_display_str <- str_c(
        "application/csv,application/excel,application/vnd.ms-excel,",
        "application/vnd.msexcel,text/anytext,text/comma-separated-values,",
        "text/csv,text/plain,text/x-csv,application/x-csv,",
        "text/x-comma-separated-values,text/tab-separated-values,",
        "data:text/csv,application/xml,text/plain,text/xml,image/jpeg,",
        "application/octet-stream,data:text/csv,application/pdf"
    )

    if(browserName == "firefox"){
        # set up the ff profile to tell selenium just to download csvs and not 
        # bring up a prompt or display them
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
            browser.helperApps.neverAsk.saveToDisk = no_display_str,
            browser.helperApps.neverAsk.openFile = no_display_str,
            pdfjs.disabled = TRUE,
            plugin.scan.plid.all = FALSE,
            plugin.scan.Acrobat = "99.0"))
    }else{
        fprof <- list()
    }
    
    return(fprof)
}

initiate_remote_driver <- function(browserName = "firefox", port = 4445){
    
    check_browser_name(browserName)
    
    fprof <- build_profile(browserName)
    
    RSelenium::remoteDriver(
        remoteServerAddr = "localhost",
        port = port,
        browserName = browserName,
        extraCapabilities=fprof
    )
}