rsconnect::deployApp(appFileManifest = "manifest.txt",
                     appName = "shinyLogsAnalyseR",
                     launch.browser = T,
                     account = "drees",forceUpdate = T
)
rsconnect::deployApp(appFileManifest = "manifest.txt",
                     appName = "shinyLogsAnalyseR_dev",
                     launch.browser = T,
                     account = "drees",forceUpdate = T
)
rsconnect::showLogs(streaming=T)