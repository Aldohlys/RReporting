NewTrading="C:\\Users\\aldoh\\Documents\\NewTrading\\"
RReporting="C:\\Users\\aldoh\\Documents\\R\\Trading\\RReporting\\"


options(encoding = "UTF-8")

source(paste0(RReporting,"helpersv2.R"))

source(paste0(RReporting,"server.R"))
source(paste0(RReporting,"ui.R"))


shinyApp(ui = ui, server = server, options = list(launch.browser = TRUE, width=1000))
