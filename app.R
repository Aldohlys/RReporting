NewTrading="C:\\Users\\aldoh\\Documents\\NewTrading\\"

options(encoding = "UTF-8")

source("helpersv2.R")

shinyApp(ui = ui, server = server, options = list(launch.browser = TRUE, width=1000))
