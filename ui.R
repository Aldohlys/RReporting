########################################  UI ###############################
ui <- fluidPage(
  titlePanel("Journal de trades"),

  fluidRow(
    column(width = 3,
           selectInput("account","Account: ",choices=c("Live","Simu"),selected="Live"),
           textInput("sym",label="Symbol: ",placeholder="All to get all tickers",value="All"),
           sliderInput("init_nr",label="Init nr.: ",max=300,min=1,value=250,step = 5),
           sliderInput("init_date",label="Init date: ",max=today(),min=ymd("2022-10-01"),value=today()-30
                       ,step = 7),
           br(),
           p(strong("SPY= "),lastSPY$last,strong("   Change= "),lastSPY$change),
           p(strong("Symbol price:"),textOutput("sym_price",inline=TRUE),
             strong("Symbol price change:"),textOutput("sym_price_change",inline=TRUE))
    ),

    column(width=9,
           tabsetPanel(type = "tabs",
                       tabPanel("Trades",
                                h3("TRADE"),
                                h5("PnL",tableOutput("PnL")),
                                DTOutput("trades"),
                                actionButton("loadTrades","Load", icon("truck"),
                                             style="color: white; background-color: #337ab7; border-color: #2e6da4"),
                                actionButton("saveTrades", "Save", icon("paper-plane"),
                                    style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                       ),
                       tabPanel("New Trades",
                                fluidRow(
                                  column(width=6,
                                         fileInput("file",label=" ",accept = ".csv")),
                                  column(width=6,
                                         actionButton("open","Open", icon("envelope-open"),
                                                        style="color: white; background-color: #337ab7; border-color: #2e6da4"),
                                         actionButton("adjust","Adjust", icon("pencil"),
                                                        style="color: white; background-color: #337ab7; border-color: #2e6da4"),
                                         actionButton("close","Close", icon("envelope"),
                                                      style="color: white; background-color: #337ab7; border-color: #2e6da4"),
                                        actionButton("remove","Remove", icon("trash"),
                                                    style="color: white; background-color: #337ab7; border-color: #2e6da4"))

                                ),
                                br(),
                                DTOutput("newtrades")
                       )
           )
    )
  )
)

