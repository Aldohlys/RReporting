#### R ANALYSIS - server part
#### April 2023

############################  SERVER ##############################
### Format datatable for Journal entry
### Special treatment for the \n or <br> HTML tag
format_datatable = function(dt) {
  datatable(dt, 
            filter='top',
            options=list(
              paging=TRUE,searching=FALSE, info=FALSE,ordering=TRUE,autowidth=TRUE,
              columnDefs = list(list(
                targets = "_all",
                render = JS('function(data, type, full) {
                  function formatColumn(data) {
                    return data.replace(/\\n/g, "<br>");
                  }
                  return $("<div/>").html(data).text();
                }')
              ))
            )
            # ,rownames=FALSE
  ) %>%
    formatStyle(
      columns=names(dt),
      whiteSpace= "pre-wrap") %>%
    formatCurrency(3:5)
}

server <- function(input, output, session) {
  
  #### Wait 3 seconds before invalidating previous user input
  i_s_d = debounce(reactive(input$sym),3000)
  tickerData = reactive(getLastTickerData(i_s_d()))
  
  output$sym_price=renderText({tickerData()$last})
  output$sym_price_change=renderText({tickerData()$change})
  
  # Output the text entered by the user
  # output$mkt_data <- renderTable({getLastSPY()})
  
  ###################### Tab Idées #####################################
  observeEvent(input$saveIdee,
               {
                 log_entry=data.frame(
                   date=format(today(),"%d-%m-%Y"),
                   sym=input$sym,
                   sym_close=tickerData()$last,
                   sym_close_change=tickerData()$change,
                   mkt_price=lastSPY$last,
                   mkt_change=lastSPY$change,
                   # text=deparse(input$text_input))
                   text=input$inputIdee)
                 write.table(log_entry,file=paste0(NewTrading,"Journal.csv"),append=TRUE,
                             col.names=FALSE,row.names=FALSE,sep=";",dec=".",quote=T)
                 updateTextAreaInput(session,"inputIdee",value="")
               })
  
  ################################  Tab Journal #################################
  output$text_output = renderDT({
    df=as.data.frame(read.table(file=paste0(NewTrading,"Journal.csv"),header=TRUE,sep=";",dec="."))
    if (i_s_d() != "All") df=filter(df,sym==i_s_d())
    
    format_datatable(df)
  })
  
  ############################### Tab Trades #############################
  all_trades = reactiveVal()
  observeEvent(input$loadTrades, {
                all_trades(as.data.frame(read_delim(file=paste0(NewTrading,"Trades.csv"), 
                                                     col_names=TRUE,delim=";",
                                                    locale=locale(date_names="en",decimal_mark=".",
                                                    grouping_mark=" ",encoding="UTF-8"))))
                ##### Update with values from the file: takes all Trade Nr on both accounts and only dates from current account
                TradeNr=as.numeric(all_trades()$TradeNr)
                data=filter(all_trades(),Account==input$account)
                TradeDate=dmy(data$TradeDate)
                
                max=max(TradeNr,na.rm = T)
                updateSliderInput(inputId = "init_nr",min=1,max=max,value=c(max-100,max))
                
                date_min=min(TradeDate,na.rm=T)
                date_max=max(TradeDate,na.rm=T)
                
                updateSliderInput(inputId = "init_date",min=date_min,max=date_max,value=c(date_max-100,date_max))
  })
  
  max_tradenr = reactive({
    max(dis_all_trades()$TradeNr)
  })
  
  ##### !!!! This will erase all trades and save everything again
  observeEvent(input$saveTrades, {
               file.copy(from=paste0(NewTrading,"Trades.csv"),
                         to=paste0(NewTrading,"Trades-old.csv"),overwrite = T)
               write.table(all_trades(),file=paste0(NewTrading,"Trades.csv"),append=F,
                           col.names=TRUE,row.names=FALSE,sep=";",dec=".",quote=TRUE)
  })
  
  ### 20 fields in Trades.csv file
  trade_fields=c("TradeNr","Account","TradeDate","Thème","Idée",
                 "Instrument","Ssjacent","Pos","Prix","Comm.",
                 "Total","Exp.Date","Nbj","Risk","R",
                 "Reward","PnL","Statut","Currency","Commentaires")
  #### 6 fields masked => only 13 displayed + Currency field special case
  masked_trade_fields=c(2,10,13:16)
  
  ### Prepare data for displays, keep all columns
  dis_all_trades = reactive({
    data=all_trades()
    if (!is.null(data)) {
      data %<>% mutate(.keep="unused", 
                       Exp.Date=as.Date(dmy(Exp.Date)), TradeNr=as.numeric(TradeNr),
                       TradeDate=as.Date(dmy(TradeDate)),
                       Statut=as.factor(Statut), PnL=as.numeric(PnL), Pos=as.numeric(Pos), Prix=as.numeric(Prix),
                       Comm.=as.numeric(Comm.),Total=as.numeric(Total))  
    }
  })

  
  #### All trades filtered according to account
  #### Retain only 12 fields worth to be displayed, discard others
  #### Contains only trades to be displayed according to search criteria: symbol, trade date and trade number
  #### Removes also Ssjacent from selection
  sub_all_trades = reactive({
    data = dis_all_trades()
    if (!is.null(data)) {
      data %<>% filter(Account==input$account) %>%
        select(all_of(trade_fields[-masked_trade_fields]))
      
      #### To reorder columns in the display order
      data %<>% select(TradeNr,TradeDate, Ssjacent, Statut, PnL, `Thème`,
                       Instrument,Pos,Prix, Total,  `Idée`,
                       Commentaires,Currency)
      
      if (i_s_d() != "All")
        data = data %>% filter(Ssjacent==i_s_d()) %>% select(-Ssjacent)
      
      data %<>% filter((TradeDate >= ymd(input$init_date[1]) &
                          (TradeDate <= ymd(input$init_date[2]))),
                       ((TradeNr >=input$init_nr[1]) &
                          (TradeNr <=input$init_nr[2])))
      data
    }
  })
  
  #### Displays all sub_all_trades trades in datatable format
  #### Have Idée and Commentaires a bit wider than the other fields
  output$trades = renderDT({
    data = sub_all_trades()
    if (!is.null(data)) {
      data$Prix=currency_format(data$Prix,data$Currency)
      data$Total=currency_format(data$Total,data$Currency)
      data$PnL=currency_format(data$PnL,data$Currency)
      data$Currency=NULL
      
      datatable(
        data,
        filter = 'top',
        options = list(paging = TRUE, searching=TRUE,ordering = TRUE,
                       autoWidth = TRUE, scrollX=TRUE,
          columnDefs=list(
            list(targets = c("Idée","Commentaires"),className = 'dt-left',width='200px'),
            list(targets = "Instrument",width='150px'),
            list(targets = "Total",width='100px'),
            list(targets = "PnL",width='60px'),
            
            list(targets = c("Idée","Commentaires"),
              render = JS('function(data, type, full) {
                  function formatColumn(data) {
                    return data.replace(/\\n/g, "<br>");
                  }
                  return $("<div/>").html(data).text();
                }')
            )
        )),
        rownames = FALSE
      )  %>%
        formatStyle(
          c("Idée","Commentaires"),
          whiteSpace= "pre-wrap")
    }
  })
  
  #### Computes and displays PnL stats according to datatable output$trades possibly filtered by user
  output$PnL = renderTable({
    data = sub_all_trades()
    if (!is.null(data)) {
      PnL=as.numeric(data[input$trades_rows_all, "PnL"])
      PnL_tot=sum(PnL,na.rm = TRUE)
      PnL_tot=label_dollar(accuracy=0.01,prefix="",suffix="$")(PnL_tot)
      PnL_mean=mean(PnL,na.rm = TRUE)
      PnL_mean=label_dollar(accuracy=0.01,prefix="",suffix="$")(PnL_mean)
      PnL_sd=sd(PnL,na.rm = TRUE)
      PnL_sd=label_dollar(accuracy=0.01,prefix="",suffix="$")(PnL_sd)
      
      data.frame(Total=PnL_tot,Mean=PnL_mean,SD=PnL_sd)
    }
  })
  
  ############################### Tab New Trades #############################
  business_lines = c("OFI","Perso","Erreur","BPT","BOT","CS")
  actionUser=reactiveVal()
  
  ### Loads new_trade data from file generated by readTrades, and using as input file provided by user 
  ### THis uses reactive Value and not reactive expression so it can be later updated
  ### File filled in data is the following:
  ### TradeNr;Account;TradeDate;Theme;Idee;Description;Ssjacent;Pos;Prix;Comm.;Total;Exp.Date;Nbj;
  ###    Risk;R;Reward;PnL;Statut;Curr.;Commentaires
  new_load_trade_data= reactiveVal()
  observeEvent(input$file, {
    f=input$file
    new_load_trade_data(readTrades(f$datapath,input$account))
  })

  ### Filter new loaded trade data according to symbol
  ### Filter also to status so that when in status "Ouvert", "Ajusté" or "Fermé" it is not displayed any more
  ### But do not remove any field
  new_trade_data = reactive({
    data=new_load_trade_data()
    if (!is.null(data)) {
      df=as.data.frame(data)
      if (i_s_d() != "All") df %<>% filter(Ssjacent==i_s_d())
      filter(df,(df$Statut != "Ouvert") & (df$Statut != "Fermé") & (df$Statut != "Ajusté" )) 
    }
  })
  
  
  # Show open or update modal window when action button is clicked.
  observeEvent(input$open, {print("Open pressed"); actionUser("Open"); showModalOpen()}) 
  observeEvent(input$adjust, {print("Adjust pressed"); actionUser("Adjust");showModalUpdate()}) 
  observeEvent(input$close, {print("Close pressed"); actionUser("Close");showModalUpdate()}) 
  observeEvent(input$remove, {print("Remove pressed"); actionUser("Remove");update_rows_newtrades()}) 
  
  # Return the UI for a modal dialog with data selection input for OPENING a new trade
  #. If 'failed' is TRUE, then display a message that the entered value was invalid.
  showModalOpen <- function(failed = FALSE,error_message="") {
       showModal(
         modalDialog(
              textAreaInput("idee", label = "Idée de trade", value = "",width="400px",height = "200px",
                            placeholder = 'Text to explain the logic - setup and opportunity'),
              selectInput("theme","Thème", choices=business_lines,selected="OFI"),
              if (failed) 
                div(tags$b(error_message, style = "color: red;")),
              
              footer = tagList(
                modalButton("Cancel"),
                actionButton("ModalOpen_ok", "OK"))
            )
       )
  }
 
  # Return the UI for a modal dialog with data selection input for UPDATING (CLOSING, ADJUSTING) a trade. 
  # If 'failed' is TRUE, then display a message that the entered value was invalid.
  showModalUpdate <- function(failed = FALSE,error_message="") {
    showModal(
      modalDialog(
          numericInput("tradenr_to_upd",label="Trade nr: ",min=1, max=max_tradenr(),value=0),
          textAreaInput("comment", label = "Commentaire", value = "",width="400px",height = "200px",
                    placeholder = 'Comment to the trade and why adjust or close - opportunity'),
          if (failed) 
                div(tags$b(error_message, style = "color: red;")),
      
      footer = tagList(
        modalButton("Cancel"),
        actionButton("ModalUpdate_ok", "OK"))
      )
    )
  }

  observeEvent(input$ModalOpen_ok, {
    #### Some failure conditions - for instance no selection of lines made by the user, etc...
    if (nrow(new_trade_data()[input$newtrades_rows_selected,])==0) {
      showModalOpen(failed=TRUE,error_message="Please select lines in new trades table!!")
    }

    else if (length(unique(new_trade_data()[input$newtrades_rows_selected,"Ssjacent"]))!=1) {
      showModalUpdate(failed=TRUE,error_message="Please select lines with the same symbol!!")
    }
    
    else {
      update_trades(account=input$account,theme=input$theme,trade_nr = max_tradenr()+1,
                    text=input$idee,action=actionUser(),data=new_trade_data()[input$newtrades_rows_selected,])
      update_rows_newtrades()
      removeModal()
    }
  })
  observeEvent(input$ModalUpdate_ok, {
    #### Some failure conditions - for instance no selection of lines made by the user, etc...
    if (nrow(new_trade_data()[input$newtrades_rows_selected,])==0) {
      showModalUpdate(failed=TRUE,error_message="Please select lines in new trades table!!")
    }

    else if (length(unique(new_trade_data()[input$newtrades_rows_selected,"Ssjacent"]))!=1) {
      showModalUpdate(failed=TRUE,error_message="Please select lines with the same symbol!!")
    }
    
    else {
      update_trades(account=input$account, text=input$comment,trade_nr=input$tradenr_to_upd,action=actionUser(),
                                     data=new_trade_data()[input$newtrades_rows_selected,])
      update_rows_newtrades()
      removeModal()
    }
  })
  
  #### Update all lines from initial new load data
  update_rows_newtrades = function() {
    initial_new_data= new_load_trade_data()
    ## selected_data= new_trade_data()[input$newtrades_rows_selected,]
    selected_indices_data = input$newtrades_rows_selected
    column_subset=c("Account","TradeDate","Description","Ssjacent","Prix","Pos")
    
    for (i in selected_indices_data) {
            indices=apply(initial_new_data, 1, function(row) {
                        all(row[column_subset]==new_trade_data()[i,column_subset])
                      })
            cat(paste0("Update_rows_newtrades i ",i,   "   Indices: ",indices,"\n"))
            initial_new_data[indices,"Statut"]=  switch(actionUser(),
                                                    "Open" = "Ouvert",
                                                    "Close" = "Fermé",
                                                    "Adjust" = "Ajusté",
                                                    "Remove" = "Fermé",  ### Just to suppress the trade from display)
                                                    NA)
    }
    new_load_trade_data(initial_new_data)
  }

  ### dis_trade_fields=trade_fields[-c(13:16)]
  
  insert_new_trade = function(account,trade_nr, theme,text,data) {
    
    ### Contained in data are the following fields:
    ### TradeDate;Description;Ssjacent;Pos;Prix;Comm.;Total;Exp.Date;Nbj;Statut;Curr.
    new_trade = data
    new_trade$TradeNr=trade_nr
    new_trade$Account=account
    new_trade$`Thème`=theme
    new_trade$`Idée`="" ## Create the field for all
    new_trade[1,"Idée"]=text  ### Idée only on first element
    new_trade %<>% rename(Instrument=Description)
    new_trade[c("Risk","R","Reward")]=""
    new_trade$PnL=""
    new_trade$Statut="Ouvert"
    new_trade$Currency=data$Curr.
    print(new_trade)
    
    new_trade %<>% select(all_of(trade_fields))
 
    #### Update all_trades by adding new_trade
    all_trades(rbind.data.frame(all_trades(),new_trade))
    
    ### No automatic save
    # write.table(new_trade,file="C:\\Users\\aldoh\\Documents\\NewTrading\\Trades.csv",append=TRUE,
    #             col.names=FALSE,row.names=FALSE,sep=";",dec=".",quote=TRUE)
  }
  
  adjust_trade = function(account,trade_nr,text,data) {
    ### First retrieve initial trade
    initial_trade = filter(all_trades(),TradeNr==trade_nr)
    
    end_trade = data
    end_trade$TradeNr=trade_nr
    end_trade$Account=account
    end_trade$`Thème`=unique(initial_trade$`Thème`) ### Same theme for all legs of initial trade
    end_trade$`Idée`=""
    end_trade %<>% rename(Instrument=Description)
    end_trade[c("Risk","R","Reward")]=""
    end_trade$PnL="" ## Create the field for all legs of end_trade
    end_trade$Statut="Ajusté";
    end_trade$Currency=data$Curr.
    end_trade$Commentaires="" ## Create the field for all legs of end_trade
    end_trade[1,"Commentaires"]=text
    
    print("Adjust trade:")
    print(end_trade)
    end_trade %<>% select(all_of(trade_fields))
    
    
    #### In all trades 1/ update Statut field 2/ 1/ add end_trade- so it is ready to Save
    df = all_trades()
    df$Statut=ifelse(df$TradeNr==trade_nr,"Ajusté",df$Statut)
    df=rbind(df,end_trade)
    all_trades(df)
  }
  
  close_trade = function(account,trade_nr,text,data) {
    ### First retrieve initial trade
    initial_trade = filter(all_trades(),TradeNr==trade_nr)
    
    end_trade = data
    end_trade$TradeNr=trade_nr
    end_trade$Account=account
    end_trade$`Thème`=unique(initial_trade$`Thème`) ### Same theme for all legs of initial trade
    end_trade$`Idée`=""
    end_trade %<>% rename(Instrument=Description)
    end_trade[c("Risk","R","Reward")]=""
    PnL=sum(as.numeric(initial_trade$Total),as.numeric(end_trade$Total),na.rm = T)
    end_trade$PnL="" ## Create the field for all legs of end_trade
    end_trade[1,"PnL"] = PnL
    end_trade$Statut="Fermé";
    end_trade$Currency=data$Curr.
    end_trade$Commentaires="" ## Create the field for all legs of end_trade
    end_trade[1,"Commentaires"]=text
    print("Close trade:")
    print(end_trade)
    
    end_trade %<>% select(all_of(trade_fields))

    
    #### In all trades 1/ update Statut field 2/ 1/ add end_trade- so it is ready to Save
    df = all_trades()
    df$Statut=ifelse(df$TradeNr==trade_nr,"Fermé",df$Statut)
    df=rbind(df,end_trade)
    all_trades(df)
  }
  
  #### Switch function to insert/adjust/close functions
  update_trades = function(account,theme=NA,trade_nr=0, text,action,data) {
    switch(action,
           "Open" = insert_new_trade(account,trade_nr,theme,text,data),
           "Adjust"= adjust_trade(account,trade_nr,text,data),
           "Close" = close_trade(account,trade_nr,text,data),
           stop("Error in update_trades funtion!")
    )
    
    print(paste("theme:",theme))
    print(paste("trade_nr:",trade_nr))
    print(paste("idée/comment:",text))
    print(paste("action:",action))
    print(paste("max trade nr",max_tradenr()))
  }
  
  output$newtrades=renderDT({
    data =  new_trade_data()
    
    if (!is.null(data)) {
      data %<>% mutate(.keep = "unused", Pos=as.numeric(Pos),Prix=as.numeric(Prix),Comm.=as.numeric(Comm.),
                       Total=as.numeric(Total))
      
      data$Prix=currency_format(data$Prix,data$Curr.)
      data$Comm.=currency_format(data$Comm.,data$Curr.)
      data$Total=currency_format(data$Total,data$Curr.)
      data %<>% select(TradeDate,Instrument=Description,Ssjacent,Statut, Pos,Prix,Comm.,Total)
      
      dt=datatable(
        data,
        options = list(
          paging = T,
          ordering = TRUE,
          autoWidth = TRUE,
          scrollX=TRUE,
          columnDefs=list(
            list(targets = "Instrument",width='150px'),
            list(targets = c("Prix","Total"),width='100px'),
            list(targets = "Pos",width='20px')
          )
        ),
        rownames = FALSE
      )
    }
  })
}