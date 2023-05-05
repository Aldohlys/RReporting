### Creation date 02.12.2022
library(shiny)
library(dplyr)
library(DescTools) ## For GCD
library(quantmod)  ### Retrieve last market values
#### library(shinyWidgets) ## for radioGroupButtons widget
library(lubridate) #### For today() function
library(DT)  ### Data table
library(scales) ### label_percent function
library(readr) ## for read_delim function
library(magrittr) ### for %<>% pipe



#Returns the amount values formatted with their respective currency sign, based on the currency argument
## Amounts are rounded to 0.01
### Results are considered as strings
currency_format = function(amount,currency){
  
  euro <- label_dollar(
    prefix = "",
    suffix = " \u20ac",
    big.mark=" ",
    accuracy=0.01
  )
  chf <- label_dollar(
    prefix = "",
    suffix = " CHF",
    big.mark=" ",
    accuracy=0.01
  )
  dollar <- label_dollar(
    prefix = "",
    suffix = " $",
    big.mark=" ",
    accuracy=0.01
  )
  
  dplyr::case_match(currency, 
             "EUR" ~ ifelse (!is.na(amount), euro(amount), NA),
             "CHF"~  ifelse (!is.na(amount), chf(amount), NA),
             "USD"~  ifelse (!is.na(amount), dollar(amount), NA))
}


getLastTickerData = function(ticker) {
  if (is.null(ticker) |
      ticker %in% c("","All")) return(list(last=NA,change=NA))
  
  ### Retrieve data from Yahoo Finance - no need to launch IBKR TWS
  ### Get last price and last change (J/J-1)
  tryCatch({
    ticker=getSymbols(ticker,auto.assign=FALSE,from=today()-10,warnings=FALSE) ## Case Tuesday morning and US market not yet opened + Monday and Friday were off -> Get Wed and THur data
    names(ticker)[length(names(ticker))]="Adjusted" ### Last column is "ticker.Adjusted" -> to be renamed as Adjusted
    last_data=as.numeric(ticker[[nrow(ticker),"Adjusted"]])
    p_last_data=as.numeric(ticker[[nrow(ticker)-1,"Adjusted"]])
    return(list(
      last=round(last_data,2),
      change=label_percent(accuracy=0.01)(last_data/p_last_data-1)
    ))
  }, error = function(e) {
    print(paste("Error:", e))
    return(list(last="Non disponible",change=NA))
  })
}

lastSPY=getLastTickerData("SPY")  ### Mkt value


read_trade= function(file){
  print(file)
  
  ### check.names change "Put/Call" column name into "Put.Call" which is easier to handle
  trade=read.csv(file,check.names = TRUE)
  
  ### First group all sub-trades on different exchanges but with same underlying,strike, exp. date, quantity sign (in case buy and sell the same day)
  # and rights into a single position
  trade = trade %>% group_by(Date=as.Date(ymd_hms(`Date.Time`)),UnderlyingSymbol,Strike,Description,Put.Call,
                             Exp.Date=as.Date(ymd(Expiry)),CurrencyPrimary,Sign=sign(Quantity)) %>% 
    summarize(Pos=sum(Quantity),Prix=abs(weighted.mean(sign(Quantity)*Price,abs(Quantity))),`Comm.`=-sum(Commission),Proceeds=sum(Proceeds),
              Total=sum(Proceeds)+sum(Commission),
              Code=paste0(unique(as.list(strsplit(gsub("[^[:alnum:] ]", "",Code),"")[[1]])),collapse=""))
  
  trade = trade %>% 
    select(Date,Ssjacent=UnderlyingSymbol,Description,Strike,Put.Call,Exp.Date,Pos,Prix,#Proceeds,
           Comm.,Total,Code,Currency=CurrencyPrimary)
  
  return(trade)
  
}

### This function takes as input an IBKR-generated Flex Query CSV file 
### -> go to Account Management Home / Flex Queries / TradeLive and execute with CSV and time period arguments
### THen it processes this IBKR CSV file and produces one file names "Report-<<account type>>-date of today.csv"
### Data written to the file is also returned by the function readTrades

readTrades= function(file,typeAccount){
  #### Convert data coming from IBKR into data more readable and summarized
  #### Store resulting data
  
  trade=read_trade(file)
  #### Note: Using read.table will not work as it expects row numbers - and therefore 1 column less in the header
  
  ### Treat case where a combo exists
  simpletrade= trade %>% group_by(Date,Ssjacent,Exp.Date,Put.Call,Currency,Code) %>% filter(n() == 1)
  combotrade= trade %>% group_by(Date,Ssjacent,Exp.Date,Put.Call,Currency,Code) %>% filter(n() != 1)
  if (nrow(combotrade) != 0) {
    combotrade = combotrade %>%
      summarize(ComboPos=GCD(Pos),
                ComboPrix= sum(Prix*Pos)/GCD(Pos),
                ComboStrike=paste0(Strike,collapse="/"),
                ComboPut.Call=paste0(Pos/GCD(Pos),Put.Call,collapse="/"),
                #ComboDescription=paste(Ssjacent,"Vertical Spread",Exp.Date,ComboStrike,ComboPut.Call),
                #ComboProceeds=sum(Proceeds),
                ComboComm.=sum(Comm.),
                ComboTotal=sum(Total))
    
    combotrade=mutate(combotrade,ComboDescription=paste(Ssjacent,"Vertical Spread",
                                             format(Exp.Date,"%d.%m.%Y"),ComboStrike,ComboPut.Call))
    
    
    combotrade=ungroup(combotrade) %>% select(Date,Ssjacent,Description=ComboDescription,Exp.Date,
                                     Currency,Code,Pos=ComboPos,Prix=ComboPrix,
                                     Comm.=ComboComm.,Total=ComboTotal)
    simpletrade=ungroup(simpletrade) %>% select(Date,Ssjacent,Description,Exp.Date,
                                                Currency,Code,Pos,Prix,
                                                Comm.,Total)
    trade=rbind(simpletrade,combotrade)
  }
  
  trade= trade %>% arrange(Date) %>% 
    mutate(Nbj=Exp.Date-today())  %>% 
    mutate(Date=format(Date,"%d.%m.%Y"),Exp.Date=format(Exp.Date,"%d.%m.%Y"))
  
  trade= cbind(TradeNr=0, Account=typeAccount,TradeDate=trade$Date,Theme="",Idee="",
               Description=trade$Description,Ssjacent=trade$Ssjacent,
               Pos=trade$Pos,Prix=trade$Prix,Comm.=trade$Comm.,
               Total=trade$Total,Exp.Date=trade$Exp.Date,Nbj=trade$Nbj,
               Risk="",R="",Reward="",PnL="",Statut=trade$Code,
               Curr.=trade$Currency,Commentaires="")
  
  write.table(x=as.data.frame(trade),file=paste0(NewTrading,"Report-",
                                                 typeAccount,"-",format(today(),"%d.%m.%Y"),".csv",
                                                 collapse=""),
              row.names=FALSE,sep=";",dec=".",quote=FALSE)
  return(trade)
}
