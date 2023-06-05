### Creation date 02.12.2022
library(shiny)
library(lubridate) #### For today() function
library(dplyr)
library(tibble)
library(DescTools) ## For GCD
library(quantmod)  ### Retrieve last market values
#### library(shinyWidgets) ## for radioGroupButtons widget
library(DT)  ### Data table
library(scales) ### label_percent function
library(readr) ## for read_delim function
library(magrittr) ### for %<>% pipe
library(reticulate)


#Returns the amount values formatted with their respective currency sign, based on the currency argument
## Amounts are rounded to 0.01

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

  case_match(currency,
             "EUR" ~ ifelse (!is.na(amount), euro(amount), NA),
             "CHF"~  ifelse (!is.na(amount), chf(amount), NA),
             "USD"~  ifelse (!is.na(amount), dollar(amount), NA))
}

reticulate::py_run_file("C:/Users/aldoh/Documents/R/Repo/RAnalysis/getContractValue.py")
EUR = double()
CHF= double()
EUR = tryCatch(
  py$getCurrencyPairValue("EURUSD",reqType=2),
  error=function(e) {
    cat("\nNo value for EUR-USD pair\nEnter new value: ")
    scan(what=double(),nmax=1)
  }
)

CHF = tryCatch(
  py$getCurrencyPairValue("CHFUSD",reqType=2),
  error=function(e) {
      cat("No value for CHF-USD pair\nEnter new value: ")
    scan(what=double(),nmax=1)
  }
)

USD=as.double(1)


#### takes 2 vectors (one double, one string) as input and compute sum and mean for one or multiple currencies
### Returns strings
compute_stats = function(amount,currency) {

  if (length(unique(currency))==1) { ### This specific case allows result to be displayed in EUR or CHF according to symbol
    currency=unique(currency)
    tot=sum(amount,na.rm = TRUE)
    tot=currency_format(tot,currency)
    avg=mean(amount,na.rm = TRUE)
    avg=currency_format(avg,currency)
  }

  else { ### General case -multiple currencies- may still occur and needs to be taken into consideration
    ### Convert PnL into USD if originally in CHF or EUR
    amount = case_match(currency, "EUR" ~ EUR*amount, "CHF"~  CHF*amount, "USD"~  amount)
    tot=sum(amount,na.rm = TRUE)
    tot=currency_format(tot,"USD")
    avg=mean(amount,na.rm = TRUE)
    avg=currency_format(avg,"USD")
  }
  list(sum=tot, mean=avg)
}






################################################################
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

  trade %<>% arrange(Date)

  trade= as.data.frame(cbind(Account=typeAccount,TradeDate=format(trade$Date,"%d.%m.%Y"),
               Description=trade$Description,Ssjacent=trade$Ssjacent, Exp.Date=format(trade$Exp.Date,"%d.%m.%Y"),
               Pos=trade$Pos,Prix=trade$Prix,Comm.=trade$Comm.,
               Total=trade$Total,Statut=trade$Code,
                 Curr.=trade$Currency))

  ### Useful as safety
  write.table(x=trade,file=paste0(NewTrading,"Report-",
                                                 typeAccount,"-",format(today(),"%d.%m.%Y"),".csv",
                                                 collapse=""),
              row.names=FALSE,sep=";",dec=".",quote=FALSE)
  return(trade)
}

################  Display error message

display_error_message = function(error_msg) {
  showModal(modalDialog(
    title = "Error message",
    error_msg,
    easyClose = TRUE,
    footer = NULL
  ))
}

##########################

### Format datatable for Journal entry
### Special treatment for the \n or <br> HTML tag
format_datatable = function(dt) {
  print("format_datatable:")
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



