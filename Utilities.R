##########  Trades.CSV file transformation



data = read_delim(file=paste0(NewTrading,"Trades.csv"),
                  col_names=TRUE,delim=";",
                  locale=locale(date_names="en",decimal_mark=".",
                                grouping_mark=" ",encoding="UTF-8"))
# data %<>% mutate(Remarques= if_else( is.na(Idée), Commentaires,
#                            if_else(is.na(Commentaires), Idée, paste(Idée,Commentaires))))
# data$Commentaires=NULL
# data$Idée=NULL
# data$R=NULL
# data$Nbj=NULL

file.copy(from=paste0(NewTrading,"Trades.csv"),
          to=paste0(NewTrading,"Trades-old.csv"),overwrite = T)

write_delim(data,file=paste0(NewTrading,"Trades.csv"),append=F,
            delim=";")

write.table(data,file=paste0(NewTrading,"Trades.csv"),append=F,row.names=F,
            sep=";",dec=".",quote=T)
