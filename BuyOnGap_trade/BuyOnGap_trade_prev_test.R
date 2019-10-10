#############################################################################
#
#    Buy On Gap Trading implementation
#    
#    these code execute before the market open, data preparation
#
#############################################################################
Sys.setenv(TZ='EST')
library("quantmod")
library("PerformanceAnalytics") #Load the PerformanceAnalytics library
source('/home/jhleong/dev/R/lib/libtrade.R');

start_time <- Sys.time();

nskip <- 0
stdLookback <- 132
stdMultiple <- 0.5
nStocksBuy <- 10


sym_list <- sp500index.components();
# remove Symbol with character "^", yahoo don't seem to have this data, google use "-"
sym_list <- sym_list[ !grepl("\\^", sym_list)]
sym_list <- gsub("\\.", "-", sym_list)

cat('Number of stock need to download:', length(sym_list),'\n');

trading_date_str <- '2013-02-28'

to_date <- as.Date(trading_date_str);
from_date <- as.Date(from_date_str) - 200;

stockData <- new.env()

library(quantmod)
for(c_sym in sym_list)
{
  try(Sys.sleep(abs(rnorm(1, mean = 0.5, sd =0.2))))
  try(getSymbols(c_sym, from=from_date, to=to_date, src="yahoo", env=stockData, auto.assign=T))
}

# second run of downloading from Yahoo
sym_list2<- setdiff(sym_list, ls(stockData));
for(c_sym in sym_list2)
{
  try(Sys.sleep(abs(rnorm(1, mean = 0.5, sd =0.5))))
  try(getSymbols(c_sym, from=from_date, to=to_date, src="yahoo", env=stockData, auto.assign=T))
}

# third run of downloading from Yahoo
sym_list3<- setdiff(sym_list, ls(stockData));
for(c_sym in sym_list3)
{
  try(Sys.sleep(abs(rnorm(1, mean = 1.0, sd =0.5))))
  try(getSymbols(c_sym, from=from_date, to=to_date, src="yahoo", env=stockData, auto.assign=T))
}

sym_list4 <- setdiff(sym_list, ls(stockData));
if(length(sym_list4) > 0)
{
  cat('There is ', length(sym_list4), ' symbol(s) not able to download from Yahoo\n');
  print(sym_list4);
}

filename <- paste("/home/jhleong/dev/R/buy_on_gap/BuyOnGap_trade/tmp/", trading_date_str, "_sp500_stockData.Rdata", sep = "")
save(list = ls(stockData), file=filename, envir=stockData)

# stockData <- new.env()
# load(file='/data/BuyOnGap/stockData/2013-02-26_sp500_stockData.Rdata', envir=stockData)

for(i in ls(stockData)) 
{
  sData <- stockData[[i]]
  oldColNames <- names(sData)
  colnames(sData) <- c("S.Open","S.High","S.Low","S.Close","S.Volume","S.Adjusted")
  sData = adjustOHLC(sData, use.Adjusted=T) 
  colnames(sData) <- oldColNames
  stockData[[i]] <- sData
}

symbolsLst <- ls(stockData)   
symToExclude <- vector();

conditionMatrix <- NULL;

for (i in 1:length(symbolsLst)) {
  cat("Calculating the returns and standard deviations for stock: ",symbolsLst[i],"\n")
  sData <- eval(parse(text=paste("stockData$\'",symbolsLst[i], '\'',sep=""))) 
  
  # The num of tick must > stdLookback and the Open price must >= 1 dollar
  if((nrow(sData) >= stdLookback) & (sData[1,1] >= 1.0))
  {
    num_tick <- nrow(sData)
    sData <- sData[ (num_tick - stdLookback):num_tick ]
    
    oldColNames <- names(sData)
    colnames(sData) <- c("S.Open","S.High","S.Low","S.Close","S.Volume","S.Adjusted")
  
    roc <- ROC(Cl(sData), type='discrete')
    sd <- runSD(roc, n=stdLookback);
    mean <- runMean(roc, n=stdLookback);
    
    stdClClRet <- mean - stdMultiple*sd    

    conditionMatrix <- rbind( conditionMatrix, 
                              data.frame(Symbol=symbolsLst[i], 
                                         Mean_SD=last(stdClClRet), 
                                         Prev_Low=Lo(last(sData)),
                                         stringsAsFactors=FALSE));
    
  }
  else
  {
    symToExclude <- append(symToExclude, symbolsLst[i])
  }
}
# exclude stock with ticker less then stdLookback
symbolsLst <- setdiff(symbolsLst, symToExclude)

colnames(conditionMatrix) <- c('Symbol','Mean_SD','Prev_Low');
row.names(conditionMatrix) <- NULL
filename <- paste("/home/jhleong/dev/R/buy_on_gap/BuyOnGap_trade/tmp/", trading_date_str, "_sp500_condMat.Rdata", sep = "")
save(conditionMatrix, symbolsLst, file=filename);


cat('The process took: '); Sys.time() - start_time;

# for (i in 1:length(symbolsLst)) {
#   cat("Processing stock: ",symbolsLst[i]," to the returns table\n")
#    sDataRET <- eval(parse(text=paste("stockData$\'",
# #                                     symbolsLst[i], '\'',"[,\"",symbolsLst[i],
# #                                     ".LowOpenRet\"]",sep="")))
#   sDataSTD <- eval(parse(text=paste("stockData$\'",
#                                     symbolsLst[i], '\'',"[,\"",symbolsLst[i],
#                                     ".StdClClRet\"]",sep="")))
# #   sDataDAYRET <- eval(parse(text=paste("stockData$\'",
# #                                        symbolsLst[i], '\'',"[,\"",symbolsLst[i],
# #                                        ".DayClOpRet\"]",sep="")))
#   if(i == 1){
#     retMat <- sDataRET
#     stdMat <- sDataSTD
# #    dayretMat <- sDataDAYRET
#   } else {
#     retMat <- cbind(retMat,sDataRET)
#     stdMat <- cbind(stdMat,sDataSTD)
# #    dayretMat <- cbind(dayretMat,sDataDAYRET)
#   }
# }

# openPrice <- get_open_price_IB();
# 
# # nStockTradeVec <- NULL;
# # 
# #   conditionTwo <- retMat #copying the structure and data, only really want the structure
# #   conditionTwo[,] <- 0 #set all the values to 0
# #   conditionTwo <- retMat - stdMat #If ClOp ret is < StdRet tmp will be < 1
# #   conditionTwo <- ifelse(as.matrix(conditionTwo) < 0, 1, 0)
# 
# matched_condition <- NULL;
# 
# for(i in 1:nrow(conditionMatrix))
# {
#   sym <- conditionMatrix[i,'Symbol'];
#   op <- openPrice[Symbol == sym, 'Open'];
#   prev_low <- conditionMatrix[i,'Prev_Low'];
#   LoOpenRet <- (op / prev_low) - 1;
#   
#   if(LoOpenRet <  conditionMatrix[i,'Mean_SD'] & LoOpenRet < 0.0 )
#   {
#     matched_cond <- rbind( matched_cond, data.frame(Symbol=sym, LoOpenRet=LoOepnRet));
#   }
#   
# }
# 
# orderindex <- order(matched_cond$LoOpenRet, decreasing=FALSE);
# orderindex <- orderindex[(nskip+1):(nStocksBuy+nskip)]; #want the smallest n (nStocksBuy) stocks
# matched_cond2 <- matched_cond[,orderindex];
# 
# # Buy this stock  matched_cond2$Symbol




#   conditionOne <- retMat #copying the structure and data, only really want the structure
#   conditionOne[,] <- 0 #set all the values to 0
#   
#   ret_GT_SD <- retMat * conditionTwo
#   # exlcude retMat with positive ret which meet the condition
#   ret_GT_SD <- ifelse(as.matrix(ret_GT_SD) < 0, ret_GT_SD, NA)
  
#   for (i in 1:length(ret_GT_SD[,1])){
#     orderindex <- order((ret_GT_SD[i,]),decreasing=FALSE)  #order row entries smallest to largest      
#     
#     ################### skip the n most negative return ######################
#     
#     orderindex <- orderindex[(nskip+1):(nStocksBuy+nskip)] #want the smallest n (nStocksBuy) stocks
#     
#     ################### skip the n most negative return ######################      
#     
#     conditionOne[i,orderindex] <- 1 #1 Flag indicates entry is one of the nth smallest
#   }
  
#   conditionsMet <- conditionOne * conditionTwo
#   colnames(conditionsMet) <- gsub(".LowOpenRet","",names(conditionsMet))
#   conditionsMet[is.na(conditionsMet)] <- 0
#   
#   tradeMat <- dayretMat
#   colnames(tradeMat) <- gsub(".DayClOpRet","",names(tradeMat))
#   tradeMat <- tradeMat * conditionsMet
#   tradeMat[is.na(tradeMat)] <- 0
#   tradeVec <- as.data.frame(apply(tradeMat, 1, function(x) sum(x, na.rm=TRUE)) / 
#                               apply(conditionsMet, 1, function(x) sum(x, na.rm=TRUE)))
#   colnames(tradeVec) <- paste("BuyOnGap.", nStocksBuy, sep="" )
#   tradeVec[is.nan(tradeVec[,1]),1] <- 0 #Didnt make or loose anything on this day
#   
#   print(paste('Processed nskip=',nskip,' SD Period=',stdLookback,' SD=',stdMultiple,' nStocksBuy=', nStocksBuy, sep=''))
#   
#   if(nStocksBuy == 10)
#   {
#     conditionsMet_SD_sp500_trade[[as.character(nskip)]][[as.character(stdLookback)]][[as.character(stdMultiple)]] <- conditionsMet;
#     tradeMat_SD_sp500_trade[[as.character(nskip)]][[as.character(stdLookback)]][[as.character(stdMultiple)]] <- tradeMat;
#   }
#   
#   if(iloop == 0)
#     nStockTradeVec <- tradeVec
#   else
#     nStockTradeVec <- cbind(nStockTradeVec,tradeVec )
#   
#   iloop <- iloop + 1;
# 
# 
# nStockTradeVec <- as.xts(nStockTradeVec)
# nStockTradeVec <- cbind(nStockTradeVec,indexRet )
# nStockTradeVec_SD_sp500_trade[[as.character(nskip)]][[as.character(stdLookback)]][[as.character(stdMultiple)]] <- nStockTradeVec
# 
# save(nStockTradeVec_SD_sp500_trade, conditionsMet_SD_sp500_trade,tradeMat_SD_sp500_trade,
#      file='/home/jhleong/dev/R/buy_on_gap/result_Rdata/nStockTradeVec_SD_sp500_trade.Rdata' );
# rm(nStockTradeVec_SD_sp500_trade, conditionsMet_SD_sp500_trade,tradeMat_SD_sp500_trade);
# gc();
