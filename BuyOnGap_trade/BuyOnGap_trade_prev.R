#############################################################################
#
#    Buy On Gap Trading implementation
#    
#    these code execute before the market open, data preparation
#
#############################################################################


Sys.setenv(TZ='America/New_York')
library("quantmod")
library("PerformanceAnalytics") #Load the PerformanceAnalytics library
source('/home/jhleong/dev/R/lib/libtrade.R');

start_time <- Sys.time();


stdLookback <- 66
stdMultiple <- 0.5



sym_list <- sp500index.components();
# remove Symbol with character "^", yahoo don't seem to have this data, google use "-"
sym_list <- sym_list[ !grepl("\\^", sym_list)]
sym_list <- gsub("\\.", "-", sym_list)

load(file='/home/jhleong/dev/R/buy_on_gap/BuyOnGap_trade/stockToAvoidIn2013.Rdata');

# exclude stock with marked avoid
sym_list <- setdiff(sym_list, stockToAvoidIn2013);

cat('S&P 500 - Number of stock need to download:', length(sym_list),'\n');

from_date <- as.Date(trunc(Sys.time(),"days")) - 365;  # 1 year

stockData <- new.env()

getSymbols_par(sym_list, stockData);

# second run of downloading from Yahoo
sym_list2<- setdiff(sym_list, ls(stockData));
if(length(sym_list2) > 0){
Sys.sleep(5);
getSymbols_par(sym_list2, stockData);
}

# third run of downloading from Yahoo
sym_list3<- setdiff(sym_list, ls(stockData));
if(length(sym_list3) > 0){
Sys.sleep(5);
getSymbols_par(sym_list3, stockData);
}

sym_list4 <- setdiff(sym_list, ls(stockData));
if(length(sym_list4) > 0)
{
  cat('S&P 500 - There is ', length(sym_list4), ' symbol(s) not able to download from Yahoo\n');
  print(sym_list4);
}


filename <- paste("/data/BuyOnGap/stockData/", as.character(trunc(Sys.time(), "days")), "_sp500_stockData.Rdata", sep = "")
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
  if((nrow(sData) > stdLookback) & (sData[1,1] >= 1.0))
  {
    # the latest downloaded quote can not older then 1 week. avoid missing data from yahoo.
    if( as.numeric(difftime(Sys.Date(), index(last(sData)), units='days')) < 7){
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
    }else {
      cat('Symbol: ',symbolsLst[i], ' latest quote older then 1 week.\n')
    }
    
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
filename <- paste("/data/BuyOnGap/stockData/", as.character(trunc(Sys.time(), "days")), "_sp500_condMat.Rdata", sep = "")
save(conditionMatrix, symbolsLst, file=filename);


###########################################################################################
#
#     S&P 600 Small Cap scenerio 
#
###########################################################################################

sp600_sym_list <- sp600small.components();
# remove Symbol with character "^", yahoo don't seem to have this data, google use "-"
sp600_sym_list <- sp600_sym_list[ !grepl("\\^", sp600_sym_list)]
sp600_sym_list <- gsub("\\.", "-", sp600_sym_list)

#load(file='/home/jhleong/dev/R/buy_on_gap/BuyOnGap_trade/stockToAvoidIn2013.Rdata');
# exclude stock with marked avoid
#sp600_sym_list <- setdiff(sp600_sym_list, stockToAvoidIn2013);

cat('S&P 600 - number of stock need to download:', length(sp600_sym_list),'\n');

from_date <- as.Date(trunc(Sys.time(),"days")) - 5;

stockData_sp600 <- new.env()

library(quantmod)

getSymbols_par(sp600_sym_list, stockData_sp600);

# second run of downloading from Yahoo
sp600_sym_list2<- setdiff(sp600_sym_list, ls(stockData_sp600));
if(length(sp600_sym_list2) > 0){
Sys.sleep(5);
getSymbols_par(sp600_sym_list2, stockData_sp600);
}

# third run of downloading from Yahoo
sp600_sym_list3<- setdiff(sp600_sym_list, ls(stockData_sp600));
if(length(sp600_sym_list3) > 0){
Sys.sleep(5);
getSymbols_par(sp600_sym_list3, stockData_sp600);
}

sp600_sym_list4 <- setdiff(sp600_sym_list, ls(stockData_sp600));
if(length(sp600_sym_list4) > 0)
{
  cat('S&P 600 - There is ', length(sp600_sym_list4), ' symbol(s) not able to download from Yahoo\n');
  print(sp600_sym_list4);
}

sym_name <- vector();
close_price <- vector();
low_price <- vector();

sp600_sym <- ls(stockData_sp600);

for(i in sp600_sym) 
{
  sData <- stockData_sp600[[i]]
  oldColNames <- names(sData)
  colnames(sData) <- c("S.Open","S.High","S.Low","S.Close","S.Volume","S.Adjusted")
  sData = adjustOHLC(sData, use.Adjusted=T) 

  # the latest downloaded quote can not older then 1 week. avoid missing data from yahoo.
  if( as.numeric(difftime(Sys.Date(), index(last(sData)), units='days')) < 7){
  sym_name <- append(sym_name, i);
  close_price <- append(close_price, as.numeric(last(Cl(sData))));
  low_price <- append(low_price, as.numeric(last(Lo(sData))));
  }else{
    cat('Symbol: ',i , ' latest quote older then 1 week.\n')
  }
  
  colnames(sData) <- oldColNames
  stockData_sp600[[i]] <- sData
}

sp600_prev_day_price <- data.frame(Symbol=sym_name, Prev_Close=close_price, Prev_Low=low_price, 
                               stringsAsFactors=F);


filename <- paste("/data/BuyOnGap/stockData/", as.character(trunc(Sys.time(), "days")), "_sp600_stockData.Rdata", sep = "")
save(list = ls(stockData_sp600), file=filename, envir=stockData_sp600)

filename <- paste("/data/BuyOnGap/stockData/", as.character(trunc(Sys.time(), "days")), "_sp600_prev_close.Rdata", sep = "")
save(sp600_prev_day_price, sp600_sym, file=filename);


cat(paste0('[',Sys.time(),']',' The process took: ', Sys.time() - start_time,'\n'));

