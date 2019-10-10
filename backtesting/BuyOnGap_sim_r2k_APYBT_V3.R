#############################################################################
#
#
#    find previous year stocks with significant frequent appearance with losing trade. 
#
#    test using Russell 2000 stocks
#    
#     1) The Gap must be wider then 2.5%
#
#############################################################################


library("quantmod")
library("PerformanceAnalytics") #Load the PerformanceAnalytics library
source('/home/jhleong/dev/R/lib/libtrade.R');

getStockToAvoid <- function(conditionsMet, tradeMat)
{
  source('/home/jhleong/dev/R/lib/libtrade.R');
  numTradePerStock <- buy_on_gap.numTradePerStock(conditionsMet, tradeMat);
  # Top 25%
  topQuarter <- floor(nrow(numTradePerStock[numTradePerStock$Count > 0,])/4)
  stockToAvoid <- numTradePerStock[1:topQuarter,]
  StockToAvoid <- subset(stockToAvoid, MeanRet <= 0, select = Symbol)
  StockToAvoid <- StockToAvoid$Symbol
  
  return(StockToAvoid)
}


indexData <- new.env()
startDate = as.Date("2002-01-01") #Specify what date to get the prices from
getSymbols("^RUT", env = indexData, src = "yahoo", from = startDate)
#Calculate returns for the index
indexRet <- (Cl(indexData$RUT)-lag(Cl(indexData$RUT),1))/lag(Cl(indexData$RUT),1)
indexRet <- as.xts(indexRet)['2002::2012']
colnames(indexRet) <- "Russell2000"



#stdLookback <- 90 #How many periods to lookback for the standard deviation calculation

nStockTradeVec_SD_r2k_APYBT_V3 <- list();
conditionsMet_SD_r2k_APYBT_V3 <- list();
tradeMat_SD_r2k_APYBT_V3 <- list();
stockToAvoid2013_SD_r2k_APYBT_V3 <- list();

nskip <- 0
# 6 momths, 3 months, 2 months, 1 month, 2 weeks, 1 week
for(stdLookback in c(132, 66, 44)){
  for(stdMultiple in c(0.5,0.7,1)){
    
    stockData <- new.env();
    gc();
    load_russell2000_stockData(stockData);
    
    # the lo() function don't like the stock name contain "LOW"
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
    
    for (i in 1:length(symbolsLst)) {
     # cat("Calculating the returns and standard deviations for stock: ",symbolsLst[i],"\n")
      sData <- eval(parse(text=paste("stockData$\'",symbolsLst[i], '\'',sep="")))
      sData <- sData['2001-07-01::2012-12-31']
      
      if(nrow(sData) >= stdLookback)
      {
        oldColNames <- names(sData)
        colnames(sData) <- c("S.Open","S.High","S.Low","S.Close","S.Volume","S.Adjusted")
        #Calculate the return from low of yesterday to the open of today
        lowOpenRet <- (Op(sData)-lag(Lo(sData),1))/lag(Lo(sData),1)
        colnames(lowOpenRet) <- paste(symbolsLst[i],".LowOpenRet",sep="")
        
        roc <- ROC(Cl(sData), type='discrete')
        sd <- runSD(roc, n=stdLookback);
        mean <- runMean(roc, n=stdLookback);
        
        colnames(sd) <- paste(symbolsLst[i],".sd",sep="")
        colnames(mean) <- paste(symbolsLst[i],".mean",sep="")
        
        stdClClRet <- mean - stdMultiple*sd
        stdClClRet <- lag(stdClClRet, 1)
        
        colnames(stdClClRet) <- paste(symbolsLst[i],".StdClClRet",sep="")
        
        dayClOpRet <- (Cl(sData)-Op(sData))/Op(sData)
        
        colnames(dayClOpRet) <- paste(symbolsLst[i],".DayClOpRet",sep="")
        colnames(sData) <- oldColNames
        eval(parse(text=paste("stockData$\'",symbolsLst[i], '\'',
                              " <- cbind(sData,lowOpenRet,stdClClRet,dayClOpRet, sd, mean)",sep="")))
      }
      else
      {
        symToExclude <- append(symToExclude, symbolsLst[i])
      }
    }
    
    # exclude stock with ticker less then stdLookback
    symbolsLst <- setdiff(symbolsLst, symToExclude)
    
    for (i in 1:length(symbolsLst)) {
     # cat("Processing stock: ",symbolsLst[i]," to the returns table\n")
      sDataRET <- eval(parse(text=paste("stockData$\'",
                                        symbolsLst[i], '\'',"[,\"",symbolsLst[i],
                                        ".LowOpenRet\"]",sep="")))
      sDataSTD <- eval(parse(text=paste("stockData$\'",
                                        symbolsLst[i], '\'',"[,\"",symbolsLst[i],
                                        ".StdClClRet\"]",sep="")))
      sDataDAYRET <- eval(parse(text=paste("stockData$\'",
                                           symbolsLst[i], '\'',"[,\"",symbolsLst[i],
                                           ".DayClOpRet\"]",sep="")))
      if(i == 1){
        retMat <- sDataRET
        stdMat <- sDataSTD
        dayretMat <- sDataDAYRET
      } else {
        retMat <- cbind(retMat,sDataRET)
        stdMat <- cbind(stdMat,sDataSTD)
        dayretMat <- cbind(dayretMat,sDataDAYRET)
      }
    }
    
    nStockTradeVec <- NULL;
    iloop <- 0;
    
    for(nStocksBuy in c(1,4,8,10,20,60))
    {
      conditionTwo <- retMat #copying the structure and data, only really want the structure
      conditionTwo[,] <- 0 #set all the values to 0
      conditionTwo <- retMat - stdMat #If ClOp ret is < StdRet tmp will be < 1
      conditionTwo <- ifelse(as.matrix(conditionTwo) < 0, 1, 0)
      
      conditionOne <- retMat #copying the structure and data, only really want the structure
      conditionOne[,] <- 0 #set all the values to 0
      
      # PASS 1
      ret_GT_SD <- retMat * conditionTwo
      # The Gap must be wider then -5%
      ret_GT_SD <- ifelse(as.matrix(ret_GT_SD) < -0.025, ret_GT_SD, NA)
      
      for (i in 1:length(ret_GT_SD[,1])){
        orderindex <- order((ret_GT_SD[i,]),decreasing=FALSE, na.last=NA)  #order row entries smallest to largest      
        
        ################### skip the n most negative return ######################
        
        orderindex <- orderindex[(nskip+1):(nStocksBuy+nskip)] #want the smallest n (nStocksBuy) stocks
        orderindex <- orderindex[!is.na(orderindex)];
        
        ################### skip the n most negative return ######################      
        
        conditionOne[i,orderindex] <- 1 #1 Flag indicates entry is one of the nth smallest
      }
      
      conditionsMet <- conditionOne * conditionTwo
      conditionsMet[is.na(conditionsMet)] <- 0
      colnames(conditionsMet) <- gsub(".LowOpenRet","",names(conditionsMet))
      
      tradeMat <- dayretMat
      colnames(tradeMat) <- gsub(".DayClOpRet","",names(tradeMat))
      tradeMat <- tradeMat * conditionsMet
      tradeMat[is.na(tradeMat)] <- 0
      
      ################### skip stock in StockToAvoid list ######################
      for( yr in 2003:2012){
        # generate previous yr StockToAvoid
        conditionsMet_yr <- eval(parse(text=paste("conditionsMet[\'", yr-1, '\']',sep="")))
        tradeMat_yr <- eval(parse(text=paste("tradeMat[\'", yr-1, '\']',sep="")))      
        stockToAvoid_yr <- getStockToAvoid(conditionsMet_yr, tradeMat_yr)
        
        cat('StockToAvoid on year ', yr, ' :', stockToAvoid_yr,'\n');
        # build condition 3 to exclude stockToAvoid
        conditionThree <- retMat #copying the structure and data, only really want the structure
        conditionThree[,] <- 1
        conditionThree <- as.xts(conditionThree)
        
        for(stk in stockToAvoid_yr){
          stk_LowOpenRet <- paste(stk, '.LowOpenRet', sep='');
          eval(parse(text=paste("conditionThree[\'", yr, '\', \'', stk_LowOpenRet, '\'] <- NA',sep="")));
        }
        
      }
      
      if(nStocksBuy == 10){
        # generate previous yr StockToAvoid
        conditionsMet_yr <- eval(parse(text=paste("conditionsMet[\'", 2012, '\']',sep="")))
        tradeMat_yr <- eval(parse(text=paste("tradeMat[\'", 2012, '\']',sep="")))      
        stockToAvoid_yr <- getStockToAvoid(conditionsMet_yr, tradeMat_yr)
        
        cat('StockToAvoid on year ', 2013, ' :', stockToAvoid_yr,'\n');
        stockToAvoid2013_SD_r2k_APYBT_V3[[as.character(nskip)]][[as.character(stdLookback)]][[as.character(stdMultiple)]] <- stockToAvoid_yr;
      }
      
      # PASS 2 -- conditionThree to exclude stockToAvoid
      ret_GT_SD <- retMat * conditionTwo * conditionThree
      # The Gap must be wider then -5%
      ret_GT_SD <- ifelse(as.matrix(ret_GT_SD) < -0.025, ret_GT_SD, NA)
      
      for (i in 1:length(ret_GT_SD[,1])){
        orderindex <- order((ret_GT_SD[i,]),decreasing=FALSE, na.last=NA)  #order row entries smallest to largest      
        
        ################### skip the n most negative return ######################
        
        orderindex <- orderindex[(nskip+1):(nStocksBuy+nskip)] #want the smallest n (nStocksBuy) stocks
        orderindex <- orderindex[!is.na(orderindex)];
        
        ################### skip the n most negative return ######################      
        
        conditionOne[i,orderindex] <- 1 #1 Flag indicates entry is one of the nth smallest
      }
      
      conditionsMet <- conditionOne * conditionTwo * conditionThree
      colnames(conditionsMet) <- gsub(".LowOpenRet","",names(conditionsMet))
      conditionsMet[is.na(conditionsMet)] <- 0
      
      tradeMat <- dayretMat
      colnames(tradeMat) <- gsub(".DayClOpRet","",names(tradeMat))
      tradeMat <- tradeMat * conditionsMet
      tradeMat[is.na(tradeMat)] <- 0
      
      ################### skip stock in StockToAvoid list ######################
      
      
      tradeVecMat <- (apply(tradeMat, 1, function(x) sum(x, na.rm=TRUE)) / 
                        apply(conditionsMet, 1, function(x) sum(x, na.rm=TRUE)));
      tradeVec <- as.xts(tradeVecMat, as.Date(names(tradeVecMat)));
      colnames(tradeVec) <- paste("BuyOnGap.", nStocksBuy, sep="" )
      tradeVec[is.nan(tradeVec[,1]),1] <- 0 #Didnt make or loose anything on this day
      
      print(paste('Processed nskip=',nskip,' SD Period=',stdLookback,' SD=',stdMultiple,' nStocksBuy=', nStocksBuy, sep=''))
      
      if(nStocksBuy == 10)
      {
        conditionsMet_SD_r2k_APYBT_V3[[as.character(nskip)]][[as.character(stdLookback)]][[as.character(stdMultiple)]] <- conditionsMet;
        tradeMat_SD_r2k_APYBT_V3[[as.character(nskip)]][[as.character(stdLookback)]][[as.character(stdMultiple)]] <- tradeMat;
      }
      
      if(iloop == 0)
        nStockTradeVec <- tradeVec
      else
        nStockTradeVec <- cbind(nStockTradeVec,tradeVec )
      
      iloop <- iloop + 1;
    }
    
    nStockTradeVec <- as.xts(nStockTradeVec)
    nStockTradeVec <- cbind(nStockTradeVec,indexRet )
    nStockTradeVec_SD_r2k_APYBT_V3[[as.character(nskip)]][[as.character(stdLookback)]][[as.character(stdMultiple)]] <- nStockTradeVec
  }
}

save(nStockTradeVec_SD_r2k_APYBT_V3, 
     conditionsMet_SD_r2k_APYBT_V3,
     tradeMat_SD_r2k_APYBT_V3, stockToAvoid2013_SD_r2k_APYBT_V3,
     file='/home/jhleong/dev/R/buy_on_gap/result_Rdata/nStockTradeVec_SD_r2k_APYBT_V3.Rdata' );
rm(nStockTradeVec_SD_r2k_APYBT_V3,conditionsMet_SD_r2k_APYBT_V3,tradeMat_SD_r2k_APYBT_V3);
gc();
