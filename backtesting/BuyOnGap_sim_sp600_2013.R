#############################################################################
#
#    calculate for SP600 for year 2013 
#    
#   
#
#############################################################################

library("quantmod")
library("PerformanceAnalytics") #Load the PerformanceAnalytics library
source('/home/jhleong/dev/R/lib/libtrade.R');

indexData <- new.env()
startDate = as.Date("2002-01-01") #Specify what date to get the prices from
#iShares Core S&P Small-Cap ETF (IJR)
getSymbols("IJR", env = indexData, src = "yahoo", from = startDate)
indexData$IJR = adjustOHLC(indexData$IJR, use.Adjusted=T) 
#Calculate returns for the index
indexRet <- (Cl(indexData$IJR)-lag(Cl(indexData$IJR),1))/lag(Cl(indexData$IJR),1)
indexRet <- as.xts(indexRet)['2010/']
colnames(indexRet) <- "S&P600"



nStockTradeVec_SD_sp600_2013 <- list();
conditionsMet_SD_sp600_2013 <- list();
tradeMat_SD_sp600_2013 <- list();


nskip <- 0

# 6 momths, 3 months, 2 months, 1 month, 2 weeks, 1 week
for(stdLookback in c(66)){
  for(stdMultiple in c(0.5,1)){
    
    stockData <- new.env();
    gc();
    load_sp600_stockData(stockData);
    
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
      #cat("Calculating the returns and standard deviations for stock: ",symbolsLst[i],"\n")
      sData <- eval(parse(text=paste("stockData$\'",symbolsLst[i], '\'',sep="")))
      sData <- sData['2010/'] 
      
      # The num of tick must > stdLookback and the Open price must >= 1 dollar
      if((nrow(sData) >= stdLookback) & (sData[1,1] >= 1.0))
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
    
    for(nStocksBuy in c(1,4,8,10,60))
    {
      conditionTwo <- retMat #copying the structure and data, only really want the structure
      conditionTwo[,] <- 0 #set all the values to 0
      conditionTwo <- retMat - stdMat #If ClOp ret is < StdRet tmp will be < 1
      conditionTwo <- ifelse(as.matrix(conditionTwo) < 0, 1, 0)
      
      conditionOne <- retMat #copying the structure and data, only really want the structure
      conditionOne[,] <- 0 #set all the values to 0
      
      ret_GT_SD <- retMat * conditionTwo
      # exlcude retMat with positive ret which meet the condition
      ret_GT_SD <- ifelse(as.matrix(ret_GT_SD) < 0, ret_GT_SD, NA)
      
      for (i in 1:length(ret_GT_SD[,1])){
        orderindex <- order((ret_GT_SD[i,]),decreasing=FALSE, na.last=NA)  #order row entries smallest to largest      
        
        ################### skip the n most negative return ######################
        
        orderindex <- orderindex[(nskip+1):(nStocksBuy+nskip)] #want the smallest n (nStocksBuy) stocks
        orderindex <- orderindex[!is.na(orderindex)];
        
        ################### skip the n most negative return ######################      
        
        conditionOne[i,orderindex] <- 1 #1 Flag indicates entry is one of the nth smallest
      }
      
      conditionsMet <- conditionOne * conditionTwo
      colnames(conditionsMet) <- gsub(".LowOpenRet","",names(conditionsMet))
      conditionsMet[is.na(conditionsMet)] <- 0
      
      tradeMat <- dayretMat
      colnames(tradeMat) <- gsub(".DayClOpRet","",names(tradeMat))
      tradeMat <- tradeMat * conditionsMet
      tradeMat[is.na(tradeMat)] <- 0
      tradeVecMat <- (apply(tradeMat, 1, function(x) sum(x, na.rm=TRUE)) / 
                        apply(conditionsMet, 1, function(x) sum(x, na.rm=TRUE)));
      tradeVec <- as.xts(tradeVecMat, as.Date(names(tradeVecMat)));
      colnames(tradeVec) <- paste("BuyOnGap.", nStocksBuy, sep="" )
      tradeVec[is.nan(tradeVec[,1]),1] <- 0 #Didnt make or loose anything on this day
      
      print(paste('Processed nskip=',nskip,' SD Period=',stdLookback,' SD=',stdMultiple,' nStocksBuy=', nStocksBuy, sep=''))
      
      if(nStocksBuy == 10)
      {
        conditionsMet_SD_sp600_2013[[as.character(nskip)]][[as.character(stdLookback)]][[as.character(stdMultiple)]] <- conditionsMet;
        tradeMat_SD_sp600_2013[[as.character(nskip)]][[as.character(stdLookback)]][[as.character(stdMultiple)]] <- tradeMat;
      }
      
      if(iloop == 0)
        nStockTradeVec <- tradeVec
      else
        nStockTradeVec <- cbind(nStockTradeVec,tradeVec )
      
      iloop <- iloop + 1;
    }
    
    nStockTradeVec <- as.xts(nStockTradeVec)
    nStockTradeVec <- cbind(nStockTradeVec,indexRet )
    nStockTradeVec_SD_sp600_2013[[as.character(nskip)]][[as.character(stdLookback)]][[as.character(stdMultiple)]] <- nStockTradeVec
  }
}

save(nStockTradeVec_SD_sp600_2013, conditionsMet_SD_sp600_2013,tradeMat_SD_sp600_2013,
     file='/home/jhleong/dev/R/buy_on_gap/result_Rdata/nStockTradeVec_SD_sp600_2013.Rdata' );
rm(nStockTradeVec_SD_sp600_2013, conditionsMet_SD_sp600_2013,tradeMat_SD_sp600_2013);
gc();
