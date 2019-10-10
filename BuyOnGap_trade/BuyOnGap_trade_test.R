#############################################################################
#
#    Buy On Gap Trading Simulation with yesterday data
#    
#
#############################################################################

source('/home/jhleong/dev/R/lib/libIB.R');

nskip <- 0;
nStocksBuy <- 10;
# investment capital is 100K
investment_capital <- 100000;

from_date <- '2013-02-28';

load_filename <- paste("/home/jhleong/dev/R/buy_on_gap/BuyOnGap_trade/tmp/", from_date, "_sp500_condMat.Rdata", sep = "");
load(load_filename);  # conditionMatrix, sym

openQuote <- getLastPrice.IB.test(symbolsLst, from_date);

matched_cond <- NULL;

for(i in 1:nrow(conditionMatrix))
{
  sym <- conditionMatrix[i,'Symbol'];
  #  sym <- gsub("\\.", " ", sym);
  #  sym <- gsub("-", " ", sym);
  
  op <- openQuote[openQuote$Symbol == sym, 'Last'];  # use the real time last price column as Open Price
  prev_low <- conditionMatrix[i,'Prev_Low'];
  LoOpenRet <- (op / prev_low) - 1;
  
  # if the sym is not at openQuote
  if(is.null(LoOpenRet))
  {
    cat('Symbol :',sym,' not found at openQuote !!!\n');
  } else
    if((LoOpenRet <  conditionMatrix[i,'Mean_SD']) & LoOpenRet < 0.0 )
    {
      # only in simulation we have close price
      close_price <- openQuote[openQuote$Symbol == sym, 'Close'];
      
      matched_cond <- rbind( matched_cond, data.frame(Symbol=sym, LoOpenRet=LoOpenRet, 
                                                      Last=op, Close=close_price,
                                                      Ret=(close_price/op -1), stringsAsFactors=FALSE));
    }
  
}

orderindex <- order(matched_cond$LoOpenRet, decreasing=FALSE);
orderindex <- orderindex[(nskip+1):(nStocksBuy+nskip)]; #want the smallest n (nStocksBuy) stocks
orderindex <- orderindex[!is.na(orderindex)];
matched_trade <- matched_cond[orderindex,];
sym_trade <- as.character(matched_cond[orderindex,1]);

cat('The average return is : ', round(mean(matched_trade$Ret) * 100,digits=1), 
    '%, amount: ', investment_capital * (mean(matched_trade$Ret) + 1), '\n');


match_cond_filename <- paste("/data/BuyOnGap/stockData/", from_date, "_sp500_sim_match_cond.Rdata", sep = "");
save(openQuote, sym_trade, matched_trade, file=match_cond_filename);


# Order to buy this stock  matched_cond2$Symbol
# if not NULL

# Order to sell this stock at Market close



