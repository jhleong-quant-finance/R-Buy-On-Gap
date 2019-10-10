#############################################################################
#
#    Buy On Gap Trading checking
#    
#
#############################################################################
library('IBrokers', quietly=T);
library('quantmod', quietly=T);
source('/home/jhleong/dev/R/lib/libIB.R');

trading_date <- '20130307'
trade_report_fname <- paste("/data/BuyOnGap/trade_report/trades.",trading_date,".csv", sep='');
trade_report = read.csv(trade_report_fname);

stock_traded <- as.vector(unique(trade_report[trade_report$Action=='BOT', 'Underlying']))

cal_avg_price <- function(sym_c, action)
{
  sym_trade <- trade_report[trade_report$Action==action & trade_report$Underlying==sym_c,c('Quantity','Price')];
  avg <- sum(sym_trade$Quantity * sym_trade$Price)/sum(sym_trade$Quantity);
  return(avg);
}

avg_buy_price <- sapply(stock_traded, function(x) cal_avg_price(x,'BOT'))
avg_buy_price <- data.frame(Symbol=stock_traded,AvgBuyPrice=avg_buy_price, stringsAsFactors=F)

avg_sell_price <- sapply(stock_traded, function(x) cal_avg_price(x,'SLD'))
avg_sell_price <- data.frame(Symbol=stock_traded,AvgSellPrice=avg_sell_price, stringsAsFactors=F)

trade_ret <- merge(x=avg_buy_price, y=avg_sell_price, by='Symbol' )

trade_ret$Ret <- round((trade_ret$AvgSellPrice - trade_ret$AvgBuyPrice) / trade_ret$AvgBuyPrice, digits=4)
trade_ret
mean_ret <- mean(trade_ret$Ret);
cat('Mean Return on ',trading_date,' : ',mean_ret,'\n')

trade_report$trade_amt <- trade_report$Quantity * trade_report$Price 


cat('Bought Amount:', sum(trade_report[trade_report$Action=='BOT','trade_amt'],na.rm=T))
cat('Sold   Amount:', sum(trade_report[trade_report$Action=='SLD','trade_amt'],na.rm=T))
cat('Commission   :', sum(trade_report$Commission, na.rm=T))
cat('P/L incl Comm:', sum(trade_report$Realized.P.L, na.rm=T))
cat('Ret incl Comm:', round(sum(trade_report$Realized.P.L, na.rm=T) / sum(trade_report[trade_report$Action=='BOT','trade_amt'],na.rm=T), digits=4))




IB_Price <- getOpenClosePrice.IB(trade_ret$Symbol, trading_date);
IB_FullDayPrice <- getFullDayPrice.IB(trade_ret$Symbol, trading_date);

num_sym <- length(IB_Price);
openClosePrice <- data.frame(Symbol=rep("", num_sym), IB_Open=rep(NA, num_sym),  IB_Close=rep(NA, num_sym),
                                   stringsAsFactors=FALSE)   
for(i in 1:num_sym)
{
  openClosePrice[i,1] = trade_ret$Symbol[i];
  openClosePrice[i,2] = Op(IB_Price[[i]]);
  openClosePrice[i,3] = Cl(IB_Price[[i]]);
  
  #generate chart
  # chartSeries(IB_FullDayPrice[[1]],theme=chartTheme('white'), up.col='dark green',dn.col='red')
}

trade_ret <- merge(x=trade_ret, y=openClosePrice, by='Symbol' );

cat('Open slippage %:\n');
round(((trade_ret$IB_Open / trade_ret$AvgBuyPrice ) - 1)*100, digits=2)
cat('Close slippage %:\n');
round(((trade_ret$AvgSellPrice / trade_ret$IB_Close) - 1)*100, digits=2)
