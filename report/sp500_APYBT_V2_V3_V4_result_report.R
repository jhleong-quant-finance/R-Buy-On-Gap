#############################################################################
#
#    Result report for sp500, APYBT, APYBTV2, APYBTV3
#    and APYBTV4
#
#    test using SP500 stocks
#    
#     1) APYBTV2 - The Gap must be wider then 2%
#     2) APYBTV3 - The Gap must be wider then 5%
#     3) APYBTV4 - The Gap must be wider then 1%
#
#############################################################################

library("quantmod")
library("PerformanceAnalytics") #Load the PerformanceAnalytics library
source('/home/jhleong/dev/R/lib/libtrade.R');

load(file='/home/jhleong/dev/R/buy_on_gap/result_Rdata/nStockTradeVec_SD_sp500.Rdata' );
print_AnnualizedReturns(nStockTradeVec_SD_sp500)
print_maxDD(nStockTradeVec_SD_sp500)

load(file='/home/jhleong/dev/R/buy_on_gap/result_Rdata/nStockTradeVec_SD_APYBT.Rdata' );
print_AnnualizedReturns(nStockTradeVec_SD_APYBT)
print_maxDD(nStockTradeVec_SD_APYBT)

load(file='/home/jhleong/dev/R/buy_on_gap/result_Rdata/nStockTradeVec_SD_APYBTV2.Rdata' );
print_AnnualizedReturns(nStockTradeVec_SD_APYBTV2)
print_maxDD(nStockTradeVec_SD_APYBTV2)
stat <- buy_on_gap.numTradePerDay((conditionsMet_SD_APYBTV2[['0']][['66']][['0.5']])['2012'],(tradeMat_SD_APYBTV2[['0']][['66']][['0.5']])['2012'], (nStockTradeVec_SD_APYBTV2[['0']][['66']][['0.5']])['2012'])
# average num of stock traded each day.
mean(as.numeric(stat[,1]))
stat <- buy_on_gap.numTradePerDay((conditionsMet_SD_APYBTV2[['0']][['66']][['0.5']])['2011'],(tradeMat_SD_APYBTV2[['0']][['66']][['0.5']])['2011'], (nStockTradeVec_SD_APYBTV2[['0']][['66']][['0.5']])['2011'])
mean(as.numeric(stat[,1]))


load(file='/home/jhleong/dev/R/buy_on_gap/result_Rdata/nStockTradeVec_SD_APYBTV3.Rdata' );
print_AnnualizedReturns(nStockTradeVec_SD_APYBTV3)
print_maxDD(nStockTradeVec_SD_APYBTV3)
stat <- buy_on_gap.numTradePerDay((conditionsMet_SD_APYBTV3[['0']][['66']][['0.5']])['2012'],(tradeMat_SD_APYBTV3[['0']][['66']][['0.5']])['2012'], (nStockTradeVec_SD_APYBTV3[['0']][['66']][['0.5']])['2012'])
mean(as.numeric(stat[,1]))
nrow(stat[as.numeric(stat$numOfTrade) > 0,])
stat <- buy_on_gap.numTradePerDay((conditionsMet_SD_APYBTV3[['0']][['66']][['0.5']])['2011'],(tradeMat_SD_APYBTV3[['0']][['66']][['0.5']])['2011'], (nStockTradeVec_SD_APYBTV3[['0']][['66']][['0.5']])['2011'])
mean(as.numeric(stat[,1]))
nrow(stat[as.numeric(stat$numOfTrade) == 0,])


##################333

library("quantmod")
library("PerformanceAnalytics") #Load the PerformanceAnalytics library
source('/home/jhleong/dev/R/lib/libtrade.R');

load(file='/home/jhleong/dev/R/buy_on_gap/result_Rdata/nStockTradeVec_SD_sp500_2013.Rdata' );
round(table.AnnualizedReturns(
  as.xts((nStockTradeVec_SD_sp500_2013[['0']][['66']][['0.5']]))['2013']), digits=2)

load(file='/home/jhleong/dev/R/buy_on_gap/result_Rdata/nStockTradeVec_SD_sp500_2013_v2.Rdata' );
round(table.AnnualizedReturns(
  as.xts((nStockTradeVec_SD_sp500_2013_v2[['0']][['66']][['0.5']]))['2013']), digits=2)

load(file='/home/jhleong/dev/R/buy_on_gap/result_Rdata/nStockTradeVec_SD_sp500_2013_v3.Rdata' );
round(table.AnnualizedReturns(
  as.xts((nStockTradeVec_SD_sp500_2013_v3[['0']][['66']][['0.5']]))['2013']), digits=2)

buy_on_gap.numTradePerDay((conditionsMet_SD_sp500_2013[['0']][['66']][['0.5']])['2013-01-01/2013-02-28'],(tradeMat_SD_sp500_2013[['0']][['66']][['0.5']])['2013-01-01/2013-02-28'], (nStockTradeVec_SD_sp500_2013[['0']][['66']][['0.5']])['2013-01-01/2013-02-28'])


> load(file='/home/jhleong/dev/R/buy_on_gap/result_Rdata/nStockTradeVec_SD_sp500_2013.Rdata' );
> round(table.AnnualizedReturns(
  +   as.xts((nStockTradeVec_SD_sp500_2013[['0']][['66']][['0.5']]))['2013']), digits=2)
BuyOnGap.1 BuyOnGap.4 BuyOnGap.8 BuyOnGap.10 BuyOnGap.60 Russell2000
Annualized Return            8616.12      91.32      32.51       27.26        6.09        0.67
Annualized Std Dev              1.53       0.55       0.40        0.35        0.25        0.14
Annualized Sharpe (Rf=0%)    5624.96     165.01      81.10       78.55       24.78        4.94
> 
  > load(file='/home/jhleong/dev/R/buy_on_gap/result_Rdata/nStockTradeVec_SD_sp500_2013_v2.Rdata' );
> round(table.AnnualizedReturns(
  +   as.xts((nStockTradeVec_SD_sp500_2013_v2[['0']][['66']][['0.5']]))['2013']), digits=2)
BuyOnGap.1 BuyOnGap.4 BuyOnGap.8 BuyOnGap.10 BuyOnGap.60 Russell2000
Annualized Return              -0.25       1.38       0.45        0.52        0.17        0.67
Annualized Std Dev              1.36       0.48       0.39        0.38        0.36        0.14
Annualized Sharpe (Rf=0%)      -0.19       2.86       1.17        1.37        0.46        4.94
> 
  > load(file='/home/jhleong/dev/R/buy_on_gap/result_Rdata/nStockTradeVec_SD_sp500_2013_v3.Rdata' );
> round(table.AnnualizedReturns(
  +   as.xts((nStockTradeVec_SD_sp500_2013_v3[['0']][['66']][['0.5']]))['2013']), digits=2)
BuyOnGap.1 BuyOnGap.4 BuyOnGap.8 BuyOnGap.10 BuyOnGap.60 Russell2000
Annualized Return             127.91       4.66       2.47        1.78        0.89        0.67
Annualized Std Dev              1.43       0.42       0.32        0.29        0.25        0.14
Annualized Sharpe (Rf=0%)      89.53      11.08       7.77        6.07        3.53        4.94