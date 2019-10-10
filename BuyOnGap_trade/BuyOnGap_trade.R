##############################################################################################
#
#    Buy On Gap Trading implementation
#
#    version : 1.03 (production)
#    unit tested on 2013 10 15 22:05:00 
#
#    Note: execute before market about to open, it will sleep till market open
#
#    version 1.02 - Add in allocation limit for SP600 trade. Max 25% for each trade.
#                   limit risk of a single trade with big movement.
#    version 1.03 - Modified the rounding of num of share going to buy with "floor".
#
##############################################################################################
#
#    TODO:
#    
#    1) Once the matched stock found, observation found it can continue going downb 
#       substantially. A monitoring procedure should be implement to only buy 
#       when there is 10% reverse from the open price to the buttom, and the current 
#       price is lower then the permited gap. eg. EXEP on 20131014
#
#    2) set max trade to 10 and min trade to 4. The 
#       the max allocation should 25% for each stock.
#
#    3) check the daily volumn of the stock before placing order. Avoid those very 
#       low volumne stock especially in SP600. eg. PCTI
#    4) put a horizontal line at the trade report chart to indicate  avg buying price
#
##############################################################################################


Sys.setenv(TZ='America/New_York') # for reqCurrentTime to return US market local time.
source('/home/jhleong/dev/R/lib/libIB.R');
library(RSQLite);

# disable placing order and write to db if turn on
DEBUG_FLAG <- F

if(DEBUG_FLAG)
  cat(' DEBUG_FLAG is turn on !!! NOT placing any order and NOT saving any data !!!\n');

# SP500
load_filename <- paste("/data/BuyOnGap/stockData/", as.character(trunc(Sys.time(), "days")), 
                       "_sp500_condMat.Rdata", sep = "");
load(load_filename);  # conditionMatrix, sym
sp500_sym <- symbolsLst;

# SP600
filename <- paste("/data/BuyOnGap/stockData/", as.character(trunc(Sys.time(), "days")), 
                  "_sp600_prev_close.Rdata", sep = "");
load(file=filename); # sp600_prev_day_price, sp600_sym

# Global db connection
db_conn <- init_db();
.buy_on_gap_sp500_order_ref <- db_get_order_ref(.buy_on_gap_sp500_portf_id);
.buy_on_gap_sp600_order_ref <- db_get_order_ref(.buy_on_gap_sp600_portf_id);

tws_acc <- twsConnect(clientId=.cleint_id_general_purpose);

trading_date_str <- strftime(Sys.time(), format="%Y%m%d", tz='EST5EDT');

# Sleep until 9:25:00 am EST, do preapration work
sec_to_sleep <- ((9 * 60 + 25) * 60 + 0) - as.numeric(difftime( reqCurrentTime(tws_acc) , trunc(Sys.time(),"days")), units='secs')
if(sec_to_sleep > 0 )
{
  cat(paste0('[',Sys.time(),']',' Sleep for ',sec_to_sleep,' secs\n'));
  Sys.sleep( sec_to_sleep );
}


# trade_capital_sp500 <- db_get_portf_prev_amt(.buy_on_gap_sp500_portf_id, trading_date_str, db_conn)$cash;
# cat('BuyOnGap_SP500 current cash :', trade_capital_sp500,'\n');
# 
# trade_capital_sp600 <- db_get_portf_prev_amt(.buy_on_gap_sp600_portf_id, trading_date_str,db_conn)$cash;
# cat('BuyOnGap_SP600 current cash :', trade_capital_sp600,'\n');


# Sleep until 9:32:00 am EST
sec_to_sleep <- ((9 * 60 + 32) * 60 + 0) - as.numeric(difftime( reqCurrentTime(tws_acc) , trunc(Sys.time(),"days")), units='secs')
if(sec_to_sleep > 0 )
{
  cat(paste0('[',Sys.time(),']',' Sleep for ',sec_to_sleep,' secs\n'));
  Sys.sleep( sec_to_sleep );
}

# get any manual execution for open position.
db_ins_exection(tws_acc, trading_date_str, db_conn);
db_process_BOG_execution(db_conn, debug_flag=F);

trade_capital_sp500 <- db_get_portf_prev_amt(.buy_on_gap_sp500_portf_id, trading_date_str, db_conn)$cash;
cat('BuyOnGap_SP500 current cash :', trade_capital_sp500,'\n');

trade_capital_sp600 <- db_get_portf_prev_amt(.buy_on_gap_sp600_portf_id, trading_date_str,db_conn)$cash;
cat('BuyOnGap_SP600 current cash :', trade_capital_sp600,'\n');

###########################################################################################
#
#     S&P 500 Large Cap scenerio 
#
###########################################################################################

openQuote_sp500 <- getLastPrice.IB.WithTimeOut(sp500_sym);
cat('S&P500 quote from IB finished at ', as.character(Sys.time()),'\n');

# convert Yahoo Symbol name to IB symbol name
conditionMatrix$Symbol = gsub("\\.", " ", conditionMatrix$Symbol);
conditionMatrix$Symbol <- gsub("-", " ", conditionMatrix$Symbol);
condMatrix_sp500 <- merge(x=conditionMatrix, y=openQuote_sp500, by='Symbol');

# eliminate the condMatrix_sp500$Last == -1, stock that been halted trading.
condMatrix_sp500 <- condMatrix_sp500[-condMatrix_sp500$Last < 0,]

condMatrix_sp500$LoOpenRet <- (condMatrix_sp500$Last / condMatrix_sp500$Prev_Low) -1 ;

matched_idx <- which((condMatrix_sp500$Mean_SD > condMatrix_sp500$LoOpenRet) & 
                       condMatrix_sp500$LoOpenRet < .buy_on_gap_sp500_min_gap);  # -1.5%
#matched_idx <- which((condMatrix_sp500$Mean_SD > condMatrix_sp500$LoOpenRet) & condMatrix_sp500$LoOpenRet < -0)
matched_condMatrix_sp500 <- condMatrix_sp500[matched_idx,];

large_gap_idx <- which(matched_condMatrix_sp500$LoOpenRet < .buy_on_gap_sp500_large_gap); # -10%

if(length(large_gap_idx) > 0){
  cat(paste0('SP500 - Manual decision needed. > ', .buy_on_gap_sp500_large_gap * 100 ,'%  Gap for '), 
      paste(matched_condMatrix_sp500[large_gap_idx,'Symbol'], collapse=','),'\n');
  
  matched_condMatrix_sp500 <- matched_condMatrix_sp500[-large_gap_idx,];
}


cat('S&P 500 - Stock matched the rules :\n');
print(matched_condMatrix_sp500);

# Open order connection to TWS
tws_order_conn <- twsConnect47(clientId = .client_id_buy_on_gap)
setServerLogLevel(tws_order_conn,5)


sym_trade_sp500 <- NULL;
matched_trade_sp500 <- NULL;
trade_order_buy_sp500 <- NULL;
trade_order_close_sp500 <- NULL;

if(nrow(matched_condMatrix_sp500) > 0){
  
  orderindex <- order(matched_condMatrix_sp500$LoOpenRet, decreasing=FALSE, na.last=NA);
  orderindex <- orderindex[(.buy_on_gap_sp500_nskip+1):(.buy_on_gap_sp500_nStocksBuy+.buy_on_gap_sp500_nskip)]; #want the smallest n (nStocksBuy) stocks
  orderindex <- orderindex[!is.na(orderindex)];
  matched_trade_sp500 <- matched_condMatrix_sp500[orderindex,];
  sym_trade_sp500 <- as.character(matched_condMatrix_sp500[orderindex,1]);
  
  num_of_trade <- nrow(matched_trade_sp500);
  
  for(i in 1:nrow(matched_trade_sp500))
  {
    sym_c <- matched_trade_sp500[i,'Symbol'];
    sym_price <- matched_trade_sp500[i,'Last'] # Assume current price is close to the previous LAST price
    sym_num_share <- floor((trade_capital_sp500 / num_of_trade) / sym_price);
    
    trade_order_buy_sp500 <- rbind( trade_order_buy_sp500, 
                                data.frame(Symbol=sym_c, ReqId=NA, Action='BUY', NumShare=sym_num_share, 
                                           OrderType='MKT', ClientId=tws_order_conn$clientId, LmtPrice=NA, 
                                           OrderRef=.buy_on_gap_sp500_order_ref, stringsAsFactors=FALSE));
    cat('Placed order to BUY at MARKET :', sym_c, ' ',sym_num_share,' shares\n'); 
  }
  
  if(is.null(DEBUG_FLAG) | DEBUG_FLAG==F)
    trade_order_buy_sp500 <- BuyOnGap_placeOrder(tws_order_conn, trade_order_buy_sp500); # update reqId;
  
} else {
  cat('S&P 500 - No trade for today !!!\n');
}


###########################################################################################
#
#     S&P 600 Small Cap scenerio 

#Sp600 at 9:40 - 9:45 am ?
###########################################################################################


openQuote_sp600 <- getLastPrice.IB.WithTimeOut(sp600_sym);
cat('S&P600 quote from IB finished at ', as.character(Sys.time()),'\n');

condMat_sp600 <- merge(x=sp600_prev_day_price, y=openQuote_sp600, by='Symbol');

# eliminate the condMat_sp600$Last == -1, stock that been halted trading.
condMat_sp600 <- condMat_sp600[-condMat_sp600$Last < 0,]

condMat_sp600$LoOpenRet <- condMat_sp600$Last  / condMat_sp600$Prev_Low - 1;
matched_idx_sp600 <- which(condMat_sp600$LoOpenRet < -0.025);
matched_condMat_sp600 <- condMat_sp600[matched_idx_sp600,];

cat('S&P 600 - Stock matched the rules :\n');
print(matched_condMat_sp600);


large_gap_idx <- which(matched_condMat_sp600$LoOpenRet < -0.3);
if(length(large_gap_idx) > 0){
  cat(paste0('SP600 - Manual decision needed > ',.buy_on_gap_sp600_large_gap * 100,'% Gap for ', paste(matched_condMat_sp600[large_gap_idx,'Symbol'], collapse=',')),'\n');
  matched_condMat_sp600 <- matched_condMat_sp600[-large_gap_idx,];
}


sym_trade_sp600 <- NULL;
matched_trade_sp600 <- NULL;
trade_order_buy_sp600 <- NULL;
trade_order_close_sp600 <- NULL;

if(nrow(matched_condMat_sp600) > 0){
  
  orderindex <- order(matched_condMat_sp600$LoOpenRet, decreasing=FALSE, na.last=NA);
  orderindex <- orderindex[(.buy_on_gap_sp600_nskip+1):(.buy_on_gap_sp600_nStocksBuy+.buy_on_gap_sp600_nskip)]; 
  orderindex <- orderindex[!is.na(orderindex)];
  matched_trade_sp600 <- matched_condMat_sp600[orderindex,];
  sym_trade_sp600 <- as.character(matched_condMat_sp600[orderindex,'Symbol']);
  
  num_of_trade <- nrow(matched_trade_sp600);
  
  for(i in 1:nrow(matched_trade_sp600))
  {
    sym_c <- matched_trade_sp600[i,'Symbol'];
    sym_price_last <- matched_trade_sp600[i,'Last'];
    sym_gap_amt <- matched_trade_sp600[i,'Prev_Low'] - sym_price_last;
    # the limit order at last +25% gap
    sym_price <- round(sym_price_last + (sym_gap_amt * .buy_on_gap_sp600_slippage_gap_limit), digits=2); 
    # each trade limited to .buy_on_gap_sp600_alloc_lmt (0.25) of portofolio
    sym_num_share <- floor((trade_capital_sp600 / max( 1/.buy_on_gap_sp600_alloc_lmt ,num_of_trade) / sym_price_last));
    
    trade_order_buy_sp600 <- rbind( trade_order_buy_sp600, data.frame(Symbol=sym_c, ReqId=NA, Action='BUY', NumShare=sym_num_share, OrderType='LMT', LmtPrice=sym_price, ClientId=tws_order_conn$clientId, OrderRef=.buy_on_gap_sp600_order_ref, stringsAsFactors=FALSE));
    cat('Placed order to BUY LIMIT :',sym_c , ' ',sym_num_share,' shares @', sym_price,'\n');
  }
  
  if(is.null(DEBUG_FLAG) | DEBUG_FLAG==F)
    trade_order_buy_sp600 <- BuyOnGap_placeOrder(tws_order_conn, trade_order_buy_sp600); # update reqId;
  
} else {
  cat('S&P 600 - No trade for today !!!\n');
}

if(is.null(DEBUG_FLAG) | DEBUG_FLAG==F) {
  
  # wait for 30 mins, if order not filled, cancel them
  # put SELL order at market close.
  Sys.sleep(60*30);
  
  # cancel unfilled order
  all_order_id <- c(trade_order_buy_sp500$ReqId, trade_order_buy_sp600$ReqId);
  lapply(all_order_id, function(x) cancelOrder(tws_order_conn, x));
  
  Sys.sleep(60);
  db_ins_exection(tws_acc, trading_date_str, db_conn);
  
  
  # TODO: a near time real loop to retrieve execution.
  
  
  # generate closing trade
  bot_sp500 <- db_get_execution(.buy_on_gap_sp500_order_ref, trading_date_str, db_conn);
  if(NROW(bot_sp500) > 0){
    print(bot_sp500);
    trade_order_close_sp500 <- rbind( trade_order_close_sp500, 
                                      data.frame(Symbol=bot_sp500$Symbol, 
                                                 ReqId=NA , Action='SELL', 
                                                 NumShare=bot_sp500$B.Qty, 
                                                 OrderType='MKTCLS', 
                                                 ClientId=tws_order_conn$clientId, 
                                                 LmtPrice=NA,
                                                 OrderRef=.buy_on_gap_sp500_order_ref, 
                                                 stringsAsFactors=FALSE));
    
    trade_order_close_sp500 <- BuyOnGap_placeOrder(tws_order_conn, trade_order_close_sp500); 
    
    cat('Placed order to SELL at MARKET CLOSE :', bot_sp500$Symbol, ' ',bot_sp500$B.Qty,' shares\n');
  }
  
  
  bot_sp600 <- db_get_execution(.buy_on_gap_sp600_order_ref, trading_date_str, db_conn);
  if(NROW(bot_sp600) > 0){
    print(bot_sp600);
    trade_order_close_sp600 <- rbind( trade_order_close_sp600, 
                                      data.frame(Symbol=bot_sp600$Symbol, 
                                                 ReqId=NA , Action='SELL', 
                                                 NumShare=bot_sp600$B.Qty, 
                                                 OrderType='MKTCLS', 
                                                 ClientId=tws_order_conn$clientId, 
                                                 LmtPrice=NA,
                                                 OrderRef=.buy_on_gap_sp600_order_ref, 
                                                 stringsAsFactors=FALSE));
    
    trade_order_close_sp600 <- BuyOnGap_placeOrder(tws_order_conn, trade_order_close_sp600); 
    
    cat('Placed order to SELL at MARKET CLOSE :', bot_sp600$Symbol, ' ',bot_sp600$B.Qty,' shares\n');
  }
  
  # update execution trade into db
  if(NROW(trade_order_buy_sp500) > 0 ){
    BuyOnGap_db_ins_orders(trade_order_buy_sp500, .buy_on_gap_sp500_portf_id); }
  if(NROW(trade_order_buy_sp600) > 0){
    BuyOnGap_db_ins_orders(trade_order_buy_sp600, .buy_on_gap_sp600_portf_id); }
  if(NROW(trade_order_close_sp500) > 0 ){
    BuyOnGap_db_ins_orders(trade_order_close_sp500, .buy_on_gap_sp500_portf_id); }
  if(NROW(trade_order_close_sp600) > 0){
    BuyOnGap_db_ins_orders(trade_order_close_sp600, .buy_on_gap_sp600_portf_id); }
}

# make sure all the message sent to IB TWS.
Sys.sleep(5);
twsDisconnect(tws_order_conn);
twsDisconnect(tws_acc);
dbDisconnect(db_conn);

if(is.null(DEBUG_FLAG) | DEBUG_FLAG==F) {
  match_cond_filename <- paste("/data/BuyOnGap/stockData/", as.character(trunc(Sys.time(), "days")), 
                               "_sp500_match_cond.Rdata", sep = "");
  save(openQuote_sp500, condMatrix_sp500, matched_condMatrix_sp500, sym_trade_sp500, matched_trade_sp500, trade_order_buy_sp500, trade_order_close_sp500, file=match_cond_filename);
  
  match_cond_filename <- paste("/data/BuyOnGap/stockData/", as.character(trunc(Sys.time(), "days")), 
                               "_sp600_match_cond.Rdata", sep = "");
  save(openQuote_sp600, condMat_sp600, matched_condMat_sp600, sym_trade_sp600, matched_trade_sp600, trade_order_buy_sp600, trade_order_close_sp600, file=match_cond_filename);
}

cat(paste0('[',Sys.time(),']',' Process finished \n'));