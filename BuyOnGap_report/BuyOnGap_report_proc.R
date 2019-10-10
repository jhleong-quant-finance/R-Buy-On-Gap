source('/home/jhleong/dev/R/lib/libtrade.R');
source('/home/jhleong/dev/R/lib/libIB.R');

log_file <- '/home/jhleong/dev/R/log/BuyOnGap_report.log';
#file_log_file <- file('/home/jhleong/dev/R/log/BuyOnGap_report.log', open="w+");
#sink(file_log_file, type='message', append=T);

# set the trading date for report generating
trading_date_str <- NULL;
#trading_date_str <- '20131121';

if(is.null(trading_date_str)){
  trading_date_str <- strftime(Sys.time(), format="%Y%m%d", tz='EST5EDT');
}

BOG_report_data_filename <- paste("/data/BuyOnGap/reportData/", trading_date_str , 
                                  "_BOG_report_data.Rdata", sep = "");

# if the BOG report data were generated
if(file.exists(BOG_report_data_filename)){
  load(BOG_report_data_filename)
} else {
  
  capture.output(tws_conn <- twsConnect(clientId=.cleint_id_general_purpose),file=log_file, append=T);
  
  capture.output(svr_time <- strftime(reqCurrentTime(tws_conn), format="%Y%m%d %H:%M:%S", tz='EST5EDT'),file=log_file, append=T);
  # 10 mins after market close
  mkt_closing_time <- paste0(trading_date_str, " ", '16:05:00');
  
  if(svr_time < mkt_closing_time){
    stop(paste0("can't process ", trading_date_str, " execution before market closed."));
  }
  
  db_conn <- init_db();
  .buy_on_gap_sp500_order_ref <- db_get_order_ref(.buy_on_gap_sp500_portf_id);
  .buy_on_gap_sp600_order_ref <- db_get_order_ref(.buy_on_gap_sp600_portf_id);
  
   capture.output(db_ins_exection(tws_conn, db_conn),file=log_file, append=T);
   capture.output(db_process_BOG_execution(db_conn, debug=T),file=log_file, append=T);

  
  trade_summary_sp500 <- db_get_trade_summary(.buy_on_gap_sp500_portf_id, trading_date_str, db_conn);
  trade_stat_sp500 <- db_get_trade_stat(.buy_on_gap_sp500_portf_id, trading_date_str, db_conn);
  trade_stat_sp500[8,'desc'] <- 'S&P 500 Ret %:';
  asset_sp500 <- db_get_asset_detail(.buy_on_gap_sp500_portf_id, trading_date_str, db_conn);
  
  trade_summary_sp600 <- db_get_trade_summary(.buy_on_gap_sp600_portf_id, trading_date_str, db_conn);
  trade_stat_sp600 <- db_get_trade_stat(.buy_on_gap_sp600_portf_id, trading_date_str, db_conn);
  trade_stat_sp600[8,'desc'] <- 'S&P 600 Ret %:';
  asset_sp600 <- db_get_asset_detail(.buy_on_gap_sp600_portf_id, trading_date_str, db_conn);
  
  
  
  # list of traded symbols
  traded_sym_list <- unique(c(as.character(trade_summary_sp500$Symbol),
                              as.character(asset_sp500$Symbol),
                              as.character(trade_summary_sp600$Symbol),
                              as.character(asset_sp600$Symbol)
  ));
  
  message('getFullDayPrice.IB');
  capture.output(IB_FullDayPrice <- getFullDayPrice.IB(traded_sym_list, trading_date_str),file=log_file, append=T);
  
  # TODO: put news title 
  
  #download google news
  contract_news <- list();
  google_news <- list();
  for(i in 1:NROW(traded_sym_list))
  {  
    goog_news <- getNews_GoogleFinance(traded_sym_list[i],10);
    contract_news[[traded_sym_list[i]]] <- goog_news[,c('title','Site','Lead')];
    news_df <- as.data.frame(goog_news$Lead);
    colnames(news_df) <- traded_sym_list[i];
    rownames(news_df) <- NULL;
    google_news[[traded_sym_list[i]]] <- news_df;
  }
  
  
  # save downloaded google news, stock prices
  save(traded_sym_list, contract_news, IB_FullDayPrice, google_news,
       trade_summary_sp500, trade_stat_sp500, asset_sp500,
       trade_summary_sp600, trade_stat_sp600, asset_sp600,
       file=BOG_report_data_filename);
  
  
  Sys.sleep(5);
  twsDisconnect(tws_conn);
  dbDisconnect(db_conn);
}

