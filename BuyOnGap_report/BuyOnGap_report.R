library(knitr); 
trading_date_str <- strftime(Sys.time(), format="%Y%m%d", tz='EST5EDT');
# knit2pdf('/home/jhleong/dev/R/buy_on_gap/BuyOnGap_trade/BuyOnGap_report2.Rnw', 
#          paste0('/data/BuyOnGap/report/BuyOnGap_report_',trading_date_str,".tex"),
#          compiler=NULL,clean=T);

file_name <- paste0('/home/jhleong/dev/R/buy_on_gap/BuyOnGap_report/BuyOnGap_report_',trading_date_str);
tex_file <- paste0(file_name,".tex");
pdf_file <- paste0(file_name,".pdf");

knit2pdf('/home/jhleong/dev/R/buy_on_gap/BuyOnGap_report/BuyOnGap_report2.Rnw', 
         tex_file,
         compiler=NULL,clean=T);

file.remove(tex_file);
system(paste0('mv ',pdf_file,' /data/BuyOnGap/report/'));