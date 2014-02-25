#' Make a Chart
#' 
#' This function creates a chart for a country
#' 
#' @export
#' @param c is country
#' @param i is indicator.
chart <- function(c='united+states', i='unemployment+rate', d1='2000-01-01'){
  #api = te.connect('guest', 'guest');
  api = te.connect('r@tradingeconomics.com', 'Lisboa!1');
  h = te.get(api,paste('q=historical&c=',c,'&i=',i,'&d1=',d1,sep=''));
  plot(as.Date(h$Date,format='%m/%d/%Y'), h$Value, type='l', xlab='Date',ylab='Value') 
  #invisible();  
}