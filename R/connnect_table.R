#' Combine all the statement data together
#'
#' @param code The fund's "OF" code you create
#' @param filename  The fund's statement file name
#'
#' @return Statement data vector containing all the items
#' @export
#'
#' @examples
#' connnect_table("014645.OF.pdf",sep='')


connnect_table<-function(code,filename){
  raw<-read_statement(filename)
  code_fees<-holding_fee(raw)
  code_trading_fee<-investment_cost(raw)
  code_trading<-affliated_cost(raw)
  code_result<-c(code,code_fees[,2],
                   code_trading_fee,code_trading[,2])
  return(code_result)
}
