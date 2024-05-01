#' Output the extracted funds statement data
#'
#' @param code_name_list A vector contain code and file name
#'
#' @return Data result
#' @export
#'
#' @examples code_name_list<-matrix(c("014645.OF",
#' 'http://www.fund.pingan.com/nasfile/1711755883661.pdf'),nrow=1,ncol=2)
#' @examples colnames(code_name_list)<-c('code','filename')
#' output_dat<-funds_output(data.frame(code_name_list))

funds_output<-function(code_name_list){   # concatenate the data from different source

  result_matrix<-lapply(1:nrow(code_name_list),function(i){connnect_table(code_name_list$code[i]
                                                           ,code_name_list$filename[i])})
  colnames(result_matrix)<-c('Code','当期持有基金产生的应支付销售服务费（元）【所有费用】',                       '当期持有基金产生的应支付托管费（元）【所有费用】',
'当期持有基金产生的应支付管理费（元）【所有费用】','（减：）交易费用【所有费用】',
                           '当期持有基金产生的应支付管理费（元）【自家基金】',
'当期持有基金产生的应支付托管费（元）【自家基金】',
'当期持有基金产生的应支付销售服务费（元）【自家基金】',
'当期交易基金产生的申购费（元）【自家基金】',
        '当期交易基金产生的赎回费 （元）【自家基金】',
'当期交易基金产生的交易费 （元）【自家基金】',
'当期交易基金产生的转换费 （元）【自家基金】')
  return(result_matrix)
}
