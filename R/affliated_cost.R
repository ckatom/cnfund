#' Extract the holding fund cost with other affliated institution
#'
#' @param raw Text content from pdf file
#'
#' @return Table of each type of fund cost
#' @export
#'
#' @examples data(pdf_content)
#' @examples affliated_cost(pdf_content)

affliated_cost<-function(raw){ # extract affliated party holding fund cost data
  table_start <- stringr::str_which(tolower(raw), "当期交易及持有基金管理人以及管理人关联方所管理基金")
  table_end <- stringr::str_which(tolower(raw), "利润分配情况")
  table_end <- ifelse(max(table_end)>table_start,table_end[min(which(table_end > table_start))],min(table_end))

  if(length(table_end)==0|length(table_start)==0){
    output_table<-(matrix(NA,nrow=7,ncol=3))
  }
  else if(table_end-table_start<15|table_end==Inf){
    output_table<-(matrix(NA,nrow=7,ncol=3))
  }
  else{
    table <- raw[(table_start+1):(table_end-1)]
    table <- str_replace_all(table, "\\s{2,}", "|")
    text_con <- textConnection(table)
    data_table <- read.csv(text_con, sep = "|",row.names=NULL,col.names = c(1,2,3))
    data_table[,2]<-(gsub(',','',data_table[,2]))
    num_vec<-as.numeric(data_table[,2])
    data_table<-data_table[which(((!is.na(num_vec)) &grepl("\\d.\\d", as.character(num_vec)))| grepl('^-$',data_table[,2])|
                                   grepl( "^－$",data_table[,2])|num_vec==0),]

    if(TRUE %in% grepl('-',data_table[,1])){
      data_table<-data_table[-which(data_table[,1]=='-'),]
    }
    trading_index<-stringr::str_which(tolower(raw), "基金产生的交易")
    if(length(stringr::str_which(tolower(raw), '产生的应支付交易'))==1){
      trading_index<-stringr::str_which(tolower(raw), "产生的应支付交易")
    }
    if(length(stringr::str_which(tolower(raw), '交易基金产生的交'))==1){
      trading_index<-stringr::str_which(tolower(raw), "交易基金产生的交")
    }

    trading_trans<-stringr::str_which(tolower(raw), "基金产生的转换")

    if(length(trading_index)==0 &length(trading_trans)==0){
      data_table<-rbind(data_table,rep(NA,3),rep(NA,3))
    }
    else if(nrow(data_table)==6){
      if(length(trading_index)==0 &length(trading_trans)==1){
        data_table<-rbind(data_table,rep(NA,3))
        data_table<-data_table[c(1:5,7,6),]}
      else{data_table<-rbind(data_table,rep(NA,3))}}
    else{
      if(trading_index<trading_trans){#交易费在前
      }
      else{#转换费在前
        data_table<-data_table[c(1:5,7,6),]
      }
    }

    data_table[,1]<-c('申购费','赎回费','销售服务费','支付管理费','支付托管费','交易费','转换费')
    data_table<-data_table[c(4,5,3,1,2,6,7),]
    output_table<-data_table
  }
  colnames(output_table)<-c('var','current','past')
  return(output_table)
}
