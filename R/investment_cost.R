
#' Calculate the trading cost during fund investment
#'
#' @param raw Text content from pdf file
#'
#' @return Investment cost data
#' @export
#'
#' @examples
#' data(pdf_content)
#' investment_cost(pdf_content)

investment_cost<-function(raw){   # extract trading fund cost data

  table_start <- str_which(tolower(raw), "卖出/赎回基金成交总额")
  table_end <- str_which(tolower(raw), "减：交易费用")+1
  table_end <- table_end[min(which(table_end > table_start))]
  table <- raw[(table_start ):(table_end)]
  suppressWarnings({
  table <- str_replace_all(table, "\\s{2,}", "|")
  text_con <- textConnection(table)
  data_table <- read.csv(text_con, sep = "|",row.names=NULL)
  })

  if(ncol(data_table)==4){
    data_table<-data_table[which(data_table[,3]!=""),]
    return(as.numeric(gsub(',','',data_table[which(data_table[,2]=='减：交易费用'),3])))
  }else{
    data_table<-data_table[which(data_table[,2]!=''),]

    return(data_table[which(data_table[,1]=='减：交易费用'),2])
  }

}
