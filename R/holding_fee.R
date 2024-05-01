
#' Extract the total holding fund cost
#'
#' @param raw Text content from pdf file
#'
#' @return 3*2 dataframe, where rows corresponding to 3 types of fund cost, columns corresponding to costs' name and data
#' @export
#'
#' @examples data(pdf_content)
#' @examples holding_fee(pdf_content)

holding_fee<-function(raw){   # extract the holding fee table, return sales, manage, trust fees during holding funds
  table_start <- str_which(tolower(raw), "持有基金产生的费用")
  table_end <- table_start+26
  table <- raw[(table_start + 3 ):(table_end - 1)]
  table <- str_replace_all(table, "\\s{2,}", "|")
  text_con <- textConnection(table)
  suppressWarnings({
  data_table <- read.csv(text_con, sep = "|",row.names=NULL)
  data_table<-data_table[which(data_table[,2]!=''),]
  num_vec<-gsub(",","",data_table[,2])
  data_table<-data_table[which((!is.na(as.numeric(num_vec))) |
                                 grepl("-",data_table[,2])),]
  })
  data_table<-data_table[1:3,]
  data_table[,1]<-c('sales','manage','trust')
  data_table<-data_table[c(2,3,1),]
  colnames(data_table)<-c('type','this year','last year')
  return(data_table)
}
