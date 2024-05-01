#' Read the text of fund's statement
#'
#' @param file_name Fund statement's filename or url
#'
#' @return Text string data
#' @export
#'
#' @examples download.file('http://www.fund.pingan.com/nasfile/1711755883661.pdf', '014645.OF.pdf')
#' pdf_content<-read_statement("014645.OF.pdf")

read_statement<-function(file_name){  # Read the fund statement pdf files on url or local files
  suppressWarnings({raw<-pdf_text(file_name)
  # Split the single pages
  raw <- map(raw, ~ str_split(.x, "\\n") %>% unlist())})
  # Concatenate the splitted pages
  raw <- reduce(raw, c)
  return(raw)
}
