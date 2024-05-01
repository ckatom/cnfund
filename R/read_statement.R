#' Read the text of fund's statement
#'
#' @param file_name Fund statement's filename or url
#'
#' @return Text string data
#' @export
#'
#' @examples example_file_loc<-(system.file("pdfdata", "014645.OF.pdf", package = "cnfunds"))
#' pdf_content<-read_statement(paste(example_file_loc,"//doc//","014645.OF.pdf",sep=''))

read_statement<-function(file_name){  # Read the fund statement pdf files on url or local files
  suppressWarnings({raw<-pdf_text(file_name)
  # Split the single pages
  raw <- map(raw, ~ str_split(.x, "\\n") %>% unlist())})
  # Concatenate the splitted pages
  raw <- reduce(raw, c)
  return(raw)
}
