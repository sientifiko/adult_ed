
library(curl); library(xml2); library(rvest);library(stringr)

#============================ General function definition ============================

google_scholar_df <- function(){
  
  
  
  
}# end of google_scholar_df

recursive_df <- function(url, df){
  
  
}# end of recurisve_df


clean_text(titles){
  titles <- gsub("PDF","", titles)
  titles <- gsub("HTML", "",titles)
  titles <- gsub("\\[|\\]", "", titles)
  
  return(titles)
} # end of clean_text

#============================ SCHOLAR.GOOGLE.CL SEARCH Nº1 ============================
#                        Search for "deserción" & "Chile"
#                          Date of search 10-07-2019

url <- "https://scholar.google.cl/scholar?start=0&q=deserci%C3%B3n+AND+Chile&hl=es&as_sdt=1,5"

page <- read_html(url)
closeAllConnections()

titles <- html_text(html_nodes(page, "h3.gs_rt" ))



