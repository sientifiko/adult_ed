
library(curl); library(xml2); library(rvest);library(stringr)

#============================ General function definition ============================



google_scholar_df <- function(url, iterations){
  
  # ten iterations by default
  if(missing(iterations) ){
    iterations <- 10
  }
  
  page <- read_html(url)
  closeAllConnections()
  
  titles <- c()
  links <- c()
  
  # this loop fills the titles and links vector
  for(i in 1:iterations){
    
  }
  
  
  titles <- html_text(html_nodes(page, "h3.gs_rt" ))
  links <- html_attr(html_nodes(page,"h3.gs_rt > a"), "href")
  
  df <- data.frame(links, titles)
  
}# end of google_scholar_df


clean_text(titles){
  titles <- gsub("PDF","", titles)
  titles <- gsub("HTML", "",titles)
  titles <- gsub("\\[|\\]", "", titles)
  
  return(titles)
} # end of clean_text

next_link <- function(url, i){
  # this functions creates the new link to
  # navigate the current query
  if(!is.na( str_locate(url, "TO_REPLACE")[1] ) ){
    
  } else {
    
    url <- str_replace(url, "0", "TO_REPLACE" )
    next_link(url,i)
  }
  
  
}# end of next_link



#============================ SCHOLAR.GOOGLE.CL SEARCH Nº1 ============================
#                        Search for "deserción" & "Chile"
#                          Date of search 10-07-2019

url <- "https://scholar.google.cl/scholar?start=0&q=deserci%C3%B3n+AND+Chile&hl=es&as_sdt=1,5"


page <- read_html(url)
closeAllConnections()

titles <- html_text(html_nodes(page, "h3.gs_rt" ))
links <- html_attr(html_nodes(page,"h3.gs_rt > a"), "href")

str_locate(url, "0")[1]
is.na( str_locate(url, "TO_REPLACE")[1] )

url2 <- str_replace(url, "0", "TO_REPLACE" )

str_replace(url2, "TO_REPLACE", "40")
