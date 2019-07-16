
library(curl); library(xml2); library(rvest);library(stringr)

install.packages("RSelenium")

#============================ General function definition ============================



google_scholar_df <- function(url, iterations){
  
  # ten iterations by default
  if(missing(iterations) ){
    iterations <- 10
  }
  
  page <- read_html(url)
  closeAllConnections()
  
  p <- 1
  titles <- c()
  links <- c()
  url <- str_replace(url, "0", "TO_REPLACE" )
  # this loop fills the titles and links vector
  for(i in 1:iterations){
    
    to_replace <- paste(p, "0", sep = "")
    
    titles <- append(titles, html_text(html_nodes(page, "h3.gs_rt" )))
    links <- append(links, html_attr(html_nodes(page,"h3.gs_rt > a"), "href"))
    
    page <- read_html( str_replace(url, "TO_REPLACE", to_replace )  )
    closeAllConnections()
    
    p <- p + 1
  }# end of loop
  
  titles <- clean_vector(titles)
  
  return( data.frame(links, titles) )
  
}# end of google_scholar_df


clean_vector <- function(titles){
  for (i in 1:length(titles)) {
    
    val <- str_detect(titles, "CITAS")[i]
    if(is.na(val||missing(val))){
      val <- T
    }
    if( val ){
      print(paste("removing: ", titles[i]))
      titles[i] <- "to_delete"
    }
  }
  if(length(which(titles == "to_delete")>0)){
    titles <- titles[-c(which(titles == "to_delete"))]
  }
  
  # remove unnecesary characters from titles
  titles <- gsub("PDF","", titles)
  titles <- gsub("HTML", "",titles)
  titles <- gsub("\\[|\\]", "", titles)
  titles <- gsub("LIBROB", "",titles)
  
  return(titles)
} # end of clean_vector


  
#============================ SCHOLAR.GOOGLE.CL SEARCH Nº1 ============================
#                        Search for "deserción" & "Chile"
#                          Date of search 16-07-2019

url <- "https://scholar.google.cl/scholar?start=0&q=deserci%C3%B3n+AND+Chile&hl=es&as_sdt=1,5"

df.gscholar_1 <- google_scholar_df(url)


#============================ SCHOLAR.GOOGLE.CL SEARCH Nº2 ============================
#                          Search for "fracaso escolar" & "Chile"
#                              Date of search 16-07-2019

url <- "https://scholar.google.cl/scholar?start=0&q=%22fracaso+escolar%22+AND+Chile&hl=en&as_sdt=1,5"

df.gscholar_2 <- google_scholar_df(url)




