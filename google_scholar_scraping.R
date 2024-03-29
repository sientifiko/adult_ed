
library(curl); library(xml2); library(rvest);library(stringr);library(curl)


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
    
    aux_links <- html_nodes(page,"h3.gs_rt")
    
    if(length( which( is.na( html_node( aux_links, "a" ) )) ) != 0){
      i_remove <- c(which(is.na(html_node(aux_links, "a"))))
      aux_links <- aux_links[-i_remove]
    }
    
    # auxiliar self-overwriting vectors
    aux_titles <- html_text(aux_links)
    aux_links <- html_attr(html_node(aux_links, "a"), "href")
    
    # appending titles and links vectors with the auxiliars
    titles <- append(titles, aux_titles)
    links <- append(links, aux_links)
    Sys.sleep(2)
    page <- read_html( str_replace(url, "TO_REPLACE", paste0(p, "0") )  )
    closeAllConnections()
    
    p <- p + 1
  }# end of loop
  
  titles <- clean_vector(titles)
  
  return( data.frame(links, titles) )
  
}# end of google_scholar_df


clean_vector <- function(titles){
  # for (i in 1:length(titles)) {  #   
  #   val <- ( str_detect(titles, "CITAS")[i] |  str_detect(titles, "CITATION")[i] )  #   
  #   if(is.na(val|missing(val))){
  #     val <- T
  #   }  #   
  #   if( val ){
  #     print(paste("removing: ", titles[i], "N�:", i))
  #     titles[i] <- "to_delete"
  #   }
  # }
  # if(length(which(titles == "to_delete")>0)){
  #   titles <- titles[-c(which(titles == "to_delete"))]
  # }
  
  
  # remove unnecesary characters from titles
  titles <- gsub("PDF","", titles)
  titles <- gsub("HTML", "",titles)
  titles <- gsub("\\[|\\]", "", titles)
  titles <- gsub("LIBROB", "",titles)
  titles <- gsub("BOOKB", "",titles)
  
  return(titles)
} # end of clean_vector


  
#============================ SCHOLAR.GOOGLE.CL SEARCH N�1 ============================
#                        Search for "deserci�n" & "Chile"
#                          Date of search 23-07-2019

url <- "https://scholar.google.cl/scholar?start=0&q=deserci%C3%B3n+AND+Chile&hl=es&as_sdt=1,5"

df.gscholar_1 <- google_scholar_df(url)


#============================ SCHOLAR.GOOGLE.CL SEARCH N�2 ============================
#                          Search for "fracaso escolar" & "Chile"
#                              Date of search 23-07-2019

url <- "https://scholar.google.cl/scholar?start=0&q=%22fracaso+escolar%22+AND+Chile&hl=en&as_sdt=1,5"

df.gscholar_2 <- google_scholar_df(url)


#============================ SCHOLAR.GOOGLE.CL SEARCH N�3 ============================
#                       Search for "educaci�n de adultos" & "Chile"
#                           Date of search 24-07-2019

url <- "https://scholar.google.cl/scholar?start=0&q=%22educaci%C3%B3n+de+adultos%22+AND+Chile&hl=en&as_sdt=1,5"

df.gscholar_3 <- google_scholar_df(url)




