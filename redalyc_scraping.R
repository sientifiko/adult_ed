
library(curl); library(xml2); library(rvest);library(stringr)


#============================ General function definition ============================

get_titles <- function(page){
  link <- read_html(page)
  titles <- html_text(html_nodes(page, "span.link-mostrar-ventana.txt-titulo-art"))
  rm(link)
  Sys.sleep(2)
  return(titles)
} # end of get_titles

clean_vector <- function(links){
  for(i in 1:length(links)){
    print(paste("ingresó a clean y revisó ", links[i]))
    if(str_detect(links, "BusquedaAutorPorNombre")[i] ){
      links <- links[-i]
    }
  }
  return(links)
} # end of clean_vector

get_links <- function(page){
  link <- read_html(page)
  # Sys.sleep(2)
  
  aux_links <- html_attr( html_nodes(link, "table > tbody > tr:nth-child(1) > td > a"  )  , "href")
  aux_links <-  setdiff(aux_links, y = "#panel" )
  aux_links <- aux_links[1:(length(aux_links)-2)]
  rm(link)
  for(i in 1:length(aux_links)){
    if( !is.na( str_match(aux_links[i], "articulo")[1] )   ){
      aux_links[i] <- paste("http://www.redalyc.org/", aux_links[i], sep = "")
    }
  }
  aux_links <- clean_vector(aux_links)
  return(aux_links)
}# end of get_links

redalyc_df <- function(url, name){
  
  download_html(url, file = paste("htmls/", name, sep = "" ), quiet = F)
  page <- read_html( paste("htmls/", name, sep = "" ) )
  
  #page <- read_html(url)
  
  num.page <- str_remove_all( html_text( html_node(page, 
                                                   "tr > td:nth-child(4).btn-art-activo")), 
                              pattern = "[\t \n]")
  rm(page)
  if(is.na(num.page) ){
    titles <- get_titles(page)
    links <- get_links(page)
  } else{
    basic.link <- substring(url, first = 1, last =(str_count(url)-1))
    titles <- c()
    links <- c()
    for (i in 1:as.numeric(num.page)) {
      #print(paste("di vuelta ", i))
      titles <- append(titles, get_titles(paste(basic.link, i, sep = "")))
      links <- append(links, get_links(paste(basic.link, i, sep= "" )))
      Sys.sleep(1)
    } # end of loop
  }# end of if else
  return(data.frame(links, titles))
}# end of redalyc_df


#============================ SCIELO.ORG SEARCH Nº1 ============================
#                        Search for "deserción" & "Chile"
#                          Date of search 09-07-2019

url <- "https://www.redalyc.org/busquedaArticuloFiltros.oa?q=%22desercion%20AND%20Chile%22&a=&i=&d=&cvePais=&idp=1"

df.redalyc_1 <- redalyc_df(url, "search_1.html")


#============================ SCIELO.ORG SEARCH Nº2 ============================
#                        Search for "fracaso escolar" & "Chile"
#                          Date of search 09-07-2019

url <- "https://www.redalyc.org/busquedaArticuloFiltros.oa?q=%22fracaso%20escolar%20AND%20Chile%22&idp=1"

df.redalyc_2 <- redalyc_df(url, "search_2.html")


#============================ SCIELO.ORG SEARCH Nº3 ============================
#                       Search for "educación de adultos" & "Chile"
#                           Date of search 09-07-2019

url <- "https://www.redalyc.org/busquedaArticuloFiltros.oa?q=%22educaci%C3%B3n%20de%20adultos%20AND%20Chile%22&idp=1"

df.redalyc_3 <- redalyc_df(url, "search_3.html")
