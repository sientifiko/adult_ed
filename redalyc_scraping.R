
library(curl); library(xml2); library(rvest);library(stringr)

#============================ General function definition ============================

redalyc_df <- function(url){
  
}


url <- "https://www.redalyc.org/busquedaArticuloFiltros.oa?q=%22desercion%20AND%20Chile%22&a=&i=&d=&cvePais=&idp=1"
page <- read_html( url )
num.page <- str_remove_all( html_text( html_node(page, "tr > td:nth-child(4).btn-art-activo"   ) ), 
                            pattern = "[\t \n]"  )



get_titles <- function(url){
  link <- read_html(url)
  titles <- html_text( html_nodes(link, "span.link-mostrar-ventana.txt-titulo-art "   )   )
  return(titles)
} # end of get_titles

get_url <- function(url){
  link <- read_html(url)
  aux_links <- html_attr( html_nodes(link, "table > tbody > tr:nth-child(1) > td > a"  )  , "href")
  aux_links <- aux_links[3:(length(aux_links)-4)]
  
  for(i in 1:length(aux_links)){
    if( !is.na( str_match(aux_links[i], "articulo")[1] )   ){
      aux_links[i] <- paste("http://www.redalyc.org/", aux_links[i], sep = "")
    }
  }
  return(aux_links)
}# end of get_url




a#============================ SCIELO.ORG SEARCH Nº1 ============================
#                        Search for "deserción" & "Chile"
#                          Date of search 01-07-2019










