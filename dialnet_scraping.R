library(curl); library(xml2); library(rvest);library(stringr)

#============================ General function definition ============================
full_links <- function(links){
  
  for(i in 1:length(links)){
    links[i] <- paste("https://dialnet.unirioja.es", links[i], sep = "")
  }
  return(links)
}# end of full_links

recursive_df <- function(url, df){
  # Recursive functions that captures posterior pages from the current
  # query.Takes de next page url as and argument, and the current df, 
  # which gets update with new data in every loop
  
  page <- read_html(url)
  closeAllConnections()
  
  titles <- html_text(html_nodes(page, "span.titulo"  )  )
  links <- html_attr( html_nodes(page, "div.descripcion > p.titulo > span > a" ) , "href" )
  links <- full_links(links)
  
  aux_df <- data.frame(links, titles)
  
  # Current df gets updated
  df <- rbind(df, aux_df)
  
  # Current df gets updated
  next_page <- html_attr(html_node(page, "ul.multipagina > li:nth-child(4) > a") , "href")
  
  # evaluates if theres another page, if true, return the df
  if( is.na(next_page) ){
    return(df)
  } else {
    
    next_page <- paste("https://dialnet.unirioja.es", next_page, sep = "")
    recursive_df(next_page, df)
  }
  
} # end of recursive_df

dialnet_df <- function(url){
  
  page <- read_html(url)
  closeAllConnections()
  
  titles <- html_text(html_nodes(page, "span.titulo"))
  links <- html_attr( html_nodes(page, "div.descripcion > p.titulo > span > a" ) , "href" )
  links <- full_links(links)
  df <- data.frame(links, titles)
  
  # get the next page of the current page
  next_page <- html_attr(html_node(page, "ul.multipagina > li:nth-child(4) > a") , "href")
  
  # evaluates if theres another page, if true, return the df
  if( is.na(next_page) ){
    return(df)
  } else {
    next_page <- paste("https://dialnet.unirioja.es", next_page, sep = "")
    recursive_df(next_page,df) 
  }
}# end of dialnet_df

#============================ DIALNET.UNIRIOJA.ES SEARCH Nº1 ============================
#                            Search for "deserción" & "Chile"
#                             Date of search 10-07-2019

url <- "https://dialnet.unirioja.es/buscar/documentos?camposOrdenacion=%7BDOCUMENTAL_SORT_SCORE%3DDESC%7D&registrosPorPagina=50&querysDismax.DOCUMENTAL_TODO=%22deserci%C3%B3n%22%20AND%20%22Chile%22&filtros.DOCUMENTAL_FACET_ENTIDAD=artrev"

df.dialnet_1 <- dialnet_df(url)


#============================ DIALNET.UNIRIOJA.ES SEARCH Nº2 ============================
#                         Search for "fracaso escolar" & "Chile"
#                              Date of search 10-07-2019

url <- "https://dialnet.unirioja.es/buscar/documentos?camposOrdenacion=%7BDOCUMENTAL_SORT_SCORE%3DDESC%7D&registrosPorPagina=50&querysDismax.DOCUMENTAL_TODO=%22fracaso%20escolar%22%20AND%20%22Chile%22&filtros.DOCUMENTAL_FACET_ENTIDAD=artrev"

df.dialnet_2 <- dialnet_df(url)


#============================ DIALNET.UNIRIOJA.ES SEARCH Nº3 ============================
#                       Search for "educación de adultos" & "Chile"
#                           Date of search 10-07-2019

url <- "https://dialnet.unirioja.es/buscar/documentos?camposOrdenacion=%7BDOCUMENTAL_SORT_SCORE%3DDESC%7D&registrosPorPagina=50&querysDismax.DOCUMENTAL_TODO=%22educaci%C3%B3n%20de%20adultos%22%20AND%20%22Chile%22&filtros.DOCUMENTAL_FACET_ENTIDAD=artrev"

df.dialnet_3 <- dialnet_df(url)

#============================ DIALNET.UNIRIOJA.ES SEARCH Nº4 ============================
#                         Search for "school dropout" & "Chile"
#                             Date of search 10-07-2019

url <- "https://dialnet.unirioja.es/buscar/documentos?filtros.DOCUMENTAL_FACET_ENTIDAD=artrev&querysDismax.DOCUMENTAL_TODO=%22school+dropout%22+AND+%22Chile%22&registrosPorPagina=50&camposOrdenacion=%7BDOCUMENTAL_SORT_SCORE%3DDESC%7D"

df.dialnet_4 <- dialnet_df(url)

#============================ DIALNET.UNIRIOJA.ES SEARCH Nº5 ============================
#                         Search for "school failure" & "Chile"
#                             Date of search 10-07-2019

url <- "https://dialnet.unirioja.es/buscar/documentos?filtros.DOCUMENTAL_FACET_ENTIDAD=artrev&querysDismax.DOCUMENTAL_TODO=%22school+failure%22+AND+%22Chile%22&registrosPorPagina=50&camposOrdenacion=%7BDOCUMENTAL_SORT_SCORE%3DDESC%7D"

df.dialnet_5 <- dialnet_df(url)

#============================ DIALNET.UNIRIOJA.ES SEARCH Nº6 ============================
#                       Search for "adult education" & "Chile"
#                           Date of search 10-07-2019

url <- "https://dialnet.unirioja.es/buscar/documentos?camposOrdenacion=%7BDOCUMENTAL_SORT_SCORE%3DDESC%7D&registrosPorPagina=50&querysDismax.DOCUMENTAL_TODO=%22adult%20education%22%20AND%20%22Chile%22&filtros.DOCUMENTAL_FACET_ENTIDAD=artrev"

df.dialnet_6 <- dialnet_df(url)


#============================ Consolidating and exporting data ============================

# merging all the df
final_dialnet_df <- rbind(df.dialnet_1, df.dialnet_2, df.dialnet_3,
                          df.dialnet_4, df.dialnet_5, df.dialnet_6)

write.table(final_dialnet_df, "dialnet_data.csv", sep = ";", row.names=F)


