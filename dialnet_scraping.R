library(curl); library(xml2); library(rvest);library(stringr)

#============================ General function definition ============================

dialnet_df <- function(url){
  
}# end of dialnet_df


url <- "https://dialnet.unirioja.es/buscar/documentos?camposOrdenacion=%7BDOCUMENTAL_SORT_SCORE%3DDESC%7D&registrosPorPagina=50&querysDismax.DOCUMENTAL_TODO=%22deserci%C3%B3n%22%20AND%20%22Chile%22&filtros.DOCUMENTAL_FACET_ENTIDAD=artrev"

page <- read_html(url)
closeAllConnections()
titles <- html_text(html_nodes(page, "span.titulo"  )  )
titles


