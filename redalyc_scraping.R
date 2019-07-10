
library(curl); library(xml2); library(rvest);library(stringr)


#============================ General function definition ============================

get_titles <- function(url){
  link <- read_html(url)
  closeAllConnections()
  titles <- html_text(html_nodes(link, "span.link-mostrar-ventana.txt-titulo-art"))
  # rm(link)
  # Sys.sleep(1)
  return(titles)
} # end of get_titles

clean_vector <- function(links){
  for(i in 1:length(links)){
    #print(paste("ingresó a clean y revisó ", links[i]))
    
    if( str_detect(links, "BusquedaAutorPorNombre")[i] ){
      links[i] <- "delete"
    }
  }
  links <- setdiff(links, y="delete")
  return(links)
} # end of clean_vector

get_links <- function(link){
  link <- read_html(link)
  # Sys.sleep(2)
  closeAllConnections()
  aux_links <- html_attr( html_nodes(link, "table > tbody > tr:nth-child(1) > td > a"  )  , "href")
  aux_links <-  setdiff(aux_links, y = "#panel" )
  aux_links <- aux_links[1:(length(aux_links)-2)]
  # rm(link)
  for(i in 1:length(aux_links)){
    if( !is.na( str_match(aux_links[i], "articulo")[1] )){
      aux_links[i] <- paste("http://www.redalyc.org/", aux_links[i], sep = "")
    }
  }
  aux_links <- clean_vector(aux_links)
  return(aux_links)
}# end of get_links

redalyc_df <- function(url){
  
  # path <- paste("htmls/", name, sep = "" )
  # download_html(url, file = path, quiet = F)
  # page <- read_html(path)
  page <- read_html(url)
  closeAllConnections()
  num.page <- str_remove_all( html_text( html_node(page, 
                                                   "tr > td:nth-child(4).btn-art-activo")), 
                              pattern = "[\t \n]")
  
  if(is.na(num.page) ){
    #Sys.sleep(1)
    titles <- get_titles(url)
    #Sys.sleep(1)
    links <- get_links(url)
  } else{
    basic.link <- substring(url, first = 1, last =(str_count(url)-1))
    titles <- c()
    links <- c()
    for (i in 1:as.numeric(num.page)) {
      #print(paste("di vuelta ", i))
      #Sys.sleep(1)
      titles <- append(titles, get_titles(paste(basic.link, i, sep = "")))
      #Sys.sleep(1)
      links <- append(links, get_links(paste(basic.link, i, sep= "" )))
    } # end of loop
  }# end of if else
  # rm(page)
  return(data.frame(links, titles))
}# end of redalyc_df


#============================ REDALYC.ORG SEARCH Nº1 ============================
#                        Search for "deserción" & "Chile"
#                          Date of search 10-07-2019

url <- "https://www.redalyc.org/busquedaArticuloFiltros.oa?q=%22desercion%20AND%20Chile%22&a=&i=&d=&cvePais=&idp=1"

df.redalyc_1 <- redalyc_df(url)


#============================ REDALYC.ORG SEARCH Nº2 ============================
#                        Search for "fracaso escolar" & "Chile"
#                          Date of search 10-07-2019

url <- "https://www.redalyc.org/busquedaArticuloFiltros.oa?q=%22fracaso%20escolar%20AND%20Chile%22&idp=1"

df.redalyc_2 <- redalyc_df(url)


#============================ REDALYC.ORG SEARCH Nº3 ============================
#                       Search for "educación de adultos" & "Chile"
#                           Date of search 10-07-2019

url <- "https://www.redalyc.org/busquedaArticuloFiltros.oa?q=%22educaci%C3%B3n%20de%20adultos%20AND%20Chile%22&idp=1"

df.redalyc_3 <- redalyc_df(url)


#============================ REDALYC.ORG SEARCH Nº4 ============================
#                       Search for "school dropout" & "Chile"
#                           Date of search 10-07-2019

url <- "https://www.redalyc.org/busquedaArticuloFiltros.oa?q=%22school%20dropout%20AND%20Chile%22&idp=1"

df.redalyc_4 <- redalyc_df(url)


#============================ REDALYC.ORG SEARCH Nº5 ============================
#                       Search for "school failure" & "Chile"
#                           Date of search 10-07-2019

url <- "https://www.redalyc.org/busquedaArticuloFiltros.oa?q=%22school%20failure%20AND%20Chile%22&idp=1"

# this query yield no results

#============================ Consolidating and exporting data ============================

# merging all the df
final_redalyc_df <- rbind(df.redalyc_1, df.redalyc_2, df.redalyc_3,
                          df.redalyc_4)

write.table(final_redalyc_df, "redalyc_data.csv", sep = ";", row.names=F)
