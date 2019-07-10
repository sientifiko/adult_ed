library(curl); library(xml2); library(rvest);library(stringr)

#============================ General function definition ============================

scielo_df <- function(url){
  
  # this function retrieve the titles, links and abstract of 
  # every paper. It receive as an argument the search URL
  # and return the df with the three elements
  
  # Read the page
  page <- read_html(url)
  
  # Get the titles of the papers
  titulos <- html_text(html_nodes(page, "strong.title"  )  )
  
  # Get the links of the papers
  links <- html_attr( html_nodes(page, "div.col-md-11.col-sm-10.col-xs-11 > div:nth-child(1) > a" ),
                      "href"  )
  
  # Get de abstracts. 
  abstracts <- html_text(html_nodes(page,  "div.col-md-11.col-sm-10.col-xs-11 > div.user-actions"  ) )
  
  # Cleaning the text retrieved
  abstrac <- str_replace_all(abstracts,"[\n\t]", "")
  abstrac <- gsub("· Espanhol:" ,  "",abstrac)
  abstrac <- gsub("Texto",  "",abstrac)
  abstrac <- gsub("Resumo",  "",abstrac)
  abstrac <- gsub("PDF",  "",abstrac)
  abstrac <- gsub("· Inglês:",  "",abstrac)
  abstrac <- gsub("· Francês:", "", abstrac)
  abstrac <- gsub("· Português:", "", abstrac)
  abstrac <- str_trim(abstrac)
  
  return(data.frame(links,titulos,abstrac))
}# end of scielo_df



#============================ SCIELO.ORG SEARCH Nº1 ============================
#                        Search for "deserción" & "Chile"
#                          Date of search 01-07-2019

# Search URL
url <- "https://search.scielo.org/?q=%28deserci%C3%B3n%29+AND+%28Chile%29&lang=pt&count=50&from=0&output=site&sort=&format=summary&fb=&page=1&q=%28deserci%C3%B3n%29+AND+%28Chile%29&lang=pt&page=1"

# asigning to a data frame
df.scielo_1 <- scielo_df(url)

#============================ SCIELO.ORG SEARCH Nº2 ============================
#                        Search for "fracaso escolar" & "Chile"
#                          Date of search 05-07-2019

url <- "https://search.scielo.org/?q=%28%22fracaso+escolar%22%29+AND+%28Chile%29&lang=pt&count=50&from=0&output=site&sort=&format=summary&fb=&page=1&q=%28%22fracaso+escolar%22%29+AND+%28Chile%29&lang=pt&page=1"

# asigning to a data frame
df.scielo_2 <- scielo_df(url)


#============================ SCIELO.ORG SEARCH Nº3 ============================
#                       Search for "educación de adultos" & "Chile"
#                           Date of search 05-07-2019

url <- "https://search.scielo.org/?q=%28%22educaci%C3%B3n+de+adultos%22%29+AND+%28Chile%29&lang=pt&count=50&from=0&output=site&sort=&format=summary&fb=&page=1&q=%28%22educaci%C3%B3n+de+adultos%22%29+AND+%28Chile%29&lang=pt&page=1"

# asigning to a data frame
df.scielo_3 <- scielo_df(url)


#============================ SCIELO.ORG SEARCH Nº4 ============================
#                       Search for "school dropout" & "Chile"
#                           Date of search 05-07-2019

url <- "https://search.scielo.org/?q=%28%22school+dropout%22%29+AND+%28Chile%29&lang=pt&count=50&from=0&output=site&sort=&format=summary&fb=&page=1&q=%28%22school+dropout%22%29+AND+%28Chile%29&lang=pt&page=1"

# asigning to a data frame
df.scielo_4 <- scielo_df(url)


#============================ SCIELO.ORG SEARCH Nº5 ============================
#                       Search for "school failure" & "Chile"
#                           Date of search 05-07-2019


url <- "https://search.scielo.org/?q=%28%22school+failure%22%29+AND+%28Chile%29&lang=pt&count=50&from=0&output=site&sort=&format=summary&fb=&page=1&q=%28%22school+failure%22%29+AND+%28Chile%29&lang=pt&page=1"

# asigning to a data frame
df.scielo_5 <- scielo_df(url)


#============================ SCIELO.ORG SEARCH Nº6 ============================
#                       Search for "adult education" & "Chile"
#                           Date of search 05-07-2019

url <- "https://search.scielo.org/?q=%28%22adult+education%22%29+AND+%28Chile%29&lang=pt&count=50&from=0&output=site&sort=&format=summary&fb=&page=1&q=%28%22adult+education%22%29+AND+%28Chile%29&lang=pt&page=1"

# asigning to a data frame
df.scielo_6 <- scielo_df(url)


#============================ Consolidating and exporting data ============================

# merging all the df
final_scielo_df <- rbind(df.scielo_1, df.scielo_2, df.scielo_3,
                         df.scielo_4, df.scielo_5, df.scielo_6)

# exporting to .csv file
write.table(final_scielo_df, "scielo_data.csv", sep = ";", row.names = F)


