library(curl); library(xml2); library(rvest);library(stringr)

#============================ General function definition ============================


eric_df <- function(url){
  
  page <- read_html(url)
  closeAllConnections()
  
  raw_url <- substr(url, start = 1, stop = (str_count(url)-1) )
  
  next_page <- paste(raw_url, 2, sep = "")
  another_page <- read_html(next_page)
  closeAllConnections()
  
  no_R <- html_text(html_node(another_page, "div.noRs"))
  
  if( !is.na(no_R) ){
    titles <- html_text(html_nodes(page, "div.r_t"  ))
    links <- html_attr( html_nodes(page, "div.r_t > a" ),"href" )
    abstracts <- html_text(html_nodes(page, "div.r_d"))
    df <- data.frame(links, titles, abstracts)
    return(df)
  } else {
   
    i <- 1
    titles <- c()
    links <- c()
    abstracts <- c()
    
    while (T) {
      
      current_page <- paste(raw_url, i, sep = "")
      page <- read_html(current_page)
      closeAllConnections()
      no_R <- html_text(html_node(page, "div.noRs"))
      
      if( !is.na(no_R) ){
        df <- data.frame(links, titles, abstracts)
        return(df)
      }
      
      titles <- append(titles, html_text(html_nodes(page, "div.r_t"  )))
      links <- append(links,  html_attr( html_nodes(page, "div.r_t > a" ),"href" ))
      abstracts <- append(abstracts, html_text(html_nodes(page, "div.r_d")))
      
      i <- i + 1
    }# end of while
    
    df <- data.frame(links, titles, abstracts)
    return(df)
    
  }
} # end of eric_df



#============================ ERIC.ED.GOV SEARCH Nº1 ============================
#                        Search for "deserción" & "Chile"
#                          Date of search 10-07-2019

# url <- "https://eric.ed.gov/?q=deserci%C3%B3n+AND+Chile"

# This query yield no results


#============================ ERIC.ED.GOV SEARCH Nº2 ============================
#                       Search for "fracaso escolar" & "Chile"
#                          Date of search 10-07-2019

# url <- "https://eric.ed.gov/?q=%22fracaso+escolar%22+AND+Chile"

# This query yield no results

#============================ ERIC.ED.GOV SEARCH Nº3 ============================
#                     Search for "educación de adultos" & "Chile"
#                          Date of search 10-07-2019


# url <- "https://eric.ed.gov/?q=%22educaci%C3%B3n+de+adultos%22+AND+Chile"

# This query yield no results

#============================ ERIC.ED.GOV SEARCH Nº4 ============================
#                     Search for "school dropout" & "Chile"
#                          Date of search 10-07-2019

url <- "https://eric.ed.gov/?q=%22school+dropout%22+AND+Chile"

# This query yield no results

#============================ ERIC.ED.GOV SEARCH Nº5 ============================
#                      Search for "school failure" & "Chile"
#                          Date of search 10-07-2019

url <- "https://eric.ed.gov/?q=%22school+failure%22+AND+Chile"

df.eric_5 <- eric_df(url)


#============================ ERIC.ED.GOV SEARCH Nº6 ============================
#                        Search for "adult education" & "Chile"
#                            Date of search 10-07-2019


url <- "https://eric.ed.gov/?q=%22adult+education%22+AND+Chile&ff1=pubReports+-+Research&pg=1"

df.eric_6 <- eric_df(url)



#============================ Consolidating and exporting data ============================

# merging all the df
final_eric_df <- rbind(df.eric_5, df.eric_6)

# exporting to .csv file
write.table(final_eric_df, "eric_data.csv", sep = ";", row.names = F)

