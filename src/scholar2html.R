# H/T Thomas Hackl, https://thackl.github.io/
# See https://thackl.github.io/automatically-update-publications-with-R-scholar for how it works.
# Thanks to Jared Stefanowicz, http://jaredstef.me/

library(scholar)
library(tidyverse)
library(glue)

# escape some special chars, german umlauts, ...
char2html <- function(x){
  dictionary <- data.frame(
    symbol = c("ä","ö","ü","Ä", "Ö", "Ü", "ß"),
    html = c("&auml;","&ouml;", "&uuml;","&Auml;",
             "&Ouml;", "&Uuml;","&szlig;"))
  for(i in 1:dim(dictionary)[1]){
    x <- gsub(dictionary$symbol[i],dictionary$html[i],x)
  }
  x
}

# my google scholar user id from my profile url
# https://scholar.google.com/citations?hl=en&user=XKCyZk0AAAAJ&view_op=list_works&sortby=pubdate
user <- "XKCyZk0AAAAJ"

# pull from google
html_1 <- get_publications(user)

###############################################################
### convert to htlm table -- formatter for the archive page ###
###############################################################
html_2_arx <- html_1 %>%
  as_tibble %>% arrange(desc(year)) %>%
  mutate(
    author=str_replace_all(author, "([A-Z]) ([A-Z]) ", "\\1\\2 "),
    author=str_replace_all(author, ", \\.\\.\\.", " et al."),
    author=str_replace_all(author, "E Airoldi", "<i><u>E Airoldi</u></i>"), # make my name fat
    author=str_replace_all(author, "EM Airoldi", "<i><u>EM Airoldi</u></i>"), # make my name fat
    journal=paste0('<b><i>', journal, '</i></b>') # bold/italicize pub venues
  ) %>% split(.$year) %>%
  map(function(x){
    x <- x %>%
      glue_data('<li><p><a href="https://scholar.google.com/scholar?oi=bibs&cluster={cid}&btnI=1&hl=en" target="_blank">{title}</a> &nbsp;&nbsp;&nbsp; ({cites} cit.)<br>{journal}, {number}, {year}<br>By {author}</p></li>') %>%
      str_replace_all("(, )+</li>", "</li>") %>%
      char2html()
    #x <- c('<table class="publication-table" border="10px solid blue" cellspacing="0" cellpadding="6" rules="", frame=""><tbody>', x, '</tbody></table>')
    return(x);
  }) %>% rev 

html_3_arx <- paste0(map2(names(html_2) %>% paste0("<h3><b><u>", ., "</u></b></h3>"), html_2_arx, c) %>% unlist, collapse = "\n")
html_3_arx <- paste('<ol reversed class="publication-table" border="10px solid blue" cellspacing="0" cellpadding="6" rules="", frame="">\n',html_3_arx,'</ol>')

html_4_arx <- 
  paste0('<p style="text-align: center; margin-top: 40px;"><small>Last updated <i>',
         format(Sys.Date(), format="%B %d, %Y"),
         '&ndash; Pulled automatically from <a href="https://scholar.google.com/citations?hl=en&user=b8bWNkUAAAAJ">Google Scholar</a>.</i></small></p>
         <p>Citaions: ',sum(html_1$cites),'<br>
            h-index: ',sum(sort(html_1$cites, decreasing=TRUE)>=seq(length(html_1$cites))),'<br>
            i10-index: ',sum(html_1$cites>=10),'</p>', html_3_arx, collapse="")

# write the html list to a file
writeLines(html_4_arx, "/Volumes/Data/GitHub/airoldi.github.io/src/html4_arx.html")


################################################################
### convert to htlm table -- formatter for the research page ###
################################################################
html_2_res <- html_1 %>%
  as_tibble %>% arrange(desc(year)) %>%
  mutate(
    author=str_replace_all(author, "([A-Z]) ([A-Z]) ", "\\1\\2 "),
    author=str_replace_all(author, ", \\.\\.\\.", " et al."),
    journal=paste0('<b><i>', journal, '</i></b>') # bold/italkicize pub venues
  ) %>% split(.$year) %>%
  map(function(x){
    x <- x %>%
      glue_data('<li><p><a href="https://scholar.google.com/scholar?oi=bibs&cluster={cid}&btnI=1&hl=en" target="_blank">{title}</a>, {journal}, {number}, {year}. &nbsp; ({cites} cit.)</p></li>') %>%
      str_replace_all("(, )+</li>", "</li>") %>%
      char2html()
    return(x);
  }) %>% rev 

html_3_res <- paste0(map2(names(html_2) %>% paste0("<h3>", ., "</h3>"), html_2_res, c) %>% unlist, collapse = "\n")
html_3_res <- paste('<ol reversed class="publication-table" border="10px solid blue" cellspacing="0" cellpadding="6" rules="", frame=""><br>',html_3_res,'</ol>')

html_4_res <- 
  paste0('<p style="text-align: center; margin-top: 40px;"><small>Last updated <i>',
         format(Sys.Date(), format="%B %d, %Y"),
         '&ndash; Pulled automatically from <a href="https://scholar.google.com/citations?hl=en&user=b8bWNkUAAAAJ">Google Scholar</a>.</i></small></p>', html_3_res, collapse="")

# write the html list to a file
writeLines(html_4_res, "/Volumes/Data/GitHub/airoldi.github.io/src/html4_res.html")

