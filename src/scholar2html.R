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
# https://scholar.google.com/citations?user=b8bWNkUAAAAJ&hl=en
user <- "XKCyZk0AAAAJ"

# pull from google
html_1 <- get_publications(user)

# convert to htlm table - the ugly way ;)
html_2 <- html_1 %>%
  as_tibble %>% arrange(desc(year)) %>%
  mutate(
    #    author=str_replace_all(author, " (\\S) ", "\\1 "),
    author=str_replace_all(author, "([A-Z]) ([A-Z]) ", "\\1\\2 "),
    author=str_replace_all(author, ", \\.\\.\\.", " et al."),
    author=str_replace_all(author, "Airoldi", "<b>Airoldi</b>") # make my name fat
  ) %>% split(.$year) %>%
  map(function(x){
    x <- x %>%
      glue_data('<li width="100%">{author} ({year}) <a href="https://scholar.google.com/scholar?oi=bibs&cluster={cid}&btnI=1&hl=en">{title}</a>, {cites} Citations, {journal}, {number}</li>') %>%
      str_replace_all("(, )+</li>", "</li>") %>%
      char2html()
    #x <- c('<table class="publication-table" border="10px solid blue" cellspacing="0" cellpadding="6" rules="", frame=""><tbody>', x, '</tbody></table>')
    return(x);
  }) %>% unlist %>% rev 

html_3 <- c('<ol reversed class="publication-table" border="10px solid blue" cellspacing="0" cellpadding="6" rules="", frame="">', html_2,'</ol>')

html_4 <- c(
  paste0('<p style="text-align: center; margin-top: 40px;"><small>Last updated <i>',
         format(Sys.Date(), format="%B %d, %Y"),
         '&ndash; Pulled automatically from my <a href="https://scholar.google.com/citations?hl=en&user=lZUH1lcAAAAJ">Google Scholar profile</a>. <!--See <a href="https://thackl.github.io/automatically-update-publications-with-R-scholar">this post</a> for how it works.--></i></small></p>'), html_3)

# write the html list to a file
writeLines(html_4, "/Volumes/Data/GitHub/airoldi.github.io/src/html4.html")
