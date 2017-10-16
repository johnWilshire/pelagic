
library(plyr)
library(dplyr)
library(rvest)
library(purrr)
library(stringr)

url <- "http://www.sossa-international.org/forum/content.php?325-1999-wollongong1"

years <- read_html(url)


years %>% html_nodes('a') %>% 
  keep(~ str_detect(html_text(.x),
                    "[0-9]{4} (Wollongong|/*Kiama)+$")) %>% 
  keep(~ !is.na(html_attr(.x, "title"))) %>%
  html_attr("href") -> year_urls

month_links <- function(year_page) year_page %>%
  html_nodes(css = '.cms_article_readmore > a') %>% 
  html_attr('href')


(plyr::llply(year_urls, function(url) {
  Sys.sleep(2) # wait 2 seconds
  as.character(read_html(url) %>% month_links)
}, .progress = "text") -> trip_links)
trip_links %>% unlist -> trip_links


# way 1 using this regex, just add birds
birds <- paste(c("Cormorant",
           "Albatross",
           "Tern",
           "Shearwater",
           "Gannet",
           "Gull",
           "Petrel", 
           "Pelican"), collapse = "|") %>% tolower()

trips <- ldply(trip_links, function(url) {
  Sys.sleep(1)
  read_html(url) %>% 
    html_nodes(css = '.article') %>%
    html_text() %>% 
    str_split("\n") %>%
    unlist %>% 
    tolower %>% 
    keep(~ nchar(.x) < 100) %>% 
    # way 1
    # keep(~ str_detect(.x, birds)  
    # way 2 using the sp code that should be present 
    keep(~ str_detect(.x, "^[0-9]{3}")) -> observations
  print(url)
  print(observations )
  if(length(observations) == 0)
    return(data.frame(url, observation = "parsing_failed"))
  data.frame(url, observation = observations)
}, .progress = "text")


missing <- trips %>% filter(observation == "parsing_failed")

months <- paste0(collapse = "|", c(month.name, month.abb))  %>% tolower
date_extract <- function(x) str_extract(x, paste0("[0-9]{1,}(th|rd|st|nd)*-(",months,")-[0-9]{4}"))

trips %>%
  filter(observation != "parsing_failed") %>% 
  mutate(
    url = url %>% tolower(),
    date = date_extract(url),
    observation = observation %>% str_trim(),
    id_code = str_extract(observation, "^[0-9]{3,}") %>% str_trim,
    count = str_extract(observation, " [0-9+]+ *") %>% str_trim,
    max_at_once = str_extract(observation, "\\([0-9+]+\\)") %>%
      str_extract('[0-9]+') %>% str_trim,
    notes = str_extract(observation, "[^\\)][a-z-,&; ]*$") %>% str_trim
  ) -> good

write.csv(good, "good.csv")
write.csv(missing, "missing.csv")
