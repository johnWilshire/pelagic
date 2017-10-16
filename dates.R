library(ggplot2)
library(dplyr)

lower_month <- month.name %>% tolower()
good %>% transmute(
  month = str_extract(date, "-[a-z]+-") %>% str_extract("[a-z]+"),
  month_number = sapply(month, function(x) which(x == lower_month)),
  day = str_extract(date, "^[0-9]{1,2}"),
  year = str_extract(date, "[0-9]{4}$"),
  proper_date = paste(sep = "-",year, month_number, day),
  proper_date = lubridate::ymd(proper_date)
) -> good_2
  
good_2 %>% group_by(proper_date) %>% dplyr::summarise(n = n()) %>% 
  ggplot(aes(proper_date, n)) + geom_point()
