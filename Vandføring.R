library(tidyverse);library(readxl);library(lubridate);library(patchwork)

read_excel("Data/Vandløbsdata/Pøleå/Pøle å, Bendstrup.xlsx") %>% 
  mutate(date = dmy_hm(`Dato (DK normaltid)`),
         date = as.Date(date),
         year = year(date),
         month = month(date)) %>% 
  select(date, discharge = 2, year,month) -> bendstrup

read_excel("Data/Vandløbsdata/Æbelholt Å/49000061_Vandforing, Dognmiddel (DMP)_Dag.xlsx") %>% 
  mutate(date = dmy_hm(`Dato (DK normaltid)`),
         date = as.Date(date),
         year = year(date),
         month = month(date)) %>% 
  select(date, discharge = 2, year,month)-> æbelholt


bendstrup %>% 
  reframe(median = median(discharge, na.rm=T),
          max = max(discharge, na.rm=T),
          perc95 = quantile(discharge, probs = 0.95, na.rm=T),
                  .by = c(year,month)) %>% 
  arrange(year,month) -> bend_table

æbelholt %>% 
  reframe(median = median(discharge, na.rm=T),
          max = max(discharge, na.rm=T),
          perc95 = quantile(discharge, probs = 0.95, na.rm=T),
          .by = c(year,month)) %>% 
  arrange(year,month) -> æbel_table


full_join(bend_table,æbel_table, 
            by = join_by(year,month)) %>% 
  write_csv("vandføring_median.csv")

æbel_table %>% 
  reframe(perc95_median = mean(perc95/median),
          max_median = mean(max/median),
                .by = year) -> year_æbel
bend_table %>% 
  reframe(perc95_median = mean(perc95/median),
        max_median = mean(max/median),
        .by = year) -> year_bend

full_join(year_æbel,year_bend,
                by = join_by(year)) %>% arrange(year) %>% write_csv("yearly_percmedian.csv")

year_bend %>% 
  filter(year < 1997) %>% 
  lm(max_median ~ year, data = .) %>% summary

year_bend %>% 
  mutate(class = case_when(year < 1997 ~ "early",
                           year > 1996 ~ "late")) %>% 
  t.test(max_median~class, data = .) 

year_æbel %>% 
  filter(year > 2010) %>% 
  lm(max_median ~ year, data = .) %>% summary

year_æbel %>% 
  mutate(class = case_when(year < 2000 ~ "early",
                           between(year, 2000, 2010) ~ "mid",
                           year > 2010 ~ "late")) %>% 
  lm(max_median~class, data = .)  %>% 
  aov %>% TukeyHSD()





bend_table %>% 
  #filter(year >1996) %>% 
  mutate(max_median = max/median) %>% 
  lm(max_median ~ year, data = .) %>% summary


æbel_table %>% 
  mutate(class = case_when(year < 2000 ~ "early",
                           between(year, 2000, 2010) ~ "mid",
                           year > 2010 ~ "late"),
         max_median = max/median) %>% 
  filter(class %in% c("mid", "late")) %>% 
  lm(max_median~year, data = .) %>% summary



