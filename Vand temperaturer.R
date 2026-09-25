source("Library.R")

#### Arresø ####
read_delim("Data/Arresø data/Salt, ilt, temperatur, ph m_ling - S__20241120_123045.csv", 
           delim = ";", escape_double = FALSE, col_types = cols(Dato = col_character()), 
           locale = locale(decimal_mark = ",", grouping_mark = "."), 
           trim_ws = TRUE) %>% 
  select(date = Dato, 
         depth = Dybde,
         type = Parameter,
         value = Resultat,
         unit = Enhed) %>% 
  mutate(date = dmy(date)) %>% 
  reframe(value = mean(value, na.rm=T),
              .by = c(date,type,unit)) %>% 
  mutate(site = "Arresø") -> arre_mdb

#### Syd for Alsønderup ####
read_delim("Data/Vandløbsdata/Syd for Alsønderup/Salt, ilt, temperatur, ph m_ling - Vandl_b_20241120_122811.csv", 
           delim = ";", escape_double = FALSE, col_types = cols(Dato = col_character()), 
           locale = locale(decimal_mark = ",", grouping_mark = "."), 
           trim_ws = TRUE) %>% 
  filter(Parameter == "Temperatur") %>% 
  select(date = Dato, 
         type = Parameter,
         value = Resultat,
         unit = Enhed) %>% 
  mutate(date = dmy(date)) %>% 
  mutate(site = "Syd for Alsønderup")-> syd_alsonderup

#### Æbelholt ####
read_delim("Data/Vandløbsdata/Æbelholt Å/Salt, ilt, temperatur, ph m_ling - Vandl_b_20241120_122743.csv", 
           delim = ";", escape_double = FALSE, col_types = cols(Dato = col_character()), 
           locale = locale(decimal_mark = ",", grouping_mark = "."), 
           trim_ws = TRUE) %>% 
  filter(Parameter == "Temperatur") %>% 
  select(date = Dato, 
         type = Parameter,
         value = Resultat,
         unit = Enhed) %>% 
  mutate(date = dmy(date)) %>% 
  mutate(site = "Æbelholt Å")-> æbel


#### Combine ####
bind_rows(arre,syd_alsonderup,æbel) %>% 
  mutate(month = month(date),
         year = year(date)) %>% 
  filter(between(month,5,9)) %>% 
  reframe(avg_temp = mean(value,na.rm=T),
          min_temp = min(value, na.rm=T),
          max_temp = max(value,na.rm=T),
            .by = c(site,year)) -> sommer_temps

sommer_temps %>% 
  ggplot(aes(year, fill = site)) + 
  geom_line(aes(y = avg_temp,col = site)) + 
  geom_ribbon(aes(ymin = min_temp, ymax = max_temp), alpha = 0.5, show.legend = F) + 
  tema + 
  facet_wrap(~site, ncol = 1) + 
  labs(x = "",
       y = "Sommer temperatur",
       col = "")

sommer_temps %>% 
  filter(site == "Syd for Alsønderup") %>% 
  lm(data = ., avg_temp~year) %>% summary
