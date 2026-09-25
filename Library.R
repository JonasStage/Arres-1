library(leaflet);library(sf);library(tidyverse);library(lubridate);library(readxl);library(mapview);library(patchwork);library(ggpmisc)
source("ggplot_themes.R")
options(scipen=10000)

stream_load_function <- function(kemi_path, discharge_path,kemi_date_min, kemi_date_max){
kemi <- read_delim(kemi_path, 
                        delim = ";", escape_double = FALSE, col_types = cols(Dato = col_character(), Resultat = col_double()), 
                        locale = locale(decimal_mark = ",", grouping_mark = "."), trim_ws = TRUE) %>% 
  filter(Analysefraktion == "Total") %>% 
  select(date = Dato, 
         parameter = Stofparameter,
         value = Resultat,
         unit = Enhed) %>% 
  mutate(date = dmy(date)) %>% 
  filter(parameter %in% c("Nitrogen,total N","Phosphor, total-P"),
         between(date, kemi_date_min, kemi_date_max))

discharge <- read_excel(discharge_path) %>% 
  mutate(date = dmy_hm(`Dato (DK normaltid)`),
         discharge = `Vandføring Døgnmiddel (DMP) (l/s)`,
         date = as.Date(date)) %>% 
  select(date,discharge) 

ggplot() + 
  geom_point(data = kemi, aes(date,value)) + 
  geom_line(data = discharge, aes(date, discharge)) +
  facet_wrap(~parameter, scales = "free", ncol = 1)

seq(ymd(min(kemi$date)),ymd(max(kemi$date)), by = "day") %>% 
  tibble(date = .) -> date_range

kemi %>% 
  select(date:value) %>% 
  pivot_wider(names_from = parameter, values_from = value, values_fn = mean) -> kemi_wide

date_range %>% 
  left_join(discharge) %>% 
  full_join(kemi_wide) %>% 
  rename(TN = 3, TP = 4) %>% 
  mutate(TN = zoo::na.approx(TN,maxgap = 365),
         TP = zoo::na.approx(TP,maxgap = 365),
         N_load = TN*discharge/1000,
         P_load = TP*discharge/1000) %>% 
  pivot_longer(N_load:P_load) -> load
  return(load)
}
