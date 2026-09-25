library(tidyverse);library(readxl);library(lubridate);library(patchwork)
setwd("/Users/jonas/Library/CloudStorage/OneDrive-SyddanskUniversitet/Arresø")
source("/Users/jonas/Library/CloudStorage/OneDrive-SyddanskUniversitet/R help script/ggplot_themes.R")
source("/Users/jonas/Library/CloudStorage/OneDrive-SyddanskUniversitet/R help script/dmi_kommune_get.R")

#### Read in data ####
read_csv("Data/Salt, ilt, temperatur, ph m_ling - S__20241025_094830.csv", 
         col_types = cols(Resultat = col_double()), 
         locale = locale(decimal_mark = ",", grouping_mark = ".")) %>% 
  select(date = Dato, 
         Dybde,
         type = Parameter,
         value = Resultat,
         unit = Enhed) %>% 
  mutate(date = dmy(date)) -> field_data

#### Data exploration ####
##### Water Temperature #####
field_data %>% 
  filter(type == "Temperatur") %>% 
  mutate(value = if_else(value>40, value/10, value),
         year = year(date)) %>% 
  reframe(value = mean(value, na.rm=T),
            .by = year) %>% 
  ggplot(aes(year, value)) + 
  geom_point() +
  geom_smooth(method = "lm", se =F) + 
  labs(x = "År", 
       y = "Middel vand temperatur") + 
  tema -> wtr_temp



##### Air temp and precip #####
get_kommune_data(time_from = "2008-12-01T00:00:00Z", time_to ="2024-01-01T00:00:00Z", municipalityID = "0250", par = "mean_temp", time_res = "month") %>% 
  mutate(date = as.Date(nearest_hour)) %>% 
  select(date, air_temp = value) -> dmi_temp

get_kommune_data(time_from = "2008-12-01T00:00:00Z", time_to ="2024-01-01T00:00:00Z", municipalityID = "0250", par = "acc_precip", time_res = "month") %>% 
  mutate(date = as.Date(nearest_hour)) %>% 
  select(date, precip = value) -> dmi_precip

full_join(dmi_temp, 
          dmi_precip) -> temp_precip
  
temp_precip %>% 
  ggplot(aes(date)) + 
  geom_col(aes(y = precip/7), fill = "blue") +
  geom_line(aes(y = air_temp), linewidth = 2) +
  scale_x_date(date_labels = "%Y",
               date_breaks = "1 year") + 
  tema + 
  scale_y_continuous(sec.axis = sec_axis(~.x*7))
