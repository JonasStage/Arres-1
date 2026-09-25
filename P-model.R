source("Library.R")

# P-model ----

## Inflow ----
all_loading %>% 
  filter(date > ymd("2016-01-01")) %>% 
  mutate(month = month(date),
         discharge = discharge*3600*24/1000,
         discharge_unit = "m3 d-1",
         conc_unit = "mg/l") %>% 
  reframe(discharge = mean(discharge, na.rm=T)*30,
          TP = mean(TP, na.rm=T),
          TN = mean(TN, na.rm=T),
          .by = c(stream,month, conc_unit)) %>% 
  mutate(discharge_unit = "m3 m-1") -> p_model_df
  
p_model_df %>% 
  select(stream,month,discharge, discharge_unit) %>% 
  group_by(month) %>% 
  mutate(fraction_of_discharge = discharge/sum(discharge)) 


p_model_df %>% 
  mutate(TP_input = TP*discharge/1000,
         TN_input = TN*discharge/1000,
         input_unit = "kg month-1") %>% 
  group_by(month) %>% 
  mutate(fraction_of_discharge = discharge/sum(discharge)) %>% 
  select(-c(TP:TN)) %>% 
  ungroup() %>% 
  reframe(across(c(discharge,TP_input,TN_input), ~sum(.x)),
          .by = month) %>% 
  mutate(tp_conc = TP_input/discharge*10^6,
         tn_conc = TN_input/discharge*10^6,
         conc_unit = "µg l-1",
         discharge_unit = "m3 month-1") 

## Lake concentration ----
plot_data %>% 
  filter(date > ymd("2016-01-01"), type %in% c("Phosphor, total-P","Ortho-phosphat-P")) %>% 
  mutate(month = month(date),
         year = year(date)) %>% 
  reframe(value = mean(value, na.rm=T),
          .by = c(type,year,month,unit)) %>% 
  reframe(value = mean(value, na.rm=T),
          .by = c(type,year,unit)) %>% 
  reframe(value = mean(value, na.rm=T),
          .by = c(type,unit))
  
