source("Library.R")
read_csv("Data/Vandløbsdata/Pøleå nærings.csv", 
         locale = locale(decimal_mark = ",", grouping_mark = "."),
         col_types = cols(Resultat = col_double())) %>% 
  select(Dato, Stofparameter,Resultat, Enhed) %>% 
  mutate(date = dmy(Dato))-> data

data %>% 
  filter(Stofparameter %in% c("Phosphor, total-P", "Nitrogen,total N","Nitrit+nitrat-N")) %>% 
  mutate(doy = yday(date)) %>% 
  ggplot(aes(date,Resultat, )) + 
  geom_point() + 
  facet_wrap(~Stofparameter, ncol = 1, scales = "free_y") 
  


read_csv("Data/Vandløbsdata/Skjern å.csv", 
         locale = locale(decimal_mark = ",", grouping_mark = "."),
         col_types = cols(Resultat = col_double())) %>% 
  select(Dato, Stofparameter,Resultat, Enhed) %>% 
  mutate(date = dmy(Dato))-> data

data %>% 
  filter(Stofparameter %in% c("Phosphor, total-P", "Nitrogen,total N"),
         Resultat < 0.5) %>% 
  mutate(doy = yday(date)) %>% 
  ggplot(aes(date,Resultat, )) + 
  geom_point() + 
  geom_smooth() +
  facet_wrap(~Stofparameter, ncol = 1, scales = "free_y") 


read_csv("Data/Vandløbsdata/grindstedå.csv", 
         locale = locale(decimal_mark = ",", grouping_mark = "."),
         col_types = cols(Resultat = col_double())) %>% 
  select(Dato, Stofparameter,Resultat, Enhed) %>% 
  mutate(date = dmy(Dato))-> data

data %>% 
  filter(Stofparameter %in% c("Phosphor, total-P")) %>% 
  mutate(doy = yday(date)) %>% 
  ggplot(aes(date,Resultat, )) + 
  geom_point() + 
  geom_smooth() +
  facet_wrap(~Stofparameter, ncol = 1, scales = "free_y") 

