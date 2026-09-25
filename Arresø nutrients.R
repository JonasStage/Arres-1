source("Library.R")

#### Sediment data ####
##### Data from 1988 #####
read_excel("Data/Arresø tal fra Kaj.xlsx", sheet = "Sheet4", range = "A3:M19") %>% 
  select(station,depth, TP = TP...9,TN =TN...10) %>% 
  mutate(unit = "mg/cm2") %>% 
  filter(!str_detect(station,"Snit"),
         !depth == "0-10 cm") %>% 
  reframe(year = 1988,
          tn = sum(TN),
          tp = sum(TP),
            .by = station) -> old_sediment_data

#### New data - Miljøportalen ####
read_delim("Data/Arresø data/Kemi i sediment - S__20241118_123749.csv",
           delim = ";", escape_double = FALSE, col_types = cols(Dato = col_character(), Resultat = col_double()), 
           locale = locale(decimal_mark = ",", grouping_mark = "."), trim_ws = TRUE) %>% view
  filter(Stofparameter %in% c("Tørstof,total","Nitrogen,total N","Phosphor, total-P","Glødetab, total")) %>% 
  select(date = Dato, Prøve,"Sedimentlag-fra (cm)", "Sedimentlag-til (cm)", Stofparameter, Resultat, Enhed,
         `Målested, x-koordinat`,`Målested, y-koordinat`,station = Stedtekst) %>% 
  rename(number = Prøve, depth_start = "Sedimentlag-fra (cm)", depth_end = "Sedimentlag-til (cm)", 
         parameter = Stofparameter, value = Resultat, unit = Enhed,x_coord =  `Målested, x-koordinat`,y_coord = `Målested, y-koordinat`) %>% 
  mutate(date = dmy(date)) -> sediment_df

sediment_df %>% 
  filter(parameter == "Phosphor, total-P", depth_end <= 8) %>% 
  select(x_coord,y_coord) %>% 
  st_as_sf(coords = c("x_coord","y_coord"), crs = st_crs(25832)) %>% 
  st_transform(st_crs(4326))-> points

ggplot() + 
  geom_sf(data = arre) + 
  geom_sf(data = points)

sediment_df %>% 
  filter(depth_end <= 10) %>% 
  ggplot(aes(date, value, col = parameter, group = date)) + 
  geom_boxplot() + 
  facet_wrap(~parameter, scales = "free_y", ncol = 1)

sediment_df %>% 
  filter(depth_end <= 10, date == ymd("1993-12-27")) %>% 
  filter(parameter %in% c("Nitrogen,total N","Phosphor, total-P","Glødetab, total")) %>% 
  select(-unit,-x_coord,-y_coord) %>% 
  pivot_wider(names_from = parameter, values_from = value)
  arrange(date, depth_start) 
  
read_excel("Data/Arresø data/Copy of arresøtal (Autosaved).xlsx", sheet = 3) %>% 
  pivot_longer(`1988`:`2018`, names_to = "year") %>% 
  mutate(year = parse_number(year)) %>% 
  ggplot(aes(year,value, col = depth)) + 
  geom_point() + 
  geom_line() + 
  facet_wrap(~name, scales = "free_y") + 
  tema + 
  labs(x = "Year",
       y = "Value") + 
  theme(strip.text.x = element_text())

my_labeller_fig_sed <- as_labeller(c("Tons P i hele søen" = "Total~Phosphorous~(tons)",
                                     "Organisk N tons per sø" = "Total~Nitrogen~(tons)",
                                     "Organisk stof tons per sø" = "Organic~matter~(tons)"),
                                default = label_parsed)

nice_points_sed <- tibble(year = 1980,
                          value = 100000,
                          depth = "0",
                          type_f = as_factor("Organisk stof tons per sø"))


read_excel("Data/Arresø data/Copy of arresøtal (Autosaved).xlsx", sheet = 4) %>% 
  pivot_longer(`1976`:`2018`, names_to = "year") %>% 
  filter(!depth == "0-10 cm",
         !type == "Organisk kulstof tons per sø") %>% 
  mutate(year = parse_number(year),
         type_f = as.factor(type),
         type_f = factor(type_f, levels = c("Tons P i hele søen",
                                            "Organisk N tons per sø",
                                            "Organisk stof tons per sø"))) -> sediment_data_ksj
sediment_data_ksj %>% 
  ggplot(aes(as.factor(year),value, fill = depth)) + 
  geom_col(width = 0.6) +
  geom_point(data = nice_points_sed) + 
  facet_grid(type_f~., scales = "free_y", labeller = my_labeller_fig_sed, switch = "y") + 
  scale_x_discrete(limits = c("1976","1988","2002","2009","2014","2018")) +
  scale_fill_manual(limits = c("0-2 cm","2-5 cm","5-10 cm"),
                    values = c("goldenrod4","chocolate","darkorange4")) +
  tema + 
  labs(x = "Year",
       y = "",
       fill = "") + 
  theme(strip.text.x = element_text()) -> sediment_chemistry

# tiff("Manuscript/Figures/Sediment_chemistry.tiff", height = 800)
# sediment_chemistry
# dev.off()

fig4_new_df %>% 
  filter(name == "TP") %>% 
  mutate(type_f = as.factor("Tons P i hele søen")) -> TP_lake

ggplot() +
  geom_col(data = filter(sediment_data_ksj,type_f == "Tons P i hele søen") ,aes(year,value/10)) + 
  geom_point(data = TP_lake, aes(year, arre),size = 3) +
  geom_line(data = TP_lake, aes(year, arre), lwd = 2) +
  tema + 
  labs(x = "År",
       y = "Fosfor input til Arresø") +
  scale_y_continuous(sec.axis = sec_axis(trans = ~.*10,
                                         name = "Fosfor sedimentkerner"))
  

#### Vandkemi data ####

read_delim("Data/Arresø data/Vandkemi - S__20241118_124615.csv",
           delim = ";", escape_double = FALSE, col_types = cols(Dato = col_character(), Resultat = col_double()), 
           locale = locale(decimal_mark = ",", grouping_mark = "."), trim_ws = TRUE) %>% 
  filter(Stofparameter %in% c("Nitrogen,total N","Phosphor, total-P","Nitrit+nitrat-N","Ammoniak+ammonium-N","Ortho-phosphat-P")) %>% 
  select(date = Dato, 
         parameter = Stofparameter,
         value = Resultat,
         unit = Enhed) %>% 
  mutate(date = dmy(date)) %>% 
  arrange(date) -> vandkemi_df

vandkemi_df %>% 
  ggplot(aes(date, value, col = parameter)) + 
  geom_point() + 
  facet_wrap(~parameter, scales = "free_y", ncol = 1)

nice_points <- tibble(x = c(1970,1970,1970,1970,2024,2024,2024,2024),
       y = c(0,1.5,0,8,0,1.5,0,8),
       parameter = c("Phosphor, total-P","Phosphor, total-P","Nitrogen,total N","Nitrogen,total N","Phosphor, total-P","Phosphor, total-P","Nitrogen,total N","Nitrogen,total N")) %>% 
  mutate(parameter = case_when(parameter == "Phosphor, total-P" ~ "Total Fosfor",
                               parameter == "Nitrogen,total N" ~ "Total Kvælstof")) 

vandkemi_df %>% 
  mutate(month = month(date),
         year = year(date)) %>% 
  filter(between(month, 5,9)) %>%
  pivot_wider(names_from= parameter, values_from = value, values_fn = mean) %>% 
  mutate(tp = `Phosphor, total-P`, inorgp = `Ortho-phosphat-P`,
         tn = `Nitrogen,total N`, inorgn = (`Ammoniak+ammonium-N`+`Nitrit+nitrat-N`)) %>% 
  reframe(across(tp:inorgn, list(min=min,mean=mean,max=max), na.rm=T),
            .by = c(year))  -> fig5_df

fig5_df %>% 
  select(year:inorgp_max) %>% 
  ggplot(aes(year)) + 
  geom_point(aes(y = inorgp_mean, col = "inorgp"), size = 3) +
  geom_point(aes(y = tp_mean, col = "tp"), size = 3) +
  geom_line(aes(y = inorgp_mean, col = "inorgp"), show.legend = F) +
  geom_line(aes(y = tp_mean, col = "tp"), show.legend = F) +
  geom_ribbon(aes(ymin = inorgp_min, ymax = inorgp_max, fill = "inorgp"), alpha = 0.5, show.legend = F) + 
  geom_ribbon(aes(ymin = tp_min, ymax = tp_max, fill = "tp"), alpha = 0.5, show.legend = F) +
  tema + 
  scale_x_continuous(limits = c(1970,2024)) +
  scale_y_continuous(limits = c(0,1.5)) +
  labs(x = "",
       y = bquote("Phosphorus (mg P l"^-1*")"),
       col = "") + 
  scale_color_manual(limits = c("tp","inorgp"),
                     labels = c("Total Phosphorus","Inorganic Phosphorus"),
                     values = c("royalblue","green4")) + 
  scale_fill_manual(limits = c("tp","inorgp"),
                    values = c("royalblue","green4")) -> tp_orgp_plot

fig5_df %>% 
  select(year,tn_min:inorgn_max) %>% 
  ggplot(aes(year)) + 
  geom_point(aes(y = inorgn_mean, col = "inorgn"), size = 3) +
  geom_point(aes(y = tn_mean, col = "tn"), size = 3) +
  geom_line(aes(y = inorgn_mean, col = "inorgn"), show.legend = F) +
  geom_line(aes(y = tn_mean, col = "tn"), show.legend = F) +
  geom_ribbon(aes(ymin = inorgn_min, ymax = inorgn_max, fill = "inorgn"), alpha = 0.5, show.legend = F) + 
  geom_ribbon(aes(ymin = tn_min, ymax = tn_max, fill = "tn"), alpha = 0.5, show.legend = F) +
  tema + 
  scale_y_continuous(limits = c(0,8)) +
  labs(x = "",
       y = bquote("Nitrogen (mg N l"^-1*")"),
       col = "") + 
  scale_x_continuous(limits = c(1970,2024)) +
  scale_color_manual(limits = c("tn","inorgn"),
                     labels = c("Total Nitrogen","Inorganic Nitrogen"),
                     values = c("darkorange","cyan4")) + 
  scale_fill_manual(limits = c("tn","inorgn"),
                    values = c("darkorange","cyan4")) -> tn_orgn_plot

vandkemi_df %>% 
  mutate(month = month(date),
         year = year(date)) %>% 
  filter(between(month, 5,9)) %>% 
  pivot_wider(values_from = value, names_from = parameter, values_fn = mean) %>% 
  rename(TP = 5, TN = 8) %>% 
  mutate(TN_TP = TN/TP) %>% 
  reframe(min = min(TN_TP, na.rm=T), mean = mean(TN_TP, na.rm=T), max = max(TN_TP, na.rm=T),
          parameter = "TN:TP",
          .by = c(year)) %>% 
  ggplot() + 
  geom_ribbon(aes(year, ymin = min, ymax = max),fill = "darkred", alpha = 0.5, show.legend = F) +
  geom_point(aes(year, mean,col = "darkred"), size = 3) +
  geom_line(aes(year, mean,col = "darkred"), show.legend = F) + 
  scale_x_continuous(limits = c(1970,2024)) +
  theme(strip.text.x = element_blank(),
        strip.text.y = element_blank()) +
  labs(x = "",
       y = "TN:TP",
       col = "",
       fill = "") + 
  scale_color_manual(values = "darkred", label = "TN:TP") +
  tema  ->relative_plot

tiff("Manuscript/Nutrient dynamics/Review/Second round/figure 5.tiff", width = 800, height = 800)
tp_orgp_plot/ tn_orgn_plot/relative_plot + plot_layout(guides = "collect",axes ="collect_x") & theme(legend.position = "bottom")
dev.off()

#### Tidsudvilkling i klorofyl, TN, TP, og pH - Figure 6 ####

read_delim("Data/Vandløbsdata/Arresø kanal/Salt, ilt, temperatur, ph m_ling - Vandl_b_20241204_074150.csv", 
           delim = ";", escape_double = FALSE, col_types = cols(Dato = col_character(), 
                                                                Resultat = col_double()), 
           locale = locale(decimal_mark = ",", grouping_mark = "."), trim_ws = TRUE) %>% 
  select(date = Dato, 
         depth = Dybde,
         type = Parameter,
         value = Resultat,
         unit = Enhed) %>% 
  mutate(date = dmy(date),
         month = month(date),
         year = year(date)) %>% 
  filter(between(month,5,9)) %>% 
  reframe(value = mean(value, na.rm=T),
          .by = c(year,type)) %>% 
  filter(type == "pH") %>% 
  mutate(location = "Arresø kanal") -> kanal_ph

arre_mdb %>% 
  filter(type %in% c("pH"), 
         value > 5)-> fig6.1_df

nice_points_fig6 <- tibble(type = rep(c("pH","Chlorophyl (ukorrigeret)"), each = 2), value = c(8,10,0,500))

my_labeller_fig6 <- as_labeller(c(chl="Chlorophyll~italic(a)~(µg~l^-1)", pH="pH"),
                             default = label_parsed)

plot_data %>% 
  filter(type == "Chlorophyl (ukorrigeret)") %>% 
  bind_rows(fig6.1_df) %>% 
  mutate(month = month(date),
         year = year(date)) %>% 
  filter(between(month,5,9)) %>% 
  reframe(value = mean(value, na.rm=T),
            .by = c(year,type)) %>% 
  mutate(location = "Arresø") %>% 
  bind_rows(kanal_ph) %>%
  add_row(year = 1976, type = "pH", value = 9.65, location ="Arresø") %>% 
  bind_rows(nice_points_fig6) %>% 
  mutate(type = case_when(type == "Chlorophyl (ukorrigeret)" ~ "chl",
                          type == "pH" ~ "pH")) %>% 
  ggplot(aes(year,value, col = type)) + 
  scale_x_continuous(limits = c(1970,2024)) +
  scale_color_manual(limits = c("chl","pH"),
                     labels = c("Chlorophyll a", "pH"),
                     values = c("forestgreen","lightblue3")) +
  geom_point(size = 3) + 
  geom_line(show.legend = F) +
  facet_grid(type~., scales = "free_y", labeller = my_labeller_fig6, switch = "y") +
  labs(col = "",
       x = "",
       y = "") +
  tema -> fig6.1

nice_points_fig6.2 <- tibble(name = c("TP","TP","TN","TN","tntp","tntp"),
                             value = c(0,1,0,4,0,30),
                             year = rep(NA_real_,6)) %>% 
  mutate(name = as.factor(name),
         name = factor(name, levels = c("TP","TN","tntp")))

my_labeller_fig6.2 <- as_labeller(c(TP="Total~Phosphorus~(mg~TP~l^-1)", TN="Total~Nitrogen~(mg~TN~l^-1)", tntp = "TN:TP"),
                                default = label_parsed)
vandkemi_df %>% 
  mutate(month = month(date),
         year = year(date)) %>% 
  filter(between(month, 5,9)) %>% 
  reframe(mean = mean(value, na.rm=T),
          min = min(value,na.rm=T),
          max = max(value,na.rm=T),
          .by = c(year,parameter)) %>% 
  mutate(name = case_when(parameter == "Phosphor, total-P" ~ "TP",
                               parameter == "Nitrogen,total N" ~ "TN"),
         name = as.factor(name),
         name = factor(name, levels = c("TP","TN","tntp"))) -> range_fig6

vandkemi_df %>% 
  mutate(month = month(date),
         year = year(date)) %>% 
  filter(between(month, 5,9)) %>% 
  pivot_wider(values_from = value, names_from = parameter, values_fn = mean)  %>% 
  mutate(tntp = `Nitrogen,total N`/`Phosphor, total-P`) %>% 
  reframe(tntp_min = min(tntp, na.rm=T),
          tntp_max = max(tntp, na.rm=T),
          name = "tntp",
          .by = c(year)) %>% 
  mutate(name = as.factor(name),
         name = factor(name, levels = c("TP","TN","tntp")))-> tntp_range

vandkemi_df %>% 
  mutate(month = month(date),
         year = year(date)) %>% 
  filter(between(month, 5,9)) %>% 
  reframe(mean = mean(value, na.rm=T),
          .by = c(year,parameter)) %>% 
  mutate(parameter = case_when(parameter == "Phosphor, total-P" ~ "TP",
                               parameter == "Nitrogen,total N" ~ "TN")) %>% 
  pivot_wider(values_from = mean, names_from = parameter) %>% 
  mutate(tntp = TN/TP) %>% 
  pivot_longer(TP:tntp) %>% 
  mutate(name = as.factor(name),
         name = factor(name, levels = c("TP","TN","tntp"))) %>% 
  ggplot() + 
  geom_line(aes(year, value, col = name),show.legend = F) + 
  geom_point(aes(year, value, col = name),size =3) +
  geom_point(data = nice_points_fig6.2, aes(year,value)) +
  geom_ribbon(data = range_fig6, aes(year, ymin = min, ymax = max, fill = name), alpha = 0.5, show.legend = F) +
  geom_ribbon(data = tntp_range, aes(year, ymin = tntp_min, ymax = tntp_max, fill = name), alpha = 0.5, show.legend = F) +
  labs(x = "",
       y = "",
       col = "",
       fill = "") + 
  facet_grid(name~., scales = "free_y", labeller = my_labeller_fig6.2, switch = "y") +
  scale_x_continuous(limits = c(1970,2024),
                     breaks = seq(1970,2024,10)) +
  tema +
  scale_color_manual(values = c("royalblue","darkorange","darkred"), labels = c("Total Phosphorus","Total Nitrogen","TN:TP")) +
  scale_fill_manual(values = c("royalblue","darkorange","darkred"), labels = c("Total Phosphorus","Total Nitrogen","TN:TP"))-> fig6

tiff("Manuscript/Figures/figure 6 new.tiff") 
fig6.1
dev.off()

tiff("Manuscript/Nutrient dynamics/Figures/figure 8.tiff",height = 700, width = 600) 
fig6
dev.off()

  #### Nitrat, ammonium og fosfat figure 7 ####

plot_data %>% 
  filter(type == "Ammoniak+ammonium-N") %>% 
  add_row(date = ymd(c("1950-01-01","2050-01-01")), type = rep("Ammoniak+ammonium-N",2)) %>% 
  ggplot(aes(date, value*1000, col = type)) + 
  geom_ribbon(xmin = -Inf, xmax = Inf, aes(ymin = 0, ymax = 0.05*1000),col = NA, fill = "grey80") +
  geom_point(size = 3) + 
  scale_x_date(date_breaks = "5 years",
               date_labels = "%Y") +
  tema + 
  labs(x = "",
       y = bquote("Ammonium (µg N l"^-1*")"),
       col = "") + 
  scale_color_manual(values = "darkolivegreen", label = "Ammonium") +
  scale_y_continuous(limits = c(0,1.6)*1000, breaks = seq(0,1.5,.5)*1000,labels= seq(0,1.5,.5)*1000) -> fig7.1
  


((fig6.2_vj + labs(y = bquote("Phosphate (µg P l"^-1*")"))) / 
  (fig6.1_vj + labs(y = bquote("Nitrate (µg N l"^-1*")"))) / 
    fig7.1)  + plot_layout(guides = "collect", axes = "collect_x") & 
  theme(legend.position = "bottom") & 
  scale_x_date(limits = c(ymd("1940-01-01"),ymd("2050-01-01")),
        breaks = seq(ymd("1970-01-01"),ymd("2025-01-01"), "10 years"),
        date_labels = "%Y") & 
  coord_cartesian(xlim = c(ymd("1970-01-01"),ymd("2025-01-01")))-> fig7

tiff("Manuscript/Figures/figure 7.tiff",width = 800,height = 800) 
fig7
dev.off()

#### Klorofyl vs TN og TP figur 8 ####

(fig4_vj + labs(x = bquote("Total Phosphorus (mg TP l"^-1*")"),
                y = bquote("Chlorophyll a (µg l"^-1*")"))) + 
    (fig5_vj + labs(x = bquote("Total Nitrogen (mg TN l"^-1*")"),
                    y = bquote("Chlorophyll a (µg l"^-1*")"))) + plot_layout(axes = "collect_y") -> fig8

tiff("Manuscript/Figures/figure 8.tiff", width = 800) 
fig8
dev.off()

#### Sæsonvariationer i uorg N og fosfat - figur 10 ####

plot_data %>% 
  filter(type %in% c("Ammoniak+ammonium-N","Nitrit+nitrat-N"))  %>% 
  reframe(value = mean(value), 
            .by = c(date,type)) %>% 
  pivot_wider(names_from = type, values_from = value) %>% 
  drop_na() %>% 
  mutate(year = year(date)) %>% 
  filter(year %in% c(1977,2022)) %>% 
  rowwise() %>% 
  mutate(uorgN = sum(`Ammoniak+ammonium-N`,`Nitrit+nitrat-N`),
         doy = yday(date)) %>% 
  select(year,date,doy,uorgN) %>% 
  ggplot(aes(doy,uorgN*1000, col = as.factor(year))) + 
  geom_point(size = 3) + 
  tema + 
  scale_x_continuous(limits = c(0,365),
                     breaks = c(1,32,60,91,121,152,182,213,244,274,305,335),
                     labels = c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")) + 
  labs(x = "",
       col = "",
       y = bquote("Inorganic Nitrogen (µg N l"^-1*")")) + 
  scale_y_continuous(limits = c(0,2000)) -> fig10.1

plot_data %>% 
  filter(type %in% c("Ortho-phosphat-P"))  %>% 
  reframe(value = mean(value), 
          .by = c(date,type)) %>% 
  pivot_wider(names_from = type, values_from = value) %>% 
  drop_na() %>% 
  mutate(year = year(date)) %>% 
  filter(year %in% c(1977,2022)) %>% 
  mutate(doy = yday(date)) %>% 
  rename(ortop = `Ortho-phosphat-P`) %>% 
  ggplot(aes(doy,ortop*1000, col = as.factor(year))) + 
  geom_point(size = 3) + 
  tema + 
  scale_x_continuous(limits = c(0,365),
                     breaks = c(1,32,60,91,121,152,182,213,244,274,305,335),
                     labels = c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")) + 
  labs(x = "",
       col = "",
       y = bquote("Phosphate (µg P l"^-1*")")) -> fig10.2

tiff("Manuscript/Figures/figure 10.tiff") 
fig10.2 / fig10.1 + plot_layout(guides = "collect",axes ="collect_x") & theme(legend.position = "bottom")
dev.off()

#### Til Kaj ####
sigt

plot_data %>% 
  filter(type %in% c("Phosphor, total-P","Nitrogen,total N","Ortho-phosphat-P","Ammoniak+ammonium-N","Nitrit+nitrat-N",
                     "Chlorophyl (ukorrigeret)")) %>% 
  select(date,type,value) %>% 
  pivot_wider(names_from = type, values_from = value, values_fn = mean) %>% 
  full_join(sigt) %>% 
  arrange(date) %>% 
  write_csv("Output/Nutrients_secchi.csv")

plot_data %>% 
  filter(type %in% c("Phosphor, total-P","Nitrogen,total N","Ortho-phosphat-P","Ammoniak+ammonium-N","Nitrit+nitrat-N",
                     "Chlorophyl (ukorrigeret)")) %>% 
  select(date,type,value) %>% 
  pivot_wider(names_from = type, values_from = value, values_fn = mean) %>% 
  full_join(sigt) %>% 
  mutate(year = year(date)) %>% 
  filter(year >= 2010,
         !year == 2003) %>% 
  reframe(across(`Nitrogen,total N`:secchi, ~mean(.x, na.rm=T)),
            .by = year) %>% 
  arrange(year) %>% 
  select(year, TN = 2, TP = 3,chl = `Chlorophyl (ukorrigeret)`, secchi) %>% 
  filter(year %in% c(2010, 2024)) %>% 
  reframe(across(TN:secchi, ~((max(.x)-min(.x))/ min(.x))*100))
  
