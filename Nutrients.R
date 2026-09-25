source("Library.R")
read_excel("Data/Copy of Arresø_data.xlsx") -> arre

read_csv("Data/Arresø data/Dybder og springlag - S__20241025_094831.csv", 
              locale = locale(decimal_mark = ",", 
                              grouping_mark = "."), 
              trim_ws = TRUE) %>% 
  select(date = Dato, 
         secchi = `Sigtdybde (m)`,
         sigt_til_bund =`Sigt til bund`,
         depth = `Bunddybde (m)`) %>% 
  filter(!sigt_til_bund == "Ja") %>% 
  select(-sigt_til_bund) %>% 
  mutate(date = dmy(date),
         type = "secchi") -> sigt
  
tibble(param = c("Nitrogen,total N",
                 "Nitrit+nitrat-N",
                 "Phosphor, total-P",
                 "Ortho-phosphat-P",
                 "Chlorophyl (ukorrigeret)"),
       plot_number = c(1,1,2,2,3)) -> param_select

arre %>%  
  filter(Vandområde == "Arresø") %>% 
  select(date = Dato, 
         depth = `Dybde (m)`,
         fak_depth = `Faktiske dybder (m)`,
         type = Stofparameter,
         value = Resultat,
         unit = Enhed) %>% 
  mutate(date = ymd(date))  -> plot_data

##### Figure 1 #####
plot_data %>% 
  filter(type %in% c("Phosphor, total-P","Nitrogen,total N","Chlorophyl (ukorrigeret)")) ->fig1_data

write_csv(plot_data, "C:/Users/jonas/OneDrive - Syddansk Universitet/Arresø/Bog/Data/kapitel 6 data.csv")

fig1_data %>% 
  mutate(date = year(date)) %>% 
  reframe(value = mean(value, na.rm=T),
            .by = c(date,type,unit)) -> year_data

lim1 <- 1900; lim2 <- 1990; lim3 <- 2010; lim4 <- 2024

year_data %>% 
  filter(between(date, lim1,lim2)) %>% 
  mutate(logvalue = case_when(type %in% c("Phosphor, total-P","Nitrogen,total N","Chlorophyl (ukorrigeret)") ~ log10(value))) -> first_period

year_data %>% 
  filter(between(date, lim2, lim3)) %>% 
  mutate(logvalue = case_when(type %in% c("Phosphor, total-P","Nitrogen,total N","Chlorophyl (ukorrigeret)") ~ log10(value)))-> second_period

year_data %>% 
  filter(between(date, lim3, lim4)) %>% 
  mutate(logvalue = case_when(type %in% c("Phosphor, total-P","Nitrogen,total N","Chlorophyl (ukorrigeret)") ~ log10(value)))-> third_period

year_data %>% 
  filter(!type == "secchi") %>% 
  mutate(logvalue = log10(value)) %>% 
  ggplot() + 
  geom_point(aes(date,logvalue, col = type)) +
  geom_smooth(data = first_period,aes(date,logvalue, col = type), method = "lm", se = F) + 
  geom_smooth(data = second_period,aes(date,logvalue, col = type), method = "lm", se = F) + 
  geom_smooth(data = third_period,aes(date,logvalue, col = type), method = "lm", se = F) +
  labs(x = "", y = "Concentration", col = "") + 
  scale_x_continuous(labels = c(seq(1970, 2024, 5),2024),
                     breaks = c(seq(1970, 2024, 5),2024)) +
  tema + 
  scale_color_manual(limits = c("Phosphor, total-P","Nitrogen,total N","Chlorophyl (ukorrigeret)"),
                     labels = c(bquote("Total Phosphorous (mg P l"^-1*")"), 
                                bquote("Total Nitrogen (mg N l"^-1*")"), 
                                bquote("Chlorophyll "*italic(a)*" (µg l"^-1*")")),
                     values = c("royalblue","darkorange","forestgreen")) + 
  scale_y_continuous(limits = c(-1.5,3),
                     breaks = c(-1,0,1,2,3),
                     labels = c(0.1,1,10,100,1000)) -> fig1;fig1

tiff("Manuscript/Figures/figure 4.tiff", width = 620, height = 500)  
fig1
dev.off()

##### Figure 2 #####
year_data %>%
  filter(type %in% c("Phosphor, total-P")) %>% 
  ggplot(aes(date,value, col = type)) + 
  geom_point() + 
  tema +
  scale_x_continuous(labels = c(seq(1970, 2024, 5),2024),
                     breaks = c(seq(1970, 2024, 5),2024)) +
  geom_smooth(se =F) + 
  scale_y_continuous(limits = c(0,1),
                     breaks = seq(0,1,.2),
                     labels = seq(0,1,.2)) +
  scale_color_manual(values = "royalblue")+
  theme(legend.position = "none",
        axis.text.x = element_blank()) +
  labs(x = "",
       y = "Total fosfor koncentration (mg/l)") -> fig2.1

year_data %>%
  filter(type %in% c("Phosphor, total-P")) %>% 
  ggplot(aes(date,value, col = type)) + 
  geom_point(show.legend = F) + 
  geom_smooth(se =F,show.legend = F) + 
  labs(x = "", y = "", col = "") +
  theme(legend.position = "none") +
  scale_color_manual(values = "royalblue")+
  scale_y_continuous(breaks = c(0.05,0.1,0.15), labels = c(0.05,0.1,0.15)) +
  coord_cartesian(xlim = c(2010,2024), ylim = c(0.05,0.15)) +
  scale_x_continuous(breaks = c(2010,2015,2020,2024),labels = c(2010,2015,2020,2024)) + 
  tema + 
  theme(
    panel.background = element_rect(fill='transparent'),
    plot.background = element_rect(fill='transparent', color=NA),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.background = element_rect(fill='transparent'),
    legend.box.background = element_rect(fill='transparent')
  )-> inset_2.1

fig2.1 + inset_element(inset_2.1, 0.5, 0.5, 1, 1) -> fig2.1.1

year_data %>%
  filter(type %in% c("Nitrogen,total N")) %>% 
  ggplot(aes(date,value, col = type)) + 
  geom_point() + 
  tema +
  scale_x_continuous(labels = c(seq(1970, 2024, 5),2024),
                     breaks = c(seq(1970, 2024, 5),2024)) +
  geom_smooth(se =F) + 
  scale_y_continuous(limits = c(1,5),
                     breaks = seq(1,5,1),
                     labels = seq(1,5,1)) +
  scale_color_manual(values = "darkorange")+
  theme(legend.position = "none") +
  labs(x = "År",
       y = "Total Kvælstof koncentration (mg/l)") -> fig2.2

year_data %>%
  filter(type %in% c("Nitrogen,total N")) %>% 
  ggplot(aes(date,value, col = type)) + 
  geom_point(show.legend = F) + 
  geom_smooth(se =F,show.legend = F) + 
  labs(x = "", y = "", col = "") +
  theme(legend.position = "none") +
  scale_color_manual(values = "darkorange")+
  coord_cartesian(xlim = c(2010,2024), ylim = c(1.5,2.6)) +
  scale_y_continuous(breaks = c(1.5,2,2.5), labels = c(1.5,2,2.5)) +
  scale_x_continuous(breaks = c(2010,2015,2020,2024),labels = c(2010,2015,2020,2024)) + 
  tema + 
  theme(
    panel.background = element_rect(fill='transparent'),
    plot.background = element_rect(fill='transparent', color=NA),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.background = element_rect(fill='transparent'),
    legend.box.background = element_rect(fill='transparent')
  )-> inset_2.2

fig2.2 + inset_element(inset_2.2, 0.5, 0.5, 1, 1)-> fig2.2.1

tiff("Figures/Fig2.tiff", height = 800)
fig2.1.1 / fig2.2.1 + plot_layout()
dev.off()

##### Figure 3 #####
tibble(type = rep(c("Chlorophyl (ukorrigeret)","Nitrogen,total N","Phosphor, total-P","secchi"),2),
       value = c(50,1.5,0.05,.4,100,2.7,.15,0.8),
       date = rep(2025,8)) %>% 
  mutate(type_f = case_when(type == "Phosphor, total-P" ~ "Total Fosfor (mg/l)",
                            type == "Nitrogen,total N" ~ "Total Kvælstof (mg/l)",
                            type == "Chlorophyl (ukorrigeret)"~ "Klorofyl a (µg/l)",
                            type == "secchi" ~ "Sigtdybde (m)"),
         type_f = as.factor(type_f),
         type_f = factor(type_f, levels = c("Total Fosfor (mg/l)", "Total Kvælstof (mg/l)", "Klorofyl a (µg/l)","Sigtdybde (m)"))) -> fig3_min_max
year_data %>% 
  filter(date >= 2010) %>%
  mutate(type_f = case_when(type == "Phosphor, total-P" ~ "Total Fosfor (mg/l)",
                            type == "Nitrogen,total N" ~ "Total Kvælstof (mg/l)",
                            type == "Chlorophyl (ukorrigeret)"~ "Klorofyl a (µg/l)",
                            type == "secchi" ~ "Sigtdybde (m)"),
         type_f = as.factor(type_f),
         type_f = factor(type_f, levels = c("Total Fosfor (mg/l)", "Total Kvælstof (mg/l)", "Klorofyl a (µg/l)","Sigtdybde (m)"))) %>% 
  ggplot(aes(date, value, col = type_f)) + 
  geom_point() + 
  geom_point(data = fig3_min_max, aes(date,value,col = type_f)) +
  coord_cartesian(xlim = c(2010,2024)) +
  facet_grid(type_f~1, scales = "free_y") + 
  scale_x_continuous(breaks = c(2010,2015,2020,2024),
                     label = c(2010,2015,2020,2024)) + 
  tema + 
  labs(x = "År",
       y = "",
       col = "") +
  theme(strip.text.x = element_blank(),
        panel.spacing = unit(1, "lines"),
        legend.position = "none") + 
  scale_color_manual(limits = c("Total Fosfor (mg/l)", "Total Kvælstof (mg/l)", "Klorofyl a (µg/l)","Sigtdybde (m)"),
                     values = c("royalblue","darkorange","forestgreen","red4"))  -> fig3;fig3
  
tiff("Figures/Fig3.tiff", width = 800, height = 800)  
fig3
dev.off()

 ##### Figure 4 #####
fig1_data %>% 
  filter(type %in% c("Phosphor, total-P","Chlorophyl (ukorrigeret)")) %>% 
  select(-unit, -fak_depth, -depth) %>% 
  mutate(month = month(date),
         year = year(date)) %>% 
  filter(between(month, 5,9)) %>% 
  reframe(value = mean(value, na.rm=T),
          .by = c(type,year)) %>% 
  pivot_wider(names_from = type, values_from = value) %>% 
  rename(tp = 2, chl = 3) -> fig4_data

fig4_data %>% 
  ggplot(aes(tp, chl)) + 
  geom_point() + 
  ggpmisc::stat_poly_line(data = filter(fig4_data, tp < 0.4), se = F) +
  ggpmisc::stat_poly_eq(data = filter(fig4_data, tp < 0.4),ggpmisc::use_label(c("eq", "R2"))) +
  labs(x = bquote("Total Fosfor (mg P l"^-1*")"),
       y = bquote("Chlorophyl (µg l"^-1*")")) + 
  scale_x_continuous(limits = c(0,0.9), breaks = seq(0,0.8,.2))+
  tema  -> fig4_vj;fig4_vj 

tiff("Figures/Fig4.tiff")  
fig4_vj
dev.off()

##### Figure 5 #####

fig1_data %>% 
  filter(type %in% c("Nitrogen,total N","Chlorophyl (ukorrigeret)")) %>% 
  select(-unit, -fak_depth, -depth) %>% 
  mutate(month = month(date),
         year = year(date)) %>% 
  filter(between(month, 5,9)) %>% 
  reframe(value = mean(value, na.rm=T),
          .by = c(type,year)) %>% 
  pivot_wider(names_from = type, values_from = value) %>% 
  rename(tn = 2, chl = 3) -> fig5_data

fig5_data %>%   
  ggplot(aes(tn, chl)) + 
  geom_point() + 
  ggpmisc::stat_poly_line(data = filter(fig5_data, tn < 3), se = F) +
  ggpmisc::stat_poly_eq(data = filter(fig5_data, tn < 3),ggpmisc::use_label(c("eq", "R2"))) +  
  labs(x = bquote("Total Nitrogen (mg N l"^-1*")"),
       y = bquote("Chlorophyl (µg l"^-1*")")) + 
  scale_x_continuous(limits = c(1,4)) +
  tema -> fig5_vj

tiff("Figures/Fig5.tiff")  
fig5_vj
dev.off()

tiff("Manuscript/Vand og Jord/Figures/figure 5.tiff", width = 400)
fig4_vj + fig5_vj
dev.off()

##### Figure 6 #####
plot_data %>% 
  filter(type %in% c("Nitrit+nitrat-N")) %>% 
  add_row(date = ymd(c("1950-01-01","2050-01-01")), type = rep("Nitrit+nitrat-N",2)) %>% 
  ggplot() + 
  geom_ribbon(xmin = -Inf, xmax = Inf, aes(x = date, ymin = 0, ymax = 0.05*1000),col = NA, fill = "grey80") +
  geom_point(aes(date, value*1000, col = type),size = 3) + 
  tema + 
  scale_x_date(breaks = c(seq(ymd("1970-01-01"),ymd("2024-01-01"), "10 years")),
               date_labels = "%Y",
               limits = c(ymd("1950-01-01"),ymd("2080-01-01"))) +
  coord_cartesian(xlim = c(ymd("1970-01-01"),ymd("2024-01-01"))) + 
  labs(x = "",
       y = "Nitrat (µg N/l)",
       col = "") + 
  scale_color_manual(values = "purple3", label = "Nitrat") +
  scale_y_continuous(limits = c(0,1)*1000, breaks = seq(0,1,.25)*1000,labels= seq(0,1,.25)*1000)-> fig6.1_vj


plot_data %>% 
  filter(type %in% c("Ortho-phosphat-P")) %>% 
  add_row(date = ymd(c("1950-01-01","2050-01-01")), type = rep("Ortho-phosphat-P",2)) %>% 
  drop_na(type) %>% 
  ggplot(aes(date, (value*1000), col = type)) + 
  geom_ribbon(xmin = -Inf, xmax = Inf, aes(ymin = 0, ymax = 0.005*1000),col = NA, fill = "grey80") +
  geom_point(size = 3) + 
  scale_x_date(breaks = c(seq(ymd("1970-01-01"),ymd("2024-01-01"), "10 years")),
               date_labels = "%Y",
               limits = c(ymd("1950-01-01"),ymd("2080-01-01"))) +
  coord_cartesian(xlim = c(ymd("1970-01-01"),ymd("2024-01-01"))) + 
  tema + 
  labs(x = "",
       y = "Fosfat (µg P/l)",
       col = "") + 
  scale_color_manual(values = "darkgoldenrod", label = "Fosfat") +
  scale_y_log10() -> fig6.2_vj

plot_data %>%  
  filter(type %in% c("Ammoniak+ammonium-N")) %>% 
  add_row(date = ymd(c("1950-01-01","2050-01-01")), type = rep("Ammoniak+ammonium-N",2)) %>% 
  drop_na(type) %>% 
  ggplot(aes(date, (value*1000), col = type)) + 
  geom_ribbon(xmin = 1950, xmax = Inf, aes(ymin = 0, ymax = 0.005*1000),col = NA, fill = "grey80") +
  geom_point(size = 3) + 
  scale_x_date(breaks = c(seq(ymd("1970-01-01"),ymd("2024-01-01"), "10 years")),
               date_labels = "%Y",
               limits = c(ymd("1950-01-01"),ymd("2080-01-01"))) +
  coord_cartesian(xlim = c(ymd("1970-01-01"),ymd("2024-01-01"))) + 
  tema + 
  labs(x = "",
       y = "Ammonium (µg N/l)",
       col = "") + 
  scale_color_manual(values = "forestgreen", label = "Ammonium") +
  scale_y_log10() -> fig6.3_vj



tiff("Manuscript/Vand og Jord/Figures/figure 5.tiff", height = 600)  
fig6.2_vj / fig6.1_vj / fig6.3_vj + plot_layout(guides = "collect", axes = "collect_x") & theme(legend.position = "bottom")
dev.off()
##### Figure 7 #####
plot_data %>% 
  filter(type == "Silicium" & 
           year(date) >= 2010) %>% 
  ggplot(aes(date, value, col = type)) + 
  geom_point() + 
  scale_x_date(limits = c(ymd("2010-01-01"), ymd("20241231")),
               breaks = c(ymd("2010-01-01"),ymd("2015-01-01"),ymd("2020-01-01"),ymd("2024-01-01")),
               date_labels = "%Y") +
  tema + 
  labs(x = "År",
       y = "Silicium (mg/l)",
       col = "") + 
  scale_color_manual(values = "darkgoldenrod4")  + 
  theme(legend.position = "none") +
  scale_y_continuous(limits = c(0,7), breaks = seq(0,7,1),labels= seq(0,7,1)) -> fig7

tiff("Manuscript/Figures/figure 6.tiff")
fig7  
dev.off()
##### Figure 8 #####
read_csv("Data/Salt, ilt, temperatur, ph m_ling - S__20241025_094830.csv", 
         col_types = cols(Resultat = col_double()), 
         locale = locale(decimal_mark = ",", grouping_mark = ".")) %>% 
  select(date = Dato, 
         type = Parameter,
         value = Resultat,
         unit = Enhed) %>% 
  filter(type == "pH") %>% 
  mutate(date = dmy(date),
         value = if_else(value > 20, value/10, value),
         year = year(date)) %>% 
  reframe(ph = mean(value, na.rm=T),
            .by = year) -> ph

fig1_data %>% 
  filter(type == "Chlorophyl (ukorrigeret)",
         between(month(date), 5,9)) %>% 
  mutate(year = year(date)) %>% 
  reframe(chl = max(value, na.rm=T),
            .by = year) %>% 
  inner_join(ph,
            by = join_by(year)) %>% 
  ggplot(aes(chl,ph)) + 
  geom_point() + 
  scale_x_continuous(limits = c(100,400)) + 
  scale_y_continuous(limits = c(8,9.02),
                     breaks = c(8,8.25,8.5,8.75,9),
                     labels = c(8,8.25,8.5,8.75,9)) + 
  tema + 
  labs(x = "Klorofyl (µg/l)",
       y = "pH") -> fig8;fig8

tiff("Figures/Fig8.tiff")
fig8
dev.off()

#### Resultater ####

year_data %>% 
  filter(date >= 2010) %>% 
  nest_by(type) %>% 
  mutate(lm = list(lm(value ~ date, data = data)),
         coef = list(coef(lm)),
         p = list(summary(lm)$coefficients[,4]),
         rsq = list(summary(lm)$r.squared)) %>% 
  unnest_wider(c(coef,p,rsq), names_sep = "_")

year_data %>% 
  filter(date >= 2010) %>% 
  group_by(type) %>% 
  arrange(date) %>% 
  mutate(proc = value/first(value)*100) -> proc_data
  
proc_data %>% 
  filter(date %in% c(2010,2024)) %>% 
  select(date,type,proc) %>% 
  pivot_wider(names_from = type, values_from = proc)

proc_data %>% 
  ggplot() + 
  geom_line(aes(date,proc, col = type)) + 
  geom_point(aes(date,proc, col = type)) + 
  facet_wrap(~type, scales = "free", ncol = 1) + 
  tema

plot_data %>% 
  filter(type == "Phosphor, total-P", 
         year(date) %in% c(2016,2018)) %>% 
  mutate(doy = yday(date)) %>% 
  ggplot(aes(doy,value, col = as.factor(year(date)))) + 
  geom_point(size = 4) + tema + 
  labs(col = "År", x = "",
       y = "Fosfor koncentration (mg/l)") + 
  scale_x_continuous(breaks = c(1,32,60,91,121,152,182,213,244,274,305,335),
                     labels = c("Jan","Feb","Mar","Apr","Maj","Jun","Jul","Aug","Sep","Okt","Nov","Dec"))


#### Nedbør og temp 2016 + 2018 ####

  read_csv("Data/monthly_temp_precip.csv") %>% 
  ggplot() + 
  geom_col(aes(month, precip/5),fill = "blue") + 
  geom_line(aes(month, temp)) +
  geom_point(aes(month, temp)) +
  facet_wrap(~year(datetime), ncol = 1) + 
  scale_x_continuous(labels = c("Jan","Feb","Mar","Apr","Maj","Jun","Jul","Aug","Sep","Okt","Nov","Dec"),
                     breaks = 1:12) + 
  scale_y_continuous(sec.axis = sec_axis(~.*5, name = "Nedbør")) +
  tema + 
  labs(x = "",
       y = "Temperatur")

#### input vs sø koncentration ####
year_data %>% 
  filter(type == "Phosphor, total-P") %>% 
  rename(year = date) -> tp_concentration

fig4_new_df %>% 
  filter(name == "TP") %>% 
  select(year,arre,name_f) %>% 
  full_join(tp_concentration) %>% 
  arrange(year) %>% 
  ggplot(aes(arre,value, col = year)) + 
  geom_point(size = 4) + 
  scale_color_viridis_c() + 
  tema + 
  labs(y = bquote("Concentration in lake (mg TP l"^-1*")"), 
       x = bquote("TP input (ton TP y"^-1*")"),
       col = "Year") + 
  theme(legend.key.width = unit(3, 'cm'))
  
# Sediment P ----

tibble(year = c(rep(1988, 6),
                rep(1997, 4),
                rep(2009,6),
                rep(2014,4),
                rep(2018,4)),
       TP = c(13.07, 13.08,13.6,13.85,13.38, 13.38,
              10.6,11.1,9.6,10.8,
              11.4,9,8,8.07,12.3,8.4,
              10.4,10.5,9.4,10.3,
              12.17,12.04,8.31,8.93)) %>% 
  mutate(year = as.factor(year),
         year = factor(year, levels = c("1988","1997","2009","2014","2018"))) %>% 
aov(TP ~ year, data = .) %>% 
  TukeyHSD() %>% 
  broom::tidy() %>% 
  write_csv("/Users/jonas/Desktop/Arresø_P_sediment.csv")

tibble(year = c(rep(1988, 6),
                rep(1997, 5),
                rep(2009,6),
                rep(2014,6),
                rep(2018,4)),
       TP = c(2.56, 2.52, 2.6, 2.5, 2.2, 1.99,
              1.9,2.2,1.7,1.9,1.9,
              1.6,1.41,1.33,1.41,1.43,1.16,
              1.8,1.5,1.7,1.5,1.4,1.5,
              1.8,1.6,1.5,1.6)) %>% 
  mutate(year = as.factor(year),
         year = factor(year, levels = c("1988","1997","2009","2014","2018"))) %>% 
  aov(TP ~ year, data = .) %>% 
  TukeyHSD() %>% 
  broom::tidy() %>% 
  write_csv("/Users/jonas/Desktop/Arresø_TP_conc_sediment.csv")


# Tjek alkalinitet ----

plot_data %>% 
  filter(type == "Alkalinitet,total TA",
         year(date) > 2000,
         between(month(date), 5,9)) %>% 
  reframe(across(value, list(min = min, max = max, mean = mean, median = median)))

# Chl vs org P & Chl vs org N
plot_data %>% 
  filter(between(month(date), 5,9),
         type %in% c("Phosphor, total-P","Ortho-phosphat-P","Chlorophyl (ukorrigeret)")) %>% 
  reframe(value = mean(value, na.rm=T),
          .by = c(date,type)) %>% 
  pivot_wider(names_from = type, values_from = value) %>% 
  mutate(year = year(date)) %>% 
  reframe(across(`Phosphor, total-P`:`Chlorophyl (ukorrigeret)`, ~mean(.x, na.rm=T)),
          .by = year) %>% 
  mutate(org_P = `Phosphor, total-P`-`Ortho-phosphat-P`) %>% 
  lm(data = ., `Chlorophyl (ukorrigeret)` ~ org_P) %>% 
  summary

plot_data %>% 
  filter(between(month(date), 5,9),
         type %in% c("Nitrogen,total N","Nitrit+nitrat-N","Ammoniak+ammonium-N","Chlorophyl (ukorrigeret)")) %>% 
  reframe(value = mean(value, na.rm=T),
          .by = c(date,type)) %>% 
  pivot_wider(names_from = type, values_from = value) %>% 
  mutate(year = year(date)) %>% 
  reframe(across(`Nitrogen,total N`:`Chlorophyl (ukorrigeret)`, ~mean(.x, na.rm=T)),
          .by = year) %>% 
  mutate(org_N = `Nitrogen,total N`-(`Ammoniak+ammonium-N`+`Nitrit+nitrat-N`)) %>% 
  lm(data = ., `Chlorophyl (ukorrigeret)` ~ org_N) %>% 
  summary
