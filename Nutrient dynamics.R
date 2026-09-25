# Langtidsudvikling og næringsdynamik ved reduceret eutrofiering i en stor lavvandet sø, Arresø
source("Library.R")
# Fig S1 ----
sigt %>% 
  filter(between(month(date), 5,9)) %>% 
  mutate(year = year(date)) %>% 
  reframe(across(secchi, c(min = min, max = max,mean = mean)),
            .by = year) -> summer_secchi

plot_data %>% 
  filter(between(month(date), 5,9),
         type == "Chlorophyl (ukorrigeret)") %>% 
  mutate(year = year(date),
         chl = value) %>% 
  reframe(across(chl, c(min = min, max = max,mean = mean)),
          .by = year) -> summer_chl


full_join(summer_secchi,summer_chl) %>% 
  add_row(secchi_mean = c(.3,.2),
          secchi_min = c(NA_real_,.15),
          secchi_max = c(NA_real_,.25),
          year = c(1938,1956.5)) %>% 
  arrange(year) %>% 
  mutate(across(secchi_min:secchi_mean, ~.x*100))-> figS1_data

figS1_data %>%  
  ggplot(aes(year)) + 
  geom_ribbon(aes(ymin = secchi_min, ymax = secchi_max), fill = "cyan3", alpha = 0.5) +
  geom_line(aes(y = secchi_mean), col = "cyan3") + 
  geom_point(aes(y = secchi_mean), col = "cyan3",size =4) + 
  labs(x = "Year",
       y = "Summer Secchi depth (cm)") +
  tema + 
  scale_x_continuous(breaks = seq(1940,2020, 20)) + 
  scale_y_continuous(limits = c(0,110),
                     breaks = seq(0,100,20))-> figS1.1

figS1_data %>%  
  ggplot(aes(year)) +
  geom_ribbon(aes(ymin = chl_min, ymax = chl_max), fill = "forestgreen", alpha = 0.5) +
  geom_line(aes(y = chl_mean), col = "forestgreen") +
  geom_point(aes(y = chl_mean), col = "forestgreen",size =4) +
  labs(y = bquote("Summer Chlorophyll a (µg l"^-1*")"),
       x = "Year") + 
  tema + 
  scale_x_continuous(breaks = seq(1940,2020, 20))-> figS1.2

tiff("Manuscript/Nutrient dynamics/Figures/Figure S1.tiff", height = 700, width = 600)
figS1.1 / figS1.2 + plot_layout(axes = "collect")
dev.off()

# Fig 4 ----
read_excel("Manuscript/Nutrient dynamics/Data/Copy of arresøtal_ 17.02.xlsx", sheet = "Fig4", skip = 1) %>% 
  pivot_longer(Phosphate:Ammonia) %>% 
  mutate(name_f = as.factor(name),
         name_f = factor(name_f, levels = c("Phosphate","Nitrate","Ammonia"))) -> fig4_data

fig4_labeller <- as_labeller(c("Phosphate" = "Phosphate~(mg~P~l^-1)",
                               "Nitrate" = "Nitrate~(mg~N~l^-1)",
                               "Ammonia" = "Ammonia~(mg~N~l^-1)"),
                                   default = label_parsed)

fig4_nice_points <- tibble(year = rep(NA_real_,3),
                           name_f = c(rep("Nitrate",2),"Ammonia"),
                           value = c(0,2,0),
                           season = rep(NA_character_,3))

fig4_data %>% 
  ggplot(aes(year,value, col = season)) + 
  geom_point(size = 3) +
  geom_point(data = fig4_nice_points) + 
  facet_grid(name_f~1, labeller = fig4_labeller, scales = "free", switch = "y") + 
  scale_color_manual(values = c("darkgoldenrod1", "deepskyblue2"),
                     labels = c("Summer","Winter"),
                     breaks = c("summer","winter")) + 
  tema + 
  scale_x_continuous(breaks = seq(1967,1976,3)) + 
  labs(x = "Year",
       y = "",
       col = "") -> fig4

tiff("Manuscript/Nutrient dynamics/Figures/Figure 4.tiff", width = 700)
fig4
dev.off()

# Figure 9 ----
read_excel("Manuscript/Nutrient dynamics/Data/Copy of arresøtal_ 17.02.xlsx", sheet = "Figur 9") %>% 
  pivot_longer(`1988`:`2018`, names_to = "year") %>% 
  mutate(depth = as.factor(depth),
         depth = factor(depth, levels = c("0-2","2-5","5-10","10-20","20-30","30-40"))) -> fig9_data

fig9_data %>% 
  ggplot(aes(depth,value, col = year, group = year)) + 
  geom_point(size = 3) + 
  geom_line() +
  coord_flip() + 
  scale_x_discrete(limits = c("30-40","20-30","10-20","5-10","2-5","0-2")) + 
  scale_y_continuous(limits = c(0,2.5))+ 
  scale_color_viridis_d() +
  tema + 
  labs(y = bquote("Total Phosphorus (mg P (g DW)"^-1*")"),
       x = "Sediment depth (cm)",
       col = "Year") + 
  guides(col=guide_legend(nrow=2,byrow=TRUE)) -> fig9

tiff("Manuscript/Nutrient dynamics/Figures/Figure 9.tiff")
fig9
dev.off()

# fig 10 ----
read_excel("Manuscript/Nutrient dynamics/Data/Copy of arresøtal_ 17.02.xlsx", sheet = "Fig10") %>% 
  pivot_longer(`1988`:`2018`, names_to = "year") -> fig10_data

  fig10_data %>% 
    filter(type == "TP") %>% 
    aov(value~year, data = .) %>% 
    TukeyHSD() %>% 
    broom::tidy() %>% 
    rstatix::add_significance(p.col = "adj.p.value") %>% 
    separate(contrast, into = c("group1","group2"), sep = "-") %>% 
    filter(!adj.p.value.signif == "ns")-> stat_test_TP
  
fig10_data %>% 
  filter(type == "TP") %>% 
  ggplot(aes(year, value)) + 
  geom_jitter(size = 3, shape =21,width = 0.05) +  
  tema +
  ggpubr::stat_pvalue_manual(stat_test_TP, label = "adj.p.value.signif", y.position = seq(3,4,0.2),
                             tip.length = 0.01,size = 6) + 
  labs(x = "Year",
       y = bquote("Phosphorus (mg TP (g DW)"^-1*")")) +
  scale_y_continuous(limits = c(1,4)) -> fig10_TP_plot

fig10_data %>% 
  filter(type == "OC") %>% 
  mutate(value = value*4.4) %>% 
  aov(value~year, data = .) %>% 
  TukeyHSD() %>% 
  broom::tidy() %>% 
  rstatix::add_significance(p.col = "adj.p.value") %>% 
  separate(contrast, into = c("group1","group2"), sep = "-") %>% 
  filter(!adj.p.value.signif == "ns")-> stat_test_OC

fig10_data %>% 
  filter(type == "OC") %>% 
  ggplot(aes(year, value*4.4)) + 
  geom_jitter(size = 3, shape =21,width = 0.05) +  
  tema +
  ggpubr::stat_pvalue_manual(stat_test_OC, label = "adj.p.value.signif", y.position = 4.4*seq(50,58,2),
                             tip.length = 0.01,size = 6) + 
  labs(x = "Year",
       y = bquote("Organic Carbon (mg OC (g DW)"^-1*")")) -> fig10_OC_plot

fig10_data %>% 
  filter(type == "TP_OC") %>% 
  aov(value~year, data = .) %>% 
  TukeyHSD() %>% 
  broom::tidy() %>% 
  rstatix::add_significance(p.col = "adj.p.value") %>% 
  separate(contrast, into = c("group1","group2"), sep = "-") %>% 
  filter(!adj.p.value.signif == "ns")-> stat_test_tpoc

fig10_data %>% 
  filter(type == "TP_OC") %>% 
  ggplot(aes(year, value)) + 
  geom_jitter(size = 3, shape =21,width = 0.05) +  
  tema +
  ggpubr::stat_pvalue_manual(stat_test_tpoc, label = "adj.p.value.signif", y.position = seq(14.3,16.5,0.6),
                             tip.length = 0.01,size = 6) + 
  scale_y_continuous(limits = c(8,16.6),
                     breaks = c(8,10,12,14,16))+
  labs(x = "Year",
       y = bquote("Phosphorus (mg TP (g OC)"^-1*")")) -> fig10_tpoc_plot

fig10_TP_plot / fig10_OC_plot / fig10_tpoc_plot + plot_layout(axes = "collect") -> fig10

tiff("Manuscript/Nutrient dynamics/Figures/Figure 10.tiff", height = 800)
fig10
dev.off()

# New fig ----

read_excel("Manuscript/Nutrient dynamics/Data/Copy of arresøtal_ 17.02.xlsx", sheet = "Fig_new", skip = 1) %>% 
  mutate(date = ydm(date),
         doy = yday(date),
         year = year(date)) -> new_fig_df

doy_axes <- tibble(label = c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"),
                   doy = c(1,32,60,91,121,151,182,213,244,274,305,335))

plot_data %>% 
  filter(type == "Phosphor, total-P") %>% 
  reframe(TP = mean(value)*1000,
            .by = date) %>% 
  mutate(doy = yday(date),
         year = year(date)) %>% 
  drop_na(TP, date)-> new_fig_df2

new_fig_df2 %>% 
  filter(year %in% c(1976,1985,1992,2002,2014)) %>% 
  ggplot(aes(doy, TP/1000, col = as.factor(year), group = as.factor(year))) + 
  geom_rect(aes(xmax = 121, xmin = -Inf, ymin = -Inf, ymax = Inf), fill = "grey80", col = NA) +
  geom_rect(aes(xmax = Inf, xmin = 244, ymin = -Inf, ymax = Inf), fill = "grey80", col = NA) +
  geom_point(size = 3) + 
  geom_line() + 
  scale_color_viridis_d() +
  scale_x_continuous(breaks = doy_axes$doy,
                     labels = doy_axes$label) +
  tema + 
  labs(x = "",
       y = bquote("Total Phosphorus (mg P l"^-1*")"),
       col = "Year") + 
  scale_y_continuous(limits = c(0,1500)/1000,
                     breaks = seq(0,1500,500)/1000) -> fig_new

new_fig_df2 %>% 
  filter(year %in% c(1976,1985,1992,2002,2014),
         between(month(date),5,9)) %>% 
  reframe(diff_summer = max(TP)-min(TP),
            .by = year)

new_fig_df2 %>% 
  filter(year %in% c(1976,1985,1992,2002,2014),
         month(date) > 5) %>% 
  reframe(diff = max(TP)-min(TP),
          .by = year)

tiff("Manuscript/Nutrient dynamics/Review/Figure 6.tiff", width = 600)
fig_new
dev.off()

# Range plot ----

new_fig_df2 %>% 
  reframe(range = (max(TP)-min(TP))/1000,
            .by = year) %>% 
  ggplot(aes(year, range)) + 
  geom_point(size = 3) + 
  geom_smooth(se = F) +
  labs(x = "Year",
       y = bquote("Total Phosphorus range (mg TP l"^-1*")")) + 
  tema +
  scale_y_continuous(limits = c(0,1.200),
                     breaks = seq(0,1.200,.300)) +
  scale_x_continuous(limits = c(1970,2024),
                     labels = seq(1970,2024, 10),
                     breaks = seq(1970,2024, 10)) -> fig_range;fig_range

tiff("Manuscript/Nutrient dynamics/Review/Figure S3.tiff", width = 600)
fig_range
dev.off()
