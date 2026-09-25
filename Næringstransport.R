source("library.R")

#### Bendstrup ####

stream_load_function("Data/Vandløbsdata/Bendstrup/Vandkemi - Vandl_b_20241115_112022.csv",
                     "Data/Vandløbsdata/Bendstrup/Pøle å, Bendstrup.xlsx",
                     ymd("1900-01-01"),
                     ymd("2010-01-01")) %>% mutate(stream = "Bendstrup") -> bend_load

bend_load %>% 
  ggplot(aes(date, value, col = name)) + 
  geom_line() + 
  facet_wrap(~name, ncol = 1, scales = "free_y")

#### Lyngy å ####

stream_load_function("Data/Vandløbsdata/Lyngby å/Vandkemi - Vandl_b_20241115_111941.csv",
                     "Data/Vandløbsdata/Lyngby å/49.19_Vandforing, Dognmiddel (DMP)_Dag.xlsx",
                     ymd("1900-01-01"),
                     ymd("2024-01-01")) %>% mutate(stream = "Lyngby") %>% 
  drop_na(value) -> lyngby_load

lyngby_load %>% 
  ggplot(aes(date, value, col = name)) + 
  geom_line() + 
  facet_wrap(~name, ncol = 1, scales = "free_y")

#### Æbelholt å ####
stream_load_function("Data/Vandløbsdata/Æbelholt Å/Vandkemi - Vandl_b_20241115_112001.csv",
                     "Data/Vandløbsdata/Æbelholt Å/49000061_Vandforing, Dognmiddel (DMP)_Dag.xlsx",
                     ymd("1900-01-01"),
                     ymd("2024-01-01")) %>% mutate(stream = "Æbelholt") %>% 
  drop_na(value) -> æbel_load

æbel_load %>% 
  ggplot(aes(date, value, col = name)) + 
  geom_line() + 
  facet_wrap(~name, ncol = 1, scales = "free_y")

#### Syd for Alsønderup ####
stream_load_function("Data/Vandløbsdata/Syd for Alsønderup/Vandkemi - Vandl_b_20241115_112106.csv",
                     "Data/Vandløbsdata/Syd for Alsønderup/49000094_Vandforing, Dognmiddel (DMP)_Dag.xlsx",
                     ymd("1900-01-01"),
                     ymd("2024-01-01")) %>% mutate(stream = "Syd for Alsønderup") %>% 
  drop_na(value) -> SydAlsonderup_load

SydAlsonderup_load %>% 
  ggplot(aes(date, value, col = name)) + 
  geom_line() + 
  facet_wrap(~name, ncol = 1, scales = "free_y")

#### Arresø Kanal ####
stream_load_function("Data/Vandløbsdata/Arresø kanal/Vandkemi - Vandl_b_20241115_111909.csv",
                     "Data/Vandløbsdata/Arresø kanal/49000054_Vandforing, Dognmiddel (DMP)_Dag.xlsx",
                     ymd("1900-01-01"),
                     ymd("2024-01-01")) %>% mutate(stream = "Arresø Kanal")  -> arrekanal_load

arrekanal_load %>% 
  ggplot(aes(date, value, col = name)) + 
  geom_line() + 
  facet_wrap(~name, ncol = 1, scales = "free_y")

#### Ramløse å ####
stream_load_function("Data/Vandløbsdata/Ramløse å/Vandkemi - Vandl_b_20241119_140518.csv",
                     "Data/Vandløbsdata/Ramløse å/49000295_Vandforing, Dognmiddel (DMP)_Dag.xlsx",
                     ymd("1900-01-01"),
                     ymd("2024-01-01")) %>% mutate(stream = "Ramløse Å")  -> ramlose_load

ramlose_load %>% 
  ggplot(aes(date, value, col = name)) + 
  geom_line() + 
  facet_wrap(~name, ncol = 1, scales = "free_y") +
  tema + 
  labs(y = "gram")


#### Combine ####

bind_rows(bend_load,
          lyngby_load,
          æbel_load,
          SydAlsonderup_load,
          arrekanal_load) %>% 
  filter(between(date,ymd("1989-01-01"),ymd("2022-01-01"))) %>% 
  mutate(name = case_when(name == "N_load" ~ "TN",
                          name == "P_load" ~ "TP"),
         value = value*3600*24/1000) -> all_loading

all_loading %>% 
  ggplot(aes(date, value, col = stream)) + 
  geom_line() + 
  facet_grid(name ~ 1, scales = "free_y") +
  scale_x_date(date_breaks = "5 years", date_labels = "%Y") +
  theme(strip.text.x = element_blank()) + 
  tema + 
  labs(x = "",
       y = "Nutrient loading (kg)",
       col = "") + 
  scale_color_manual(limits = c("Æbelholt","Bendstrup","Lyngby","Syd for Alsønderup","Arresø Kanal"),
                     values = c("steelblue","steelblue1","steelblue2","steelblue3", "forestgreen"))

all_loading %>% 
  filter(stream %in% c("Syd for Alsønderup","Bendstrup")) %>% 
  ggplot(aes(date, value, col = stream)) + 
  geom_line() + 
  facet_grid(name ~ 1, scales = "free_y") +
  scale_x_date(date_breaks = "5 years", date_labels = "%Y") +
  theme(strip.text.x = element_blank()) + 
  tema + 
  labs(x = "",
       y = "Nutrient loading (kg)",
       col = "") + 
  scale_color_manual(limits = c("Bendstrup","Syd for Alsønderup"),
                     values = c("darkorange", "forestgreen"))

#### Transport in and out of Arresø ####

all_loading %>% 
  filter(!stream == "Bendstrup") %>% 
  mutate(flow = case_when(stream == "Arresø Kanal" ~ "Out",
                          T ~ "In"),
         year = year(date)) %>% 
  reframe(value = sum(value, na.rm=T),
          .by = c(flow,year,name)) -> transport_arre
  
transport_arre %>% 
  mutate(periode = case_when(between(year,1990,2000) ~ "1990-2000",
                            between(year,2001,2011) ~ "2001-2011",
                            between(year,2012,2022) ~ "2012-2022"),
         value = value/1000) %>% 
  drop_na(periode) %>% 
  reframe(avg = mean(value,na.rm=T), 
          sem = sem(value),
          sd = sd(value, na.rm=T),
          .by = c(periode,name,flow))

transport_arre %>% 
  mutate(periode = case_when(between(year,1990,2000) ~ "1990-2000",
                             between(year,2001,2011) ~ "2001-2011",
                             between(year,2012,2022) ~ "2012-2022"),
         value = value/1000) %>% 
  drop_na(periode) %>% 
  filter(name == "TP", flow == "Out") %>% 
  aov(value~periode, data = .) %>% 
  TukeyHSD()

transport_arre %>% 
  ggplot(aes(year,value/1000, fill = flow)) + 
  geom_col(position = position_dodge2()) + 
  facet_wrap(~name, scales = "free_y", ncol = 1) + 
  tema + 
  labs(y = "Ton",
       fill = "", 
       x = "")

#### Transport from each stream ####

all_loading %>% 
  mutate(year = year(date)) %>% 
  reframe(value = sum(value/1000, na.rm=T),
          .by = c(stream,year,name)) %>% 
  pivot_wider(names_from=year, values_from = value) %>% 
  arrange(name,stream) %>% 
  write_csv("Output/Yearly_nutrient_transport.csv")

all_loading %>% 
  mutate(year = year(date)) %>% 
  distinct(date,stream,year,discharge) %>% 
  mutate(discharge = discharge*3600*24/1000) %>% 
  reframe(discharge = sum(discharge, na.rm=T),
          n = n(),
          .by = c(stream,year)) %>% 
  filter(n > 360) %>% 
  pivot_wider(names_from=year, values_from = discharge)  %>% 
  write_csv("Output/Yearly_discharge.csv")
  
all_loading %>% 
  mutate(year = year(date),
         month = month(date)) %>% 
  filter(name == "TP") %>% 
  select(-c(name,value)) %>% 
  mutate(discharge = discharge*3600*24/1000,
         time_of_year = case_when(month %in% c(12,1,2,3) ~ "vinter",
                                  between(month, 4,7) ~ "sommer"),
         ) %>% 
  drop_na(time_of_year) %>% 
  reframe(TN_load = sum(discharge, na.rm=T)*mean(TN, na.rm=T),
          TP_load = sum(discharge, na.rm=T)*mean(TP, na.rm=T),
          n = n(),
          .by = c(stream,year,time_of_year)) %>% 
  filter(n > 110) %>% 
  select(-n) %>% 
  arrange(time_of_year, stream) %>%
  pivot_longer(TN_load:TP_load) %>% 
  ggplot(aes(year,value*10^-6, fill = stream, group = time_of_year)) + 
  geom_col(position = position_dodge2()) +
  facet_grid(name~time_of_year, scales = "free_y") + 
  labs(x = "",
       y = "kg") + 
  tema

#### Figure 4
all_loading %>% 
  mutate(year = year(date)) %>% 
  filter(name == "TP",
         stream %in% c("Æbelholt","Lyngby","Syd for Alsønderup", "Arresø Kanal")) %>% 
  select(-c(name,value)) %>% 
  mutate(discharge = discharge*3600*24, # l/s -> l/d
         TN_load = discharge*TN*10^-9,        # mg/d -> t/d
         TP_load = discharge*TP*10^-9) %>% 
  reframe(TN_load = sum(TN_load, na.rm=T),
          TP_load = sum(TP_load, na.rm=T),
          n = n(),
          .by = c(stream,year)) %>% 
  mutate(TN_load = case_when(stream == "Arresø Kanal" ~ -TN_load,
                             T ~ TN_load),
         TP_load = case_when(stream == "Arresø Kanal" ~ -TP_load,
                             T ~ TP_load)) %>% 
  add_row(TN_load = c(50), TP_load = 10) %>% 
  filter(n > 360) %>% 
  select(-n) %>% 
  arrange(stream) %>%
  pivot_longer(TN_load:TP_load) %>% 
  mutate(name = case_when(name == "TN_load" ~ "Total Nitrogen",
                          name == "TP_load" ~ "Total Phosphor"))-> fig4_df
  
fig4_df %>% 
  filter(name == "Total Nitrogen") %>% 
  ggplot(aes(year,value, fill = stream)) + 
  geom_col() +
  facet_wrap(~name) + 
  labs(x = "",
       fill = "",
       y = bquote("Nutrient transport (ton year"^-1*")")) + 
  tema + 
  scale_fill_discrete(limits = c("Arresø Kanal","Syd for Alsønderup","Æbelholt","Lyngby"),
                      labels = c("Outlet","Pøleå","Æbelholt Å","Lyngby Å")) +
  scale_y_continuous(limits = c(-300,200),
                     labels = seq(-300,200,100),
                     breaks = seq(-300,200,100)) -> fig4_del1

fig4_df %>% 
  filter(name == "Total Phosphor") %>% 
  ggplot(aes(year,value, fill = stream)) + 
  geom_col() +
  facet_wrap(~name) + 
  labs(x = "",
       fill = "",
       y = bquote("Nutrient transport (ton year"^-1*")")) + 
  tema + 
  scale_fill_discrete(limits = c("Arresø Kanal","Syd for Alsønderup","Æbelholt","Lyngby"),
                      labels = c("Outlet","Pøleå","Æbelholt Å","Lyngby Å")) +  
  scale_y_continuous(limits = c(-32,10),
                     labels = seq(-30,10,10),
                     breaks = seq(-30,10,10)) -> fig4_del2
  
fig4_del1 / fig4_del2 + plot_layout(guides = "collect", axes = "collect_x", axis_titles = "collect_y") & theme(legend.position = "bottom") -> fig4

tiff("Manuscript/Figures/figure 4.tiff") 
fig4
dev.off()

#### Retention i engsøer - figur 3 ####
read_excel("Data/Arresø tal fra Kaj.xlsx", sheet = 2) %>% 
  rename(unit = 1, parameter = 2) %>% 
  filter(!is.na(parameter)) %>% 
  select(parameter:9) %>% 
  pivot_longer(Solbjerg...3:Strødam...9) %>% 
  mutate(age = case_when(name =="Solbjerg...3" ~ 1,
                         name =="Solbjerg...4" ~ 2,
                         name =="Solbjerg...5" ~ 3,
                         name =="Solbjerg...6" ~ 4,
                         name =="Solbjerg...7" ~ 5,
                         name =="Strødam...8" ~ 1,
                         name =="Strødam...9" ~ 2),
         name = substr(name, 1,nchar(name)-4)) %>% 
  pivot_wider(names_from = parameter, values_from = value) %>% 
  select(-Alder) %>% 
  rename(lake = name, inflow_tp = 3, retention_tp = 4, retention_time = 5, inflow_tn = 6, retention_tn =7, retention_percent_tn = 8)-> engsø_df

engsø_df %>%
  mutate(retention_tp = -retention_tp,
         retention_tn = -retention_tn) %>% 
  pivot_longer(inflow_tp:retention_tp, names_to = "tp_name", values_to = "tp_value") %>% 
  pivot_longer(inflow_tn:retention_tn,names_to = "tn_name", values_to = "tn_value") %>% 
  add_row(lake = rep("Strødam",3), age = c(3,4,5), tn_value = rep(NA_real_, 3))  -> fig3_df
  
fig3_df %>% 
  distinct(lake,age,tp_value,tp_name) %>% 
  drop_na(tp_value) -> fig3.1_df

ggplot(data = fig3.1_df, aes(fill = interaction(lake, tp_name))) + 
  geom_hline(yintercept = 0, linetype = "dotted") +
  geom_col(data = filter(fig3.1_df, tp_name == "inflow_tp"), aes(age,tp_value), width = 0.6, position = position_dodge2(preserve = "single"), col = "black",) + 
  geom_col(data = filter(fig3.1_df, tp_name == "retention_tp"), aes(age,tp_value), width = 0.6, position = position_dodge2(preserve = "single"), col = "black",) + 
  tema + 
  labs(x = "Age (Years)",
       y = bquote("Phosphorus (g TP m"^-2*" y"^-1*")"),
       fill = "") + 
  scale_fill_discrete(breaks = c("Solbjerg.inflow_tp","Strødam.inflow_tp"),
                      type = c("deepskyblue","darkorange","deepskyblue4", "darkorange4"),
                      labels = c("Solbjerg","Strødam")) +
  scale_y_continuous(limits = c(-30,40), 
                     breaks = seq(-30,40,10),
                     labels = seq(-30,40,10)) -> fig3.1;fig3.1

fig3_df %>% 
  distinct(lake,age,tn_value,tn_name) %>% 
  drop_na(tn_value) -> fig3.2_df

ggplot(data = fig3.2_df, aes(fill = interaction(lake, tn_name))) + 
  geom_hline(yintercept = 0, linetype = "dotted") +
  geom_col(data = filter(fig3.2_df, tn_name == "inflow_tn"), aes(age,tn_value), width = 0.6,position = position_dodge2(preserve = "single"), col = "black",) + 
  geom_col(data = filter(fig3.2_df, tn_name == "retention_tn"), aes(age,tn_value), width = 0.6, position = position_dodge2(preserve = "single"), col = "black",) +
  tema + 
  labs(x = "Age (Years)",
       y = bquote("Nitrogen (g TN m"^-2*" y"^-1*")"),
       fill = "") + 
  scale_fill_discrete(breaks = c("Solbjerg.inflow_tn","Strødam.inflow_tn"),
                      type = c("deepskyblue","darkorange","deepskyblue4", "darkorange4"),
                      labels = c("Solbjerg","Strødam")) +
  scale_y_continuous(limits = c(-100,300), 
                     breaks = seq(-100,300,100),
                     labels = seq(-100,300,100)) -> fig3.2;fig3.2

fig3.1 / fig3.2 + plot_layout(guides = "collect", axes = "collect_x") & theme(legend.position = "bottom") -> fig3

tiff("Manuscript/Figures/figure 8.tiff") 
fig3
dev.off()

#### Stoftransport til Arresø - Figur 4 ####
fig4_df %>% 
  filter(stream %in% c("Lyngby","Syd for Alsønderup","Æbelholt")) %>% 
  mutate(name = case_when(name == "Total Nitrogen" ~ "TN",
                          name == "Total Phosphor" ~ "TP"))-> inflow_fig4_df

read_excel("Data/Arresø tal fra Kaj.xlsx", sheet = 3, range = "A2:R6") %>% 
  pivot_longer(`1`:`16`) %>% 
  mutate(year = parse_number(name)+1988) %>% 
  select(type,year,parameter,value) %>% 
  filter(type == "Input") %>% 
  mutate(parameter = case_when(parameter == "Tons P/år" ~ "TP",
                               parameter == "Tons N/år" ~ "TN")) %>% 
  rename(arre = value,
         name = parameter) %>% 
  {. ->> old_data_input} %>% 
  inner_join(inflow_fig4_df)  %>% 
  pivot_wider(names_from = stream, values_from = value) %>% 
  reframe(value = sum(Lyngby,Æbelholt,`Syd for Alsønderup`),
          arre = mean(arre),
          .by = c(year,name)) %>% 
  drop_na(value,arre) %>% mutate(arre/value)
  reframe(mean_stream_proportion = mean(value),
          .by = name) -> stream_proportions_df
  
stream_proportions_df_KSJ <- tibble(name = c("TP","TN"),
                                    mean_stream_proportion = c(1.43107531975367,1.67435669920142))

my_labeller <- as_labeller(c(TP="Total~Phosphorus~(ton~TP~y^-1)", TN="Total~Nitrogen~(ton~TN~y^-1)"),
                           default = label_parsed)

nice_points_fig4 <- tibble(data_type = rep(NA_character_,4),arre = c(0,30,0,600), name = c("TP","TP","TN","TN"), year = rep(NA_real_,4)) %>% 
  mutate(name_f = factor(name, levels = c("TP","TN")))

inflow_fig4_df %>% 
  reframe(value = sum(value),
            .by = c(year,name)) %>% 
  full_join(stream_proportions_df_KSJ, 
              by = join_by(name)) %>% 
  full_join(old_data_input) %>% 
  select(-type) %>% 
  add_row(year = c(1973, 1973, 1976, 1976),
          name = c("TN","TP","TN","TP"),
          arre = c(266,83.4,397,84.5)) %>% 
  mutate(data_type = case_when(is.na(arre) ~ "predicted",
                               is.numeric(arre) ~ "measured"),
         arre = case_when(is.na(arre) ~ value*mean_stream_proportion,
                          T ~ arre),
         name = case_when(name == "Total Nitrogen" ~ "TN",
                          name == "Total Phosphor" ~ "TP",
                          T ~ name),
         predicted_stream_input = mean_stream_proportion*arre,
         name_f = factor(name, levels = c("TP","TN"))) -> fig4_new_df
  
outflow %>%
  add_row(year = c(1976, 1976, 1979, 1979),
          name = c("TN","TP","TN","TP"),
          out = c(230.6,56.6,230.6,56.6)) %>% 
  mutate(name_f = factor(name, levels = c("TP","TN")),
         data_type = "measured") -> fig4_df_2

ggplot() + 
  geom_point(data = fig4_new_df, aes(year, arre, col = "Input"), size = 3) + 
  geom_point(data = fig4_df_2, aes(year, out, col = "Output"), size = 3) + 
  tema + 
  geom_point(data = nice_points_fig4,aes(year, arre)) +
  facet_grid(name_f~., scales = "free_y", labeller = my_labeller, switch="y") +
  scale_color_manual(values = c("red3","cyan3")) +
  labs(x = "",
       y = "", 
       col = "") -> fig4_new;fig4_new

fig4_new_df %>% 
  select(year, input = arre, nutrient = name) -> temp1

fig4_df_2 %>% 
  select(year, nutrient = name, outflow = out) -> temp2

full_join(temp1, temp2) %>% 
  select(year,nutrient, input,outflow) %>% 
  mutate(unit = "ton y-1") %>% 
  write_csv("C:/Users/jonas/OneDrive - Syddansk Universitet/Arresø/Bog/Data/df5.2.csv")


tiff("Manuscript/Nutrient dynamics/Figures/figure 5.tiff") 
fig4_new
dev.off()

nice_points_suppl <- tibble(name_f = c("TN","TN","TP","TP"), 
                            input = c(100,600,5,30),
                            out = c(0,300,0,40))

full_join(fig4_new_df,fig4_df_2,
            by = join_by(year, name_f)) %>% 
  select(year,name_f, input = arre, out) %>% 
  filter(name_f == "TN",
         !(out > 250 & 
           input <300)
         ) %>% 
  drop_na(input) -> figS2_df

# rigr::regress(data = figS2_df, 
#               formula = out ~ input, 
#               fnctl = "geometric mean") 
# 
lm(data = figS2_df, out ~ input + 0) %>% summary

figS2_df %>% 
  ggplot(aes(input,out, fill = name_f)) + 
  geom_point(size = 3, shape = 21) + 
  geom_point(data = nice_points_suppl, fill = NA, shape = NA) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  geom_text(aes(x = 274, y = 266, label = "(    )"))+
  geom_point(aes(x = 274, y = 265, fill = "TN"), shape =21, size = 3) + 
  ggpmisc::stat_poly_eq(ggpmisc::use_label("eq"),formula = y ~ x+ 0) +
  ggpmisc::stat_poly_eq(label.y = 0.9, formula = y ~ x+ 0) +
  #geom_abline(slope = 0.3345, intercept = 21.05, linetype = "dotted") +
  geom_smooth(method = "lm", fill = NA, col = "black", formula = y ~ x+ 0) +
  labs(x = bquote("Input (ton y"^-1*")"),
       y = bquote("Outflow (ton y"^-1*")"),
       fill = "") +
  scale_fill_manual(labels = c("Total Nitrogen"), values = "darkorange") +
  tema -> suppl_fig;suppl_fig

tiff("Manuscript/Nutrient dynamics/Figures/Figure S2.tiff")
suppl_fig
dev.off()

ggplot() + 
  geom_point(data = fig4_new_df, aes(year, arre, fill = data_type, shape = "Input"), size = 3) + 
  geom_point(data = fig4_df_2, aes(year, out, fill = data_type, shape = "Output"), size = 3) 



fig4_new_df %>% 
  select(year, data_type, input = arre, name) %>% 
  pivot_wider(names_from = name, values_from = input) %>% 
  rename(TN_t_y1 = TN, TP_t_y1 = TP) -> fig4_df_output

arrekanal_load %>% 
  select(date:TP) %>% 
  mutate(year = year(date)) %>% 
  distinct() %>% 
  reframe(n_discharge = sum(!is.na(discharge)),
          discharge = mean(discharge*3600*24, na.rm=T),
          n_tn = sum(!is.na(TN)),
          TN = mean(TN, na.rm =T),
          n_tp = sum(!is.na(TP)),
          TP = mean(TP, na.rm =T),
            .by = year) %>% 
  filter(n_tn > 350 & n_tp > 350 & n_discharge > 350) %>% 
  mutate(tn_load = discharge*TN*10^-9*365,
         tp_load = discharge*TP*10^-9*365) %>%  
  select(year, discharge, tn_load,tp_load) %>% 
  full_join(fig4_df_output) %>% 
  write_csv("Output/Arresø_kanal_outflow.csv")

#### Retention i Arresø - figur 5####
read_excel("Data/Arresø tal fra Kaj.xlsx", sheet = "Sheet5", range = "A1:R8") %>% 
  pivot_longer(`1989`:`2004`) %>% 
  rename(unit = 1, type = 2, year = name) %>% 
  select(-unit) %>% 
  pivot_wider(names_from = type, values_from = value) %>% 
  mutate(procent_retention_tp = Retention_tp/Tilført_tp*100,
         procent_retention_tn = Retention_tn/Tilført_tn*100) %>% 
  select(-`Procent retention`) -> fig5_df

fig5_df %>% 
  ggplot(aes(Opholdstid,Retention_tp)) + 
  geom_hline(yintercept = 0, linetype = "dotted") +
  geom_point(size = 3, aes(col = "Total Phosphorus")) + 
  tema + 
  geom_smooth(formula = y ~ log(x), se =F, col = "black", method = "glm") +
  scale_color_manual(values = "darkorange") +
  labs(x = "Water retention time (Years)",
       col = "",
       y = bquote("Retention (g TP m"^-2*" y"^-1*")")) -> fig5.1; 

fig5_df %>% 
  ggplot(aes(Opholdstid,Retention_tn)) + 
  geom_point(size = 3, aes(col = "Total Nitrogen")) + 
  tema + 
  geom_smooth(method = "lm", se =F, col = "black") +
  scale_color_manual(values = "royalblue") +
  scale_y_continuous(limits = c(3,12.4),
                     breaks = c(3,6,9,12)) +
  labs(x = "Water retention time (Years)",
       col ="",
       y = bquote("Retention (g TN m"^-2*" y"^-1*")")) -> fig5.2

fig5.1 / fig5.2 + plot_layout(guides = "collect", axes = "collect_x") & theme(legend.position = "bottom") -> fig5

tiff("Manuscript/Figures/figure 5.tiff") 
fig5
dev.off()


#### Spildevandsprøver ####
read_delim("Data/Rensningsanlæg/Renseanl_g (Spildevand)_20241206_100520.csv", 
           delim = ";", escape_double = FALSE, 
           col_types = cols(Dato = col_character(), Resultat = col_double()), 
           locale = locale(decimal_mark = ",",grouping_mark = "."), trim_ws = TRUE) %>% 
  select(date = Dato, `Målested navn`,Prøvetype, Status, Analysefraktion, Stofparameter, Resultat,Enhed) %>% 
  mutate(date = dmy(date)) %>% 
  arrange(date) -> spilde_helsinge

spilde_helsinge %>% 
  filter(Stofparameter == "Phosphor, total-P",
         Resultat < 400) %>% 
  ggplot(aes(date, Resultat, col= `Målested navn`)) + 
  geom_point() + 
  labs(y = "Total fosfor (mg/l)")

#### Retention Arresø ####
39.9*3.1*10^6 # arresø Volume

read_excel("Output/Copy of Copy of Arresø_kanal.xlsx", sheet = 1, range = "A2:D38") %>% 
  rename(year = 1, discharge = 2, TN = 3, TP = 4) %>% 
  pivot_longer(TN:TP, values_to = "out") %>% 
  mutate(wrt = 1/((discharge*365/1000)/(39.9*3.1*10^6))) %>% 
  select(-discharge)-> outflow

fig4_new_df %>% 
  select(year,name, arre, data_type) %>% 
  rename(input = arre) %>% 
  full_join(outflow) %>% 
  mutate(retention = input-out,
         retention_perc = retention/input*100,
         retention_m2 = (retention*10^6)/(39.9*10^6),
         name_f = factor(name, levels = c("TP","TN"))) -> fig8_df 
  
fig8_df %>% 
  ggplot(aes(wrt,retention_perc)) + 
  geom_hline(yintercept = 0, linetype = "dotted") +
  geom_point(size = 3, aes(col = name_f)) + 
  #geom_point(data = nice_points_fig8, aes(wrt,retention_perc), col = NA) + 
  facet_grid(name_f~., scales = "free") +#,labeller = my_labeller_fig8,switch="y") +
  tema + 
  geom_smooth(formula = y ~ log(x), se =F, col = "black", method = "glm") +
  labs(x = "Water retention time (Years)",
       col = "",
       y = "") + 
  scale_color_manual(values = c("royalblue","darkorange"),
                     labels = c("Total Phosphorus","Total Nitrogen")) -> fig8

tiff("Manuscript/Figures/figure 8.tiff", height = 550) 
fig8
dev.off()

#### TN tilbageholdelse i engsøer ####
#>lake sizes
#> Solbjerg = 325816 m2
#> Alsønderup = 498383 m2
#> Holløse Bredning = 500779 m2
#> Strødam = 153682 m2

fig8_df %>% 
  arrange(year) %>% 
  ungroup %>% 
  filter(name == "TN",
         year > max(year)-10) %>% 
  reframe(retention = mean(retention))

fig3.2_df %>% 
  filter(age > 2, tn_name == "retention_tn") %>% 
  reframe(tn_value = -mean(tn_value)) %>% 
  mutate(engsø_tn_removal = tn_value*(325816+498383+500779+153682)*10^-6,
         total_tn_removal = engsø_tn_removal + 95.4)

# Næringstilførelse til søen vs org P og org N om sommeren ----

full_join(fig4_new_df,fig4_df_2,
          by = join_by(year, name_f)) %>% 
  select(year,name_f, input = arre, out) -> lake_nutrient_flow

plot_data %>% 
  filter(type %in% c("Phosphor, total-P","Nitrogen,total N","Ortho-phosphat-P","Ammoniak+ammonium-N","Nitrit+nitrat-N",
                     "Chlorophyl (ukorrigeret)"),
         between(month(date), 5,9)) %>% 
  select(date,type,value) %>% 
  pivot_wider(names_from = type, values_from = value, values_fn = mean) %>% 
  arrange(date) %>% 
  rename(TN = 2, TP = 3, po4 = 4, nh4 = 5, no3 = 6, chl = 7) %>% 
  mutate(orgN = (TN-(nh4+no3))*1000,
         orgP = (TP-po4)*1000,
         year = year(date)) %>% 
  select(year,orgN,orgP) %>% 
  reframe(orgN = mean(orgN, na.rm=T),
          orgP = mean(orgP, na.rm=T),
            .by = year) -> org_conc_lake

lake_nutrient_flow %>% 
  mutate(year = year+1) %>% 
  full_join(org_conc_lake) -> figure_df
  
figure_df %>% 
  filter(name_f == "TN") %>% 
  ggplot(aes(input,orgN)) + 
  geom_point(size = 3, col = "darkorange") + 
  stat_poly_line(se = F, col = "black", formula = y~x + 0) +
  stat_poly_eq(use_label(c("eq", "R2")), formula = y~x + 0) +
  labs(y = bquote("Lake water concentration (µg org N l"^-1*")"),
       x = bquote("Total Nitrogen input (ton TN y"^-1*")")) + 
  tema + 
  scale_x_continuous(limits = c(0,600),
                     breaks = seq(0,600,100)) -> TN_orgN_plot

figure_df %>% 
  filter(name_f == "TP") %>% 
  ggplot(aes(input,orgP)) + 
  geom_point(size = 3, col = "royalblue") + 
  labs(y = bquote("Lake water concentration (µg org P l"^-1*")"),
       x = bquote("Total Phosphorus input (ton TP y"^-1*")")) + 
  tema + 
  stat_poly_line(data = filter(figure_df, name_f =="TP", input < 50),se = F, col = "black", formula = y~x + 0) +
  stat_poly_eq(data = filter(figure_df, name_f =="TP", input < 50), use_label(c("eq", "R2")), formula = y~x + 0) +
  scale_x_continuous(limits = c(0,100),
                     breaks = seq(0,100,25)) + 
  scale_y_continuous(limits = c(0,500))-> TP_orgP_plot

tiff("Output/Figures/in_out_org.tiff")
TP_orgP_plot / TN_orgN_plot
dev.off()

figure_df %>% 
  pivot_wider(names_from = name_f,
              values_from = input:out,
              values_fn = mean) %>% 
  select(-year1,-input_NA,-out_NA) %>% 
  write_csv("Output/In_out_org.csv")
  
figure_df %>% 
  filter(name_f =="TN") %>% 
  lm(orgN ~ input + 0, data = .) %>% summary
