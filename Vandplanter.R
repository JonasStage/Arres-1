source("Library.R")

# Vandplante RPA ----
read_delim("Data/Vandplanter/Vandplanter RPA - Sø.csv", 
           delim = ";", escape_double = FALSE, locale = locale(decimal_mark = ",", 
                                                               grouping_mark = "."), trim_ws = TRUE) %>% 
  select(date = `Dato`, species = `Art latin`, rpa_perc = `RPA %`, rpa_m2 = `Total RPA (m2)`, total_rpa = `Total RPA (%)`, depth_limit =`Største dybdegrænse (cm)`) %>% 
  mutate(date = dmy(date)) -> rpa

# Plot rpa ----

rpa %>% 
  ggplot(aes(date)) + 
  geom_point(aes(y = total_rpa, col = "RPA"), size = 3) + 
  geom_point(aes(y = depth_limit/400, col = "Plant depth limit"), size = 3) + 
  tema + 
  labs(x = "Year",
       y = "RPA (%)",
       col = "") + 
  scale_y_continuous(limits = c(0,.4),
                     sec.axis = sec_axis(~.*4,
                                         name = "Plant depth limit (m)",
                                         breaks = seq(0,1.5,.5))) + 
  scale_color_manual(values = c("chartreuse4", "chartreuse")) -> rpa_plot

tiff("Output/Figures/RPA.tiff")
rpa_plot
dev.off()

rpa %>% 
  ggplot(aes(date,rpa_perc, col = species)) + 
  geom_point(size = 3) + 
  geom_line() +
  tema + 
  labs(x = "Year",
       y = "RPA (%)",
       col = "") 
