source("Library.R")

plot_data %>% 
  filter(type %in% c("Phosphor, total-P","Nitrogen,total N","Ortho-phosphat-P","Ammoniak+ammonium-N","Nitrit+nitrat-N",
                     "Chlorophyl (ukorrigeret)")) %>% 
  select(date,type,value) %>% 
  pivot_wider(names_from = type, values_from = value, values_fn = mean) %>% 
  full_join(sigt) %>% 
  arrange(date) %>% 
  select(-depth,
         -type) %>% 
  rename(TN = 2, TP = 3, po4 = 4, nh4 = 5, no3 = 6, chl = 7) %>% 
  mutate(kd = 1.897/secchi,
         orgN = (TN-(nh4+no3))*1000,
         orgP = (TP-po4)*1000,
         year = as.factor(year(date)),
         year = factor(year, levels = 1973:2024),
         ) %>% 
  filter(between(month(date),5,9),
         kd < 15) -> model_df

model_df %>% 
  mutate(across(c(TN:chl,orgN:orgP), ~scale(.x))) -> model_df_scaled

# Data exploration ---

model_df %>% 
  select(kd:year,chl) %>% 
  pairs()

# Multiple linear model ----
lm(kd ~ orgP+orgN+chl, data = model_df_scaled) -> lm1

summary(lm1) 
plot(lm1)

# LME ----
library(lme4);library(lmerTest)

lmer(kd ~ orgP+orgN+chl + (1|year), data = model_df_scaled) -> lme1

summary(lme1) 
plot(lme1)
MuMIn::r.squaredGLMM(lme1)

# GAM ----
library(mgcv)

gam(kd ~  
          s(orgP) +
          s(orgN) +
          s(chl) +
          s(year, bs = "re"), 
               data = model_df) -> gam1

summary(gam1)
plot(gam1, scheme =2)

summary(model_df)

neworgP <- seq(0, 1000, len=20)
neworgN <- seq(0, 6000, len=30)
newchl <- 50
newdf <- expand.grid(orgP = neworgP, orgN = neworgN, chl = newchl, year = 2024)

matrix(predict(gam1, newdata = newdf,exclude = 's(year)'), 20,30) -> predicted_matrix

library(plotly)

plot_ly(x = neworgP,
        y = neworgN,
        z = ~predicted_matrix) %>% 
  add_surface()

newdf <- expand.grid(orgP = neworgP, orgN = neworgN, chl = seq(0,1000,20), year = 2024)

newdf %>% 
  mutate(predict = predict(gam1, newdata = newdf,exclude = 's(year)')) %>% 
  filter(between(predict,0 , 1.897)) %>% 
  ggplot(aes(orgN,orgP, fill = predict)) + 
  geom_raster() + 
  scale_fill_viridis_c()

# PSEM ----
library(piecewiseSEM)
psem(
      lmer(kd ~ orgP+orgN+chl + (1|year), data = model_df_scaled),
      lm(chl ~ orgP+orgN, data = model_df_scaled),
          data = model_df) -> psem

summary(psem)
plot(psem) -> psem_model

tiff("Output/Model/arresø_psem.tiff")
psem_model
dev.off()

