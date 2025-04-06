# Set working directory and load libraries ------
setwd("~/Documents/Papers/CryoAquaGHG/Data")
dir <- "~/Documents/Papers/CryoAquaGHG"
library(readxl)
library(dplyr)
library(tidyverse)
library(lme4)
library(lmerTest)
library(broom)
library(MuMIn)
library(respirometry)
library(broom.mixed)
library(gridExtra)
library(ggpubr) 
library(viridis)
library(cowplot)
library(grid)
library(gridExtra)

# Data read and preprocessing ------
## download data file "CryoLake.xlsx" from https://doi.org/10.5281/zenodo.11054180
Lsites <- read_excel(paste(dir,'Data', 'CryoLake.xlsx',sep = '/'), sheet = 'Sites', col_types = c("numeric", "numeric", "text", "text", "text", "text", "text", "numeric","text", "text", "text", "text","numeric","numeric","text","numeric","text","numeric","numeric","numeric","numeric","text","numeric","numeric","numeric","text")) 
nrow(Lsites) # 1697 rows
length(unique(Lsites$Site_ID))# 1585
Lconcs <- read_excel(paste(dir,'Data','CryoLake.xlsx',sep = '/'), sheet = 'Concentrations',  col_types = c("numeric", "numeric", "text", "text", "date", "date", "text","text", "text", "text","text","text","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","text","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","text","text","text","text"))
nrow(Lconcs) # 7260 rows
Lfluxes <- read_excel(paste(dir,'Data','CryoLake.xlsx',sep = '/'), sheet = 'Fluxes', guess_max=8000) 
nrow(Lfluxes) # 6943 rows

lconcflux <- full_join(Lconcs, Lfluxes, by=c("Source_ID", "Site_ID", "Site_Name", "Date_start", "Date_end","Month", "Season", "Year"), relationship = "many-to-many") %>% unique()
nrow(lconcflux) # 8550 rows

Lakedat <- left_join(lconcflux, Lsites, by=c("Source_ID", "Site_ID", "Site_Name"),  relationship = "many-to-many") 
nrow(Lakedat) # should be same as nrow(lconcflux)

Lakedat2 <- 
  Lakedat[,c("Site_ID",	"Site_Name",	"Conc_Name",	"Month", "Season",	"Year",	"CH4mean_umol/L",	"CO2mean_umol/L",	"N2Omean_umol/L",	"WaterTemp_degC",	"Cond_uScm",	"pH",	"DO_mgL",	"Chl_a_ug/l",	"Salinity_PSU",	"TSS_mg/l",	"NO3_umol/L",	"NH4_umol/L",	"TN_umol/L",	"SRP_umol/L",	"TP_umol/L",	"DOC_umol/L",	"POC_umol/L",	"DIC_umol/L",	"SUVA254",	"Ageof14C-CO2_yrBP",	"Ageof14C-CH4_yrBP",	"Diff_CH4_Flux_Mean_mmol m-2 d-1",	"Eb_CH4_Flux_Mean_mmol m-2 d-1",	"Storf_CH4",	"IBSf_CH4",	"Total_CH4_Flux_Mean_mmol m-2 d-1",	"Diff_CO2_Flux_Mean_mmol m-2 d-1",	"Eb_CO2_Flux_Mean_mmol m-2 d-1",	"Total_CO2_Flux_Mean_mmol m-2 d-1",	"N2O_Flux_Mean_mmol m-2 d-1",	"Lake_name", "Hylak_id", "Waterbody Type: GP=glacial/post-glacial, TKP=thermokarst pond, TKL=thermokarst lake, Pond=<0.01 km2<Lake",	"Region",	"Country",	"Cryosphere zone: G=Glacier, S=Sporadic/isol., D=Discontinuous, C=Continuous",	"Ecozone",	"Latitude",	"Longitude",	"Elevation_m", "Surface_area_km2",	"Depth_max_m",	"Depth_mean_m",	"Trophic_status",		"Icefree_days_peryr",	"MAT_degreeC",	"MAP_mm/yr")]

sapply(Lakedat2, class) # all in correct units
names(Lakedat2) <- c("Site_ID",	"Site_Name",	"Conc_Name",	"Month",	"Season",	"Year",	"CH4_uM",	"CO2_uM",	"N2O_uM",	"Tw",	"Cond_uScm",	"pH",	"DO_mgL",	"Chla_ugL",	"Salinity_PSU",	"TSS_mgL",	"NO3_uM",	"NH4_uM",	"TN_uM",	"SRP_uM",	"TP_uM",	"DOC_uM",	"POC_uM",	"DIC_uM",	"SUVA254",	"Ageof14CO2_yrBP",	"Ageof14CH4_yrBP",	"Diff_CH4_mmolm2d",	"Eb_CH4_mmolm2d",	"Storf_CH4",	"IBSf_CH4",	"Total_CH4_mmolm2d",	"Diff_CO2_mmolm2d",	"Eb_CO2_mmolm2d",	"CO2_mmolm2d",	"N2O_mmolm2d",	"Lake_name", "Hylak_id", "WaterType",	"Region", "Country",	"Cryozone",	"Ecozone",	"Latitude",	"Longitude",	"Elevation_m", "Surface_area_km2",	"Depth_max_m",	"Depth_mean_m",	"Trophic_status",	"Icefree_days_peryr",	"MAT",	"MAP")

write_csv(Lakedat2, "Lakedat2.csv")

# Linear mixed-effects (LME) modelling and visualization------
## Set area and depth bins------
k = 8.617333262e-5 # in eV/K

Lakedat2 <- read.csv("Lakedat2.csv")
Lakedat2 <- Lakedat2 %>%
  mutate(sizeclass = case_when(
    Surface_area_km2 < 0.0002 ~ "small_pond",
    Surface_area_km2 >= 0.0002 & Surface_area_km2 < 0.002 ~ "medium_pond",
    Surface_area_km2 >= 0.002 & Surface_area_km2 < 0.02 ~ "pond", # pond definition refer https://doi.org/10.1038/s41598-022-14569-0
    Surface_area_km2 >= 0.02 & Surface_area_km2 < 0.2 ~ "small_lake",
    Surface_area_km2 >= 0.2 & Surface_area_km2 < 2 ~ "medium_lake",
    Surface_area_km2 >= 2 ~ "lake",
    TRUE ~ NA_character_  # This line handles cases where Surface_area_km2 might be NA or otherwise not covered by the above conditions
  ))%>%
  mutate(depthclass = case_when(
    Depth_max_m < 1 ~ "depth_1",
    Depth_max_m >= 1 & Depth_max_m < 5 ~ "depth_1_5",
    Depth_max_m >= 5 & Depth_max_m < 15 ~ "depth_5_15", 
    Depth_max_m >= 15 ~ "depth_15",
    TRUE ~ NA_character_  # This line handles cases where Surface_area_km2 might be NA or otherwise not covered by the above conditions
  ))
mean(Lakedat2$Tw, na.rm=TRUE) # 11.67737
Lakedat2$ikt <- 1/k/(11.7+273.15) - 1/k/(Lakedat2$Tw+273.15) # Use 11.7 degreeC as centered temperature
obs_count <- table(Lakedat2$Site_ID)
obs_to_keep <- names(obs_count[obs_count >= 1])
Lakedat2_filtered <- Lakedat2[(Lakedat2$Site_ID %in% obs_to_keep), ] 

## Define the function to extract tidy results from the models
tidy_model <- function(model, name) {
  fixed_effects <- tidy(model, effects = "fixed", conf.int = TRUE, conf.level = 0.95) %>%
    mutate(model = name)
  n_obs <- glance(model)$nobs
  n_groups <- length(unique(model@flist$Site_ID))
  fixed_effects %>%
    mutate(N = n_obs,
           groups = n_groups)
}

## LME-based apparent activation energy for each size group -----
### dCH4 ----- model 1: R ~ T + (1|site); model 2: R ~ T + (ikt|site)
#### All size together -----
mlm_dch4_L_1 <- lmer(log(Diff_CH4_mmolm2d) ~ ikt + (1|Site_ID) , data = Lakedat2_filtered[which(Lakedat2_filtered$Diff_CH4_mmolm2d>0.0001),], REML = FALSE)
mlm_dch4_L_2 <- lmer(log(Diff_CH4_mmolm2d) ~ ikt + (ikt|Site_ID) , data = Lakedat2_filtered[which(Lakedat2_filtered$Diff_CH4_mmolm2d>0.0001),], REML = FALSE)
summary(mlm_dch4_L_2)
anova(mlm_dch4_L_2, mlm_dch4_L_2) # model 2 has lower  AIC
plot(mlm_dch4_L_2)
qqnorm(resid(mlm_dch4_L_2))
qqline(resid(mlm_dch4_L_2))

#### Size bins------
mlm_dch4_Lsp_1 <- lmer(log(Diff_CH4_mmolm2d) ~ ikt + (1|Site_ID) , data = Lakedat2_filtered[which(Lakedat2_filtered$Diff_CH4_mmolm2d > 0.0001 & Lakedat2_filtered$sizeclass == "small_pond"),], REML = FALSE) # not enough obs. for model 2

mlm_dch4_Lmp_1 <- lmer(log(Diff_CH4_mmolm2d) ~ ikt + (1|Site_ID) , data = Lakedat2_filtered[which(Lakedat2_filtered$Diff_CH4_mmolm2d > 0.0001 & Lakedat2_filtered$sizeclass == "medium_pond"),], REML = FALSE) # model 1

mlm_dch4_Lp_1 <- lmer(log(Diff_CH4_mmolm2d) ~ ikt + (1|Site_ID) , data = Lakedat2_filtered[which(Lakedat2_filtered$Diff_CH4_mmolm2d > 0.0001 & Lakedat2_filtered$sizeclass == "pond"),], REML = FALSE) # model 1 

mlm_dch4_Lsl_1 <- lmer(log(Diff_CH4_mmolm2d) ~ ikt + (1|Site_ID) , data = Lakedat2_filtered[which(Lakedat2_filtered$Diff_CH4_mmolm2d > 0.0001 & Lakedat2_filtered$sizeclass == "small_lake"),], REML = FALSE) # model 1

mlm_dch4_Lml_1 <- lmer(log(Diff_CH4_mmolm2d) ~ ikt + (1|Site_ID) , data = Lakedat2_filtered[which(Lakedat2_filtered$Diff_CH4_mmolm2d > 0.0001 & Lakedat2_filtered$sizeclass == "medium_lake"),], REML = FALSE) # model 1 

mlm_dch4_Ll_1 <- lmer(log(Diff_CH4_mmolm2d) ~ ikt + (1|Site_ID) , data = Lakedat2_filtered[which(Lakedat2_filtered$Diff_CH4_mmolm2d > 0.0001 & Lakedat2_filtered$sizeclass == "lake"),], REML = FALSE) # model 1

models_D <- list(mlm_dch4_L_2, mlm_dch4_Lsp_1, mlm_dch4_Lmp_1, mlm_dch4_Lp_1, mlm_dch4_Lsl_1, mlm_dch4_Lml_1, mlm_dch4_Ll_1)
model_names <- c("dch4_all_lake", "dch4_small_pond", "dch4_medium_pond", "dch4_pond", "dch4_small_lake", "dch4_medium_lake", "dch4_lake")
names(models_D) <- model_names
tidy_results_D <- bind_rows(lapply(names(models_D), function(name) tidy_model(models_D[[name]], name)))
print(tidy_results_D)
write.csv(tidy_results_D, paste(dir, 'Output', 'Tables', 'lmer_dch4_temp_lake.csv', sep = '/'))

### eCH4 -----
#### All size together -----
mlm_ech4_L_1 <- lmer(log(Eb_CH4_mmolm2d) ~ ikt + (1|Site_ID) , data = Lakedat2_filtered[which(Lakedat2_filtered$Eb_CH4_mmolm2d>0.0001),],REML = FALSE) # model 1
mlm_ech4_L_2 <- lmer(log(Eb_CH4_mmolm2d) ~ ikt + (ikt|Site_ID) , data = Lakedat2_filtered[which(Lakedat2_filtered$Eb_CH4_mmolm2d>0.0001),], REML = FALSE) # model 2
summary(mlm_ech4_L_2)
anova(mlm_ech4_L_1, mlm_ech4_L_2) # model 2 has lower  AIC
plot(mlm_ech4_L_2)
qqnorm(resid(mlm_ech4_L_2))
qqline(resid(mlm_ech4_L_2))
                                   
#### Size bins------
mlm_ech4_Lsp_1 <- lmer(log(Eb_CH4_mmolm2d) ~ ikt + (1|Site_ID) , data = Lakedat2_filtered[which(Lakedat2_filtered$Eb_CH4_mmolm2d > 0.0001 & Lakedat2_filtered$sizeclass == "small_pond"),], REML = FALSE)

mlm_ech4_Lmp_1 <- lmer(log(Eb_CH4_mmolm2d) ~ ikt + (1|Site_ID) , data = Lakedat2_filtered[which(Lakedat2_filtered$Eb_CH4_mmolm2d > 0.0001 & Lakedat2_filtered$sizeclass == "medium_pond"),], REML = FALSE)

mlm_ech4_Lp_1 <- lmer(log(Eb_CH4_mmolm2d) ~ ikt + (1|Site_ID) , data = Lakedat2_filtered[which(Lakedat2_filtered$Eb_CH4_mmolm2d > 0.0001 & Lakedat2_filtered$sizeclass == "pond"),], REML = FALSE)

mlm_ech4_Lsl_1 <- lmer(log(Eb_CH4_mmolm2d) ~ ikt + (1|Site_ID) , data = Lakedat2_filtered[which(Lakedat2_filtered$Eb_CH4_mmolm2d > 0.0001 & Lakedat2_filtered$sizeclass == "small_lake"),], REML = FALSE)

mlm_ech4_Lml_1 <- lmer(log(Eb_CH4_mmolm2d) ~ ikt + (1|Site_ID) , data = Lakedat2_filtered[which(Lakedat2_filtered$Eb_CH4_mmolm2d > 0.0001 & Lakedat2_filtered$sizeclass == "medium_lake"),], REML = FALSE)

mlm_ech4_Ll_1 <- lmer(log(Eb_CH4_mmolm2d) ~ ikt + (1|Site_ID) , data = Lakedat2_filtered[which(Lakedat2_filtered$Eb_CH4_mmolm2d > 0.0001 & Lakedat2_filtered$sizeclass == "lake"),], REML = FALSE)

models_E <- list(mlm_ech4_L_2, mlm_ech4_Lsp_1, mlm_ech4_Lmp_1, mlm_ech4_Lp_1, mlm_ech4_Lsl_1, mlm_ech4_Lml_1, mlm_ech4_Ll_1)
model_names <- c("ech4_all_lake", "ech4_small_pond", "ech4_medium_pond", "ech4_pond", "ech4_small_lake", "ech4_medium_lake", "ech4_lake")
names(models_E) <- model_names
tidy_results_E <- bind_rows(lapply(names(models_E), function(name) tidy_model(models_E[[name]], name)))
print(tidy_results_E)
write.csv(tidy_results_E, paste(dir, 'Output', 'Tables', 'lmer_ech4_temp_lake.csv', sep = '/'))

### tCH4-----
mlm_tch4_L_2 <- lmer(log(Total_CH4_mmolm2d) ~ ikt + (ikt|Site_ID) , data = Lakedat2_filtered[which(Lakedat2_filtered$Total_CH4_mmolm2d>0.0001),], REML = FALSE)
summary(mlm_tch4_L_2)
confint(mlm_tch4_L_2)


## LME-based apparent activation energy for each depth group-----
### dCH4-----

mlm_dch4_Ld1 <- lmer(log(Diff_CH4_mmolm2d) ~ ikt + (1|Site_ID) , data = Lakedat2_filtered[which(Lakedat2_filtered$Diff_CH4_mmolm2d > 0.0001 & Lakedat2_filtered$depthclass == "depth_1"),], REML = FALSE)

mlm_dch4_Ld1_5 <- lmer(log(Diff_CH4_mmolm2d) ~ ikt + (1|Site_ID) , data = Lakedat2_filtered[which(Lakedat2_filtered$Diff_CH4_mmolm2d > 0.0001 & Lakedat2_filtered$depthclass == "depth_1_5"),], REML = FALSE)

mlm_dch4_Ld5_15 <- lmer(log(Diff_CH4_mmolm2d) ~ ikt + (1|Site_ID) , data = Lakedat2_filtered[which(Lakedat2_filtered$Diff_CH4_mmolm2d > 0.0001 & Lakedat2_filtered$depthclass == "depth_5_15"),], REML = FALSE)

mlm_dch4_Ld15 <- lmer(log(Diff_CH4_mmolm2d) ~ ikt + (1|Site_ID) , data = Lakedat2_filtered[which(Lakedat2_filtered$Diff_CH4_mmolm2d > 0.0001 & Lakedat2_filtered$depthclass == "depth_15"),], REML = FALSE)

models_D_depth <- list(mlm_dch4_Ld1, mlm_dch4_Ld1_5, mlm_dch4_Ld5_15, mlm_dch4_Ld15)
model_names <- c("mlm_dch4_Ld1", "mlm_dch4_Ld1_5", "mlm_dch4_Ld5_15", "mlm_dch4_Ld15")
names(models_D_depth) <- model_names
tidy_results_D_depth <- bind_rows(lapply(names(models_D_depth), function(name) tidy_model(models_D_depth[[name]], name)))
print(tidy_results_D_depth)
write.csv(tidy_results_D_depth, "lmer_dch4_depth_lake.csv")

### eCH4-----
mlm_ech4_Ld1 <- lmer(log(Eb_CH4_mmolm2d) ~ ikt + (1|Site_ID) , data = Lakedat2_filtered[which(Lakedat2_filtered$Eb_CH4_mmolm2d > 0.0001 & Lakedat2_filtered$depthclass == "depth_1"),], REML = FALSE)
mlm_ech4_Ld1_5 <- lmer(log(Eb_CH4_mmolm2d) ~ ikt + (1|Site_ID) , data = Lakedat2_filtered[which(Lakedat2_filtered$Eb_CH4_mmolm2d > 0.0001 & Lakedat2_filtered$depthclass == "depth_1_5"),], REML = FALSE)
mlm_ech4_Ld5_15 <- lmer(log(Eb_CH4_mmolm2d) ~ ikt + (1|Site_ID) , data = Lakedat2_filtered[which(Lakedat2_filtered$Eb_CH4_mmolm2d > 0.0001 & Lakedat2_filtered$depthclass == "depth_5_15"),], REML = FALSE)
mlm_ech4_Ld15 <- lmer(log(Eb_CH4_mmolm2d) ~ ikt + (1|Site_ID) , data = Lakedat2_filtered[which(Lakedat2_filtered$Eb_CH4_mmolm2d > 0.0001 & Lakedat2_filtered$depthclass == "depth_15"),], REML = FALSE)

models_D_depth <- list(mlm_ech4_Ld1, mlm_ech4_Ld1_5, mlm_ech4_Ld5_15, mlm_ech4_Ld15)
model_names <- c("mlm_ech4_Ld1", "mlm_ech4_Ld1_5", "mlm_ech4_Ld5_15", "mlm_ech4_Ld15")
names(models_D_depth) <- model_names
tidy_results_D_depth <- bind_rows(lapply(names(models_D_depth), function(name) tidy_model(models_D_depth[[name]], name)))
print(tidy_results_D_depth)
write.csv(tidy_results_D_depth, "lmer_ech4_depth_lake.csv")

## Visualization ------
### Ea for each site with linear models-----
site_Ea_dch4_L <-  Lakedat2_filtered %>%
  filter(Diff_CH4_mmolm2d > 0.0001) %>% 
  drop_na(ikt) %>% 
  group_by(Site_ID) %>%
  do(tidy(lm(log(Diff_CH4_mmolm2d) ~ ikt, data = .))) %>%
  filter(term == "ikt") %>%
  select(Site_ID, slope = estimate, p_value=p.value)
site_Ea_dch4_L <- as.data.frame(site_Ea_dch4_L) %>% drop_na(slope)


site_Ea_ech4_L <-  Lakedat2_filtered %>%
  filter(Eb_CH4_mmolm2d > 0.0001) %>% 
  drop_na(ikt) %>% 
  group_by(Site_ID) %>%
  do(tidy(lm(log(Eb_CH4_mmolm2d) ~ ikt, data = .))) %>%
  filter(term == "ikt") %>%
  select(Site_ID, slope = estimate, p_value=p.value)
site_Ea_ech4_L <- as.data.frame(site_Ea_ech4_L) %>% drop_na(slope)

site_Ea_tch4_L <-  Lakedat2_filtered %>%
  filter(Total_CH4_mmolm2d > 0.0001) %>% 
  drop_na(ikt) %>% 
  group_by(Site_ID) %>%
  do(tidy(lm(log(Total_CH4_mmolm2d) ~ ikt, data = .))) %>%
  filter(term == "ikt") %>%
  select(Site_ID, slope = estimate, p_value=p.value)
site_Ea_tch4_L <- as.data.frame(site_Ea_tch4_L) %>% drop_na(slope)

### Figure 1 ------

L_dch4_t <- Lakedat2_filtered %>% 
  filter(Diff_CH4_mmolm2d > 0) %>% 
  drop_na(ikt) %>% 
  drop_na(sizeclass) %>% 
  ggplot(aes(ikt, (Diff_CH4_mmolm2d)))+
  geom_point(aes(color=log10(Surface_area_km2)), size = 2, alpha = .6)+
  geom_abline(intercept = 0.01055, slope = 0.30067)+
  annotate(geom = "label", x=0, y = 1800, label=expression(atop(bar(E)[dM]*' = 0.30 eV, 95% CI: 0.18-0.42', 'n = 2475, sites = 625, p < 0.001')), fill="white", label.size = NA)+
  scale_x_continuous(name= expression(paste("Standardized temperature ", bgroup("(", frac(1, kT[C]) - frac(1, kT), ")" ), " (eV"^-1,")")), limits= c(-2.5, 2.5), sec.axis = sec_axis(~ (273.15*k* . + 0.0417807)/(0.00350803 - k* . ), name =expression("Water temperature " ( degree*C)), breaks = seq(-5, 30, by=5)))+ 
  #scale_colour_brewer(palette = "Set1")+
  scale_colour_viridis()+
  labs(color = expression('log10(lake area) (k'*m^2*')')) +
  scale_y_continuous(trans= "log", breaks = c(.001, .01, .1, 1, 10, 100), labels=expression(10^-3, 10^-2, 10^-1, 10^0, 10^1, 10^2),
                     name= expression('Diffusive C'*H[4]*' flux (mmol '*m^-2*' '*d^-1*')'), limits = c(0.0001, 10000))+
  theme_linedraw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

pds_dch4 <- site_Ea_dch4_L %>%
  ggplot(aes(x=slope))+
  geom_density(color="brown", fill="#f0be89", alpha=0.6) +
  geom_vline(aes(xintercept = 0), linetype="dashed") +
  xlab("Apparent activation energy (eV)")+
  ylab("Kernel density") +
  annotate(geom = "text", x=7, y = 0.4, label=expression('Diffusive C'*H[4]), size=4) +
  scale_x_continuous(breaks = seq(from = -5, to = 10, by = 1), limits = c(-5,10)) +
  theme_linedraw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

L_ech4_t <- Lakedat2_filtered %>% 
  filter(Eb_CH4_mmolm2d > 0) %>% 
  drop_na(ikt) %>% 
  drop_na(sizeclass) %>% 
  ggplot(aes(ikt, (Eb_CH4_mmolm2d)))+
  geom_point(aes(color=log10(Surface_area_km2)), size = 2, alpha = .6)+
  geom_abline(intercept = -0.5856, slope = 1.3349)+
  annotate(geom = "label", x=0, y = 1800, label=expression(atop(bar(E)[eM]*' = 1.33 eV, 95% CI: 1.04-1.63', 'n = 2481, sites = 106, p < 0.001')), fill="white", label.size = NA)+
  scale_x_continuous(name= expression(paste("Standardized temperature ", bgroup("(", frac(1, kT[C]) - frac(1, kT), ")" ), " (eV"^-1,")")), limits= c(-2.5, 2.5), sec.axis = sec_axis(~ (273.15*k* . + 0.0417807)/(0.00350803 - k* . ), name =expression("Water temperature " ( degree*C)), breaks = seq(-5, 30, by=5)))+ 
  scale_y_continuous(trans= "log", breaks = c(.001, .01, .1, 1, 10, 100), labels=expression(10^-3, 10^-2, 10^-1, 10^0, 10^1, 10^2),
                     name= expression('Ebullitive C'*H[4]*' flux (mmol '*m^-2*' '*d^-1*')'), limits = c(0.0001, 10000))+
  scale_color_viridis() +
  labs(color = expression('log10(lake area) (k'*m^2*')')) +
  theme_linedraw() +
  theme( panel.grid.major = element_blank(), panel.grid.minor = element_blank())

pds_ech4 <- site_Ea_ech4_L %>%
  ggplot(aes(x=slope))+
  geom_density(color="darkblue", fill="#3d85c6", alpha=0.6) +
  geom_vline(aes(xintercept = 0), linetype="dashed") +
  xlab("Apparent activation energy (eV)")+
  ylab("Kernel density") +
  annotate(geom = "text", x=7, y = 0.3, label=expression('Ebullitive C'*H[4]), size=4) +
  scale_x_continuous(breaks = seq(from = -5, to = 10, by = 1), limits = c(-5,10)) +
  theme_linedraw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())


L_tch4_t <- Lakedat2_filtered %>% 
  filter(Total_CH4_mmolm2d > 0) %>% 
  drop_na(ikt) %>% 
  drop_na(sizeclass) %>% 
  ggplot(aes(ikt, (Total_CH4_mmolm2d)))+
  geom_point(aes(color=log10(Surface_area_km2)), size = 2, alpha = .6)+

  geom_abline(intercept = 0.01055, slope = 0.30067)+
  annotate(geom = "label", x=0, y = 1800, label=expression(atop(bar(E)[tM]*' = 0.88 eV, 95% CI: 0.53-1.16', 'n = 404, sites = 98, p < 0.001')), fill="white", label.size = NA)+
  scale_x_continuous(name= expression(paste("Standardized temperature ", bgroup("(", frac(1, kT[C]) - frac(1, kT), ")" ), " (eV"^-1,")")), limits= c(-2.5, 2.5), sec.axis = sec_axis(~ (273.15*k* . + 0.0417807)/(0.00350803 - k* . ), name =expression("Water temperature " ( degree*C)), breaks = seq(-5, 30, by=5)))+ 
  scale_colour_viridis()+
  labs(color = expression('log10(lake area) (k'*m^2*')')) +
  scale_y_continuous(trans= "log", breaks = c(.001, .01, .1, 1, 10, 100), labels=expression(10^-3, 10^-2, 10^-1, 10^0, 10^1, 10^2),
                     name= expression('Total C'*H[4]*' flux (mmol '*m^-2*' '*d^-1*')'), limits = c(0.0001, 10000))+
  theme_linedraw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

pds_tch4 <- site_Ea_tch4_L %>%
  ggplot(aes(x=slope))+
  geom_density(color="grey20", fill="#97A2A8", alpha=0.6) +
  geom_vline(aes(xintercept = 0), linetype="dashed") +
  xlab("Apparent activation energy (eV)")+
  ylab("Kernel density") +
  annotate(geom = "text", x=7, y = 0.35, label=expression('Total C'*H[4]), size=4) +
  scale_x_continuous(breaks = seq(from = -5, to = 10, by = 1), limits = c(-5,10)) +
  theme_linedraw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

ggsave(paste(dir, 'Output', 'Figures', 'Figure_1.pdf', sep = '/'), ggarrange(ggarrange(L_dch4_t, L_ech4_t, L_tch4_t, ncol = 3, nrow = 1, common.legend = TRUE, legend="bottom", labels = c("a", "b", "c")) , ggarrange(pds_dch4, pds_ech4, pds_tch4, ncol = 3, nrow = 1, labels = c("d", "e", "f")),  ncol=1, nrow=2, heights = c(1,0.6), align = "h"), width = 11, height = 8)

### Figure 2 ------
### define custom labels
size_label <- as_labeller(c(small_pond="Area~'<'~'0.0002'~km^2", 
                            medium_pond="'0.0002'~km^2~'\u2264'~Area~'<'~0.002~km^2", 
                            pond="0.002~km^2~'\u2264'~Area~'<'~0.02~km^2", 
                            small_lake="0.02~km^2~'\u2264'~Area~'<'~0.2~km^2", 
                            medium_lake="0.2~km^2~'\u2264'~Area~'<'~2~km^2", 
                            lake="Area~'\u2265'~2~km^2"),
                          default = label_parsed) 

### define Ea for diffusion
line_data_d <- data.frame(sizeclass = c("small_pond", "medium_pond", "pond", "small_lake", "medium_lake", "lake"),
                          slope = c(NA, NA, 0.13581524,
                                    0.409254594,
                                    0.746861133,
                                    0.960276598),
                          intercept = c(NA, NA, -0.015253247,
                                        -0.414719596,
                                        -0.405853965,
                                        -1.250027784))

text_d <- data.frame(sizeclass = c("small_pond", "medium_pond", "pond", "small_lake", "medium_lake", "lake"),
                     x = rep(0, 6),  # x position of the annotation, adjust as needed
                     y = rep(10000, 6),  # y position of the annotation, adjust as needed
                     label = c("'n = 143, sites = 86, p = 0.59'",
                               "'n = 216, sites = 91, p = 0.26'",
                               "atop(bar(E)[dM]*' = 0.14 eV, 95% CI: 0.01-0.27', 'n = 842, sites = 157, p = 0.04')",
                               "atop(bar(E)[dM]*' = 0.41 eV, 95% CI: 0.27-0.55', 'n = 513, sites = 130, p < 0.001')",
                               "atop(bar(E)[dM]*' = 0.75 eV, 95% CI: 0.52-0.97', 'n = 549, sites = 84, p < 0.001')",
                               "atop(bar(E)[dM]*' = 0.96 eV, 95% CI: 0.55-1.37', 'n = 195, sites = 63, p < 0.001')")
)


p.dch4 <- Lakedat2_filtered %>% 
  filter(Diff_CH4_mmolm2d > 0) %>% 
  drop_na(ikt) %>% 
  drop_na(sizeclass) %>% 
  ggplot()+
  facet_wrap(~factor(sizeclass, c("small_pond", "medium_pond", "pond", "small_lake", "medium_lake", "lake")), scales = "free_y", strip.position = "top", labeller = size_label, nrow=2, ncol=3, dir="h") +
  geom_point(aes(x = ikt, y = Diff_CH4_mmolm2d), color="#f0be89", size = 2, alpha = .5)+
  #geom_smooth(method= "lm", color="grey20", se= FALSE)+
  geom_abline(data = line_data_d, aes(slope = slope, intercept = intercept))+
  scale_x_continuous(name= expression(paste("Standardized temperature ", bgroup("(", frac(1, kT[C]) - frac(1, kT), ")" ))), limits= c(-2.5, 2.5), sec.axis = sec_axis(~ (273.15*k* . + 0.0417807)/(0.00350803 - k* . ), name =expression("Water temperature " ( degree*C)), breaks = seq(-5, 30, by=5)))+ 
  scale_y_continuous(trans= "log", breaks = c(.0001, .001, .01, .1, 1, 10, 100), labels=expression(10^-4, 10^-3, 10^-2, 10^-1, 10^0, 10^1, 10^2),
                     name= expression('Diffusive C'*H[4]*' flux (mmol '*m^-2*' '*d^-1*')'), limits = c(0.0001, 50000))+
  geom_label(data = text_d, aes(x = x, y = y, label = label), parse = TRUE, size=4, label.size = NA) +
  #geom_richtext(data = text_e, aes(x = x, y = y, label = label), size=3, label.color = NA) +
  theme_linedraw() +
  theme(text = element_text(size = 14), strip.background =element_rect(fill="grey90"), strip.text = element_text(color = "black"), legend.position = "none", axis.title.x=element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())
p.dch4

### define Ea for ebullition
line_data_e <- data.frame(sizeclass = c("small_pond", "medium_pond", "pond", "small_lake", "medium_lake", "lake"),
                          slope = c(0.798070468,
                                    0.941270598,
                                    2.117073871,
                                    1.804618761,
                                    1.786874279,
                                    3.085732079),
                          intercept = c(-3.219772691,
                                        -0.947514727,
                                        -0.61165249,
                                        -0.34721333,
                                        0.294292902,
                                        0.553850106))

text_e <- data.frame(sizeclass = c("small_pond", "medium_pond", "pond", "small_lake", "medium_lake", "lake"),
                     x = rep(0, 6),  # x position of the annotation, adjust as needed
                     y = rep(10000, 6),  # y position of the annotation, adjust as needed
                     label = c("atop(bar(E)[eM]*' = 0.80 eV, 95% CI: 0.50-1.10', 'n = 1050, sites = 8, p < 0.001')",
                               "atop(bar(E)[eM]*' = 0.94 eV, 95% CI: 0.48-1.40', 'n = 98, sites = 8, p < 0.001')",
                               "atop(bar(E)[eM]*' = 2.12 eV, 95% CI: 1.75-2.49', 'n = 485, sites = 32, p < 0.001')",
                               "atop(bar(E)[eM]*' = 1.80 eV, 95% CI: 1.58-2.03', 'n = 798, sites = 40, p < 0.001')",
                               "atop(bar(E)[eM]*' = 1.79 eV, 95% CI: 1.36-2.21', 'n = 21, sites = 13, p < 0.001')",
                               "atop(bar(E)[eM]*' = 3.09 eV, 95% CI: 1.58-4.59', 'n = 29, sites = 6, p < 0.001')")
)


p.ech4 <- Lakedat2_filtered %>% 
  filter(Eb_CH4_mmolm2d > 0) %>% 
  drop_na(ikt) %>% 
  ggplot()+
  facet_wrap(~factor(sizeclass, c("small_pond", "medium_pond", "pond", "small_lake", "medium_lake", "lake")), scales = "free_y", strip.position = "top", labeller = size_label, nrow=2, ncol = 3, dir="h") +
  geom_point(aes(x = ikt, y = Eb_CH4_mmolm2d), color="#3d85c6",  size = 2, alpha = .5)+
  #geom_smooth(method= "lm", color="grey20", se= FALSE)+
  geom_abline(data = line_data_e, aes(slope = slope, intercept = intercept))+
  scale_x_continuous(name= expression(paste("Standardized temperature ", bgroup("(", frac(1, kT[C]) - frac(1, kT), ")" ))), limits= c(-2.5, 2.5), sec.axis = sec_axis(~ (273.15*k* . + 0.0417807)/(0.00350803 - k* . ), name =expression("Water temperature " ( degree*C)), breaks = seq(-5, 30, by=5)))+ 
  scale_y_continuous(trans= "log", breaks = c(.0001, .001, .01, .1, 1, 10, 100), labels=expression(10^-4, 10^-3, 10^-2, 10^-1, 10^0, 10^1, 10^2),
                     name= expression('Ebullitive C'*H[4]*' flux (mmol '*m^-2*' '*d^-1*')'), limits = c(0.0001, 50000))+
  geom_label(data = text_e, aes(x = x, y = y, label = label), parse = TRUE, size=4, label.size = NA, check_overlap = FALSE) +
  #geom_richtext(data = text_e, aes(x = x, y = y, label = label), size=3, label.color = NA) +
  theme_linedraw() +
  theme(text = element_text(size = 14), strip.background =element_rect(fill="grey90"), strip.text = element_text(color = "black"), legend.position = "none", axis.title.x=element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())
p.ech4


plot <- plot_grid(p.dch4, p.ech4, ncol=1, align = 'h', labels = "auto" ) + theme(plot.margin = margin(0, 0, 0, 0, "pt"))
x.grob <- textGrob(expression(paste("Standardized temperature ", bgroup("(", frac(1, kT[C]) - frac(1, kT), ")" ), " (eV"^-1,")")), gp=gpar(fontface="bold",  fontsize=14))
top.grob <- textGrob(expression("Water temperature " ( degree*C)), gp=gpar(fontface="bold",  fontsize=14))

ggsave(paste(dir, 'Output', 'Figures', 'Figure_2.pdf', sep = '/'), grid.arrange(arrangeGrob(plot, bottom = x.grob, top = top.grob)), device = "svg", width = 9, height = 13.5)
