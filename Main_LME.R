setwd("~/Documents/Papers/CryoAquaGHG/Data")
dir <- "~/Documents/Papers/CryoAquaGHG" # Change this to your own directory
Data link: https://doi.org/10.6084/m9.figshare.29146295.v1 
# Data (updated 202504) read and preprocess ------
library(readxl)
library(dplyr)
library(tidyverse)

Lsites <- read_excel(paste(dir,'Data', 'CryoLake25.xlsx',sep = '/'), sheet = 'Sites', col_types = c("numeric", "numeric", "text", "text", "text", "text", "text", "numeric","text", "text", "text", "text","numeric","numeric","text","numeric","text","numeric","numeric","numeric","numeric","text","numeric","numeric","numeric","text")) 
nrow(Lsites) # 1735 rows
site_id_issue <- Lsites %>% group_by(Site_ID) %>% summarise(lat_count = n_distinct(Latitude), lon_count = n_distinct(Longitude), .groups = "drop") %>%filter(lat_count > 1 | lon_count > 1) %>% inner_join(Lsites, by = "Site_ID") # check if there are multiple lat/lon for the same site_id
site_id_issue # 0 lines means no issues

latlon_issue <- Lsites %>% group_by(Latitude, Longitude) %>% filter(n_distinct(Site_ID) > 1) %>% ungroup() # check if there are multiple site_id for the same lat/lon
latlon_issue # 0 lines means no issues

length(unique(Lsites$Site_ID)) # 1465
Lconcs <- read_excel(paste(dir,'Data','CryoLake25.xlsx',sep = '/'), sheet = 'Concentrations',  col_types = c("numeric", "numeric", "text", "text", "date", "date", "text","text", "text", "text","text","text","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","text","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","text","text","text","text"))
Lconcs <- Lconcs %>% group_by(across(1:9)) %>% summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE)), .groups = "drop") # average data from same day and same lake
nrow(Lconcs) # 6981 rows
Lfluxes <- read_excel(paste(dir,'Data','CryoLake25.xlsx',sep = '/'), sheet = 'Fluxes', guess_max=8000) 
Lfluxes <- Lfluxes %>% group_by(across(1:9)) %>% summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE)), .groups = "drop") # average data from same day and same lake
nrow(Lfluxes) # 6793 rows

head(Lsites)
head(Lconcs)
head(Lfluxes)
sapply(Lsites, class)
sapply(Lconcs, class)
sapply(Lfluxes, class)

lconcflux <- left_join(Lconcs, Lfluxes, by=c("Source_ID", "Site_ID", "Site_Name", "Date_start", "Date_end","Month", "Season", "Year"), relationship = "many-to-many") %>% unique()

nrow(lconcflux) # 7161 rows
#write_csv(lconcflux, "lconcflux.csv")

Lakedat <- left_join(lconcflux, Lsites, by=c("Source_ID", "Site_ID", "Site_Name"),  relationship = "many-to-many") 
nrow(Lakedat) # should be same as nrow(lconcflux)

Lakedat2 <- 
  Lakedat[,c("Site_ID",	"Site_Name",	"Conc_Name",	"Month", "Season",	"Year",	"CH4mean_umol/L",	"CO2mean_umol/L",	"N2Omean_umol/L",	"WaterTemp_degC",	"Cond_uScm",	"pH",	"DO_mgL",	"Chl_a_ug/l",	"Salinity_PSU",	"TSS_mg/l",	"NO3_umol/L",	"NH4_umol/L",	"TN_umol/L",	"SRP_umol/L",	"TP_umol/L",	"DOC_umol/L",	"POC_umol/L",	"DIC_umol/L",	"SUVA254",	"Ageof14C-CO2_yrBP",	"Ageof14C-CH4_yrBP",	"Diff_CH4_Flux_Mean_mmol m-2 d-1",	"Eb_CH4_Flux_Mean_mmol m-2 d-1",	"Storf_CH4",	"IBSf_CH4",	"Total_CH4_Flux_Mean_mmol m-2 d-1",	"Diff_CO2_Flux_Mean_mmol m-2 d-1",	"Eb_CO2_Flux_Mean_mmol m-2 d-1",	"Total_CO2_Flux_Mean_mmol m-2 d-1",	"N2O_Flux_Mean_mmol m-2 d-1",	"Lake_name", "Hylak_id", "Waterbody Type: GP=glacial/post-glacial, TKP=thermokarst pond, TKL=thermokarst lake, Pond=<0.01 km2<Lake",	"Region",	"Country",	"Cryosphere zone: G=Glacier, S=Sporadic/isol., D=Discontinuous, C=Continuous",	"Ecozone",	"Latitude",	"Longitude",	"Elevation_m", "Surface_area_km2",	"Depth_max_m",	"Depth_mean_m",	"Trophic_status",		"Icefree_days_peryr",	"MAT_degreeC",	"MAP_mm/yr")]

sapply(Lakedat2, class) # all in correct units
names(Lakedat2) <- c("Site_ID",	"Site_Name",	"Conc_Name",	"Month",	"Season",	"Year",	"CH4_uM",	"CO2_uM",	"N2O_uM",	"Tw",	"Cond_uScm",	"pH",	"DO_mgL",	"Chla_ugL",	"Salinity_PSU",	"TSS_mgL",	"NO3_uM",	"NH4_uM",	"TN_uM",	"SRP_uM",	"TP_uM",	"DOC_uM",	"POC_uM",	"DIC_uM",	"SUVA254",	"Ageof14CO2_yrBP",	"Ageof14CH4_yrBP",	"Diff_CH4_mmolm2d",	"Eb_CH4_mmolm2d",	"Storf_CH4",	"IBSf_CH4",	"Total_CH4_mmolm2d",	"Diff_CO2_mmolm2d",	"Eb_CO2_mmolm2d",	"CO2_mmolm2d",	"N2O_mmolm2d",	"Lake_name", "Hylak_id", "WaterType",	"Region", "Country",	"Cryozone",	"Ecozone",	"Latitude",	"Longitude",	"Elevation_m", "Surface_area_km2",	"Depth_max_m",	"Depth_mean_m",	"Trophic_status",	"Icefree_days_peryr",	"MAT",	"MAP")

write_csv(Lakedat2, "Lakedat2_25.csv")


# Load pre-processed data and group lake sizes------
k = 8.617333262e-5 # in eV/K

Lakedat2 <- read.csv("Lakedat2_25.csv")
Lakedat2 <- Lakedat2 %>%
  mutate(sizeclass2 = case_when(
    Surface_area_km2 < 0.0002 ~ "small_pond",
    Surface_area_km2 >= 0.0002 & Surface_area_km2 < 0.002 ~ "medium_pond",
    Surface_area_km2 >= 0.002 & Surface_area_km2 < 0.02 ~ "pond", # pond definition refer https://doi.org/10.1038/s41598-022-14569-0
    Surface_area_km2 >= 0.02 & Surface_area_km2 < 0.2 ~ "small_lake",
    Surface_area_km2 >= 0.2 & Surface_area_km2 < 2 ~ "medium_lake",
    Surface_area_km2 >= 2 ~ "lake",
    TRUE ~ NA_character_  # This line handles cases where Surface_area_km2 might be NA or otherwise not covered by the above conditions
  )) %>%
  mutate(sizeclass10 = case_when(
    Surface_area_km2 < 0.001 ~ "small_pond",
    Surface_area_km2 >= 0.001 & Surface_area_km2 < 0.01 ~ "medium_pond",
    Surface_area_km2 >= 0.01 & Surface_area_km2 < 0.1 ~ "pond", 
    Surface_area_km2 >= 0.1 & Surface_area_km2 < 1 ~ "small_lake",
    Surface_area_km2 >= 1 & Surface_area_km2 < 10 ~ "medium_lake",
    Surface_area_km2 >= 10 ~ "lake",
    TRUE ~ NA_character_
  )) %>%
  mutate(sizeclass5 = case_when(
    Surface_area_km2 < 0.0005 ~ "small_pond",
    Surface_area_km2 >= 0.0005 & Surface_area_km2 < 0.005 ~ "medium_pond",
    Surface_area_km2 >= 0.005 & Surface_area_km2 < 0.05 ~ "pond", 
    Surface_area_km2 >= 0.05 & Surface_area_km2 < 0.5 ~ "small_lake",
    Surface_area_km2 >= 0.5 & Surface_area_km2 < 5 ~ "medium_lake",
    Surface_area_km2 >= 5 ~ "lake",
    TRUE ~ NA_character_
  )) %>%
  mutate(depthclass = case_when(
    Depth_max_m < 1 ~ "depth_1",
    Depth_max_m >= 1 & Depth_max_m < 5 ~ "depth_1_5",
    Depth_max_m >= 5 & Depth_max_m < 15 ~ "depth_5_15", 
    Depth_max_m >= 15 ~ "depth_15",
    TRUE ~ NA_character_  # This line handles cases where Surface_area_km2 might be NA or otherwise not covered by the above conditions
  ))%>%
  mutate(depthclass10 = case_when(
    Depth_max_m < 1 ~ "depth_1",
    Depth_max_m >= 1 & Depth_max_m < 5 ~ "depth_1_5",
    Depth_max_m >= 5 & Depth_max_m < 10 ~ "depth_5_10", 
    Depth_max_m >= 10 ~ "depth_10",
    TRUE ~ NA_character_  # This line handles cases where Surface_area_km2 might be NA or otherwise not covered by the above conditions
  ))
mean(Lakedat2$Tw, na.rm=TRUE) # 11.67583
Lakedat2$ikt <- 1/k/(11.7+273.15) - 1/k/(Lakedat2$Tw+273.15) # Use 11.7 degreeC as centered temperature

# study sites------
LakeCH4Tsite <- Lakedat2[complete.cases(Lakedat2[, c("ikt", "Longitude", "Latitude", "Site_ID")]) & 
                           (!is.na(Lakedat2$Diff_CH4_mmolm2d) | !is.na(Lakedat2$Eb_CH4_mmolm2d)), ]
LakeCH4Tsite_unique <- LakeCH4Tsite[!duplicated(LakeCH4Tsite$Site_ID), ]
write.csv(LakeCH4Tsite_unique, "Site_with_T_and_CH4.csv")

Lake_Diff_site2504 <- Lakedat2[complete.cases(Lakedat2[, c("Longitude", "Latitude", "Site_ID")]) & 
                                 (!is.na(Lakedat2$Diff_CH4_mmolm2d)), ]
Lake_Diff_site2504 <- Lake_Diff_site2504[!duplicated(Lake_Diff_site2504$Site_ID), ]
write.csv(Lake_Diff_site2504, "Lake_Diff_site2504.csv")
Lake_Diff_site2504 %>% group_by(sizeclass10) %>% summarise(sum_area = sum(Surface_area_km2, na.rm = TRUE), .groups = "drop")

Lake_Eb_site2504 <- Lakedat2[complete.cases(Lakedat2[, c("Longitude", "Latitude", "Site_ID")]) & 
                               (!is.na(Lakedat2$Eb_CH4_mmolm2d)), ]
Lake_Eb_site2504 <- Lake_Eb_site2504[!duplicated(Lake_Eb_site2504$Site_ID), ]
write.csv(Lake_Eb_site2504, "Lake_Eb_site2504.csv")
head(Lake_Eb_site2504)
Lake_Eb_site2504 %>% group_by(sizeclass10) %>% summarise(sum_area = sum(Surface_area_km2, na.rm = TRUE), .groups = "drop")


table(Lakedat2$sizeclass10)
table((Lakedat2 %>%distinct(Site_ID, .keep_all = TRUE))$sizeclass10)
table(Lakedat2$depthclass)

Lakedat_by_sites2 <-
  Lakedat2  %>%
  group_by_at(vars(Site_ID, Site_Name, Cryozone)) %>%
  summarise(
    Lon = mean(Longitude, na.rm = TRUE), 
    Lat = mean(Latitude, na.rm = TRUE), 
    n_per_site = n(),
    CH4_uM = mean(CH4_uM, na.rm = TRUE), 
    Eb_CH4_mmolm2d = mean(Eb_CH4_mmolm2d, na.rm = TRUE),
    Diff_CH4_mmolm2d = mean(Diff_CH4_mmolm2d, na.rm = TRUE),
    Storf_CH4 = mean(Storf_CH4, na.rm = TRUE),
    IBSf_CH4 = mean(IBSf_CH4, na.rm = TRUE),
    Total_CH4_mmolm2d = mean(Total_CH4_mmolm2d, na.rm = TRUE),
    Tw = mean(Tw, na.rm = TRUE),
    Cond_uScm = mean(Cond_uScm, na.rm = TRUE), 
    pH = mean(pH, na.rm = TRUE),
    DO_mgL = mean(DO_mgL, na.rm = TRUE),	
    Chla_ugL = mean(Chla_ugL, na.rm = TRUE),	
    Salinity_PSU = mean(Salinity_PSU, na.rm = TRUE),	
    TSS_mgL = mean(TSS_mgL, na.rm = TRUE),	
    NO3_uM = mean(NO3_uM, na.rm = TRUE),	
    NH4_uM = mean(NH4_uM, na.rm = TRUE),	
    TN_uM = mean(TN_uM, na.rm = TRUE),	
    SRP_uM = mean(SRP_uM, na.rm = TRUE),	
    TP_uM = mean(TP_uM, na.rm = TRUE),	
    DOC_uM = mean(DOC_uM, na.rm = TRUE),	
    POC_uM = mean(POC_uM, na.rm = TRUE),	
    DIC_uM = mean(DIC_uM, na.rm = TRUE),	
    SUVA254 = mean(SUVA254, na.rm = TRUE),	
    Ageof14CH4_yrBP = mean(Ageof14CH4_yrBP, na.rm = TRUE),
    Elevation_m = mean(Elevation_m, na.rm = TRUE), 
    Surface_area_km2 = mean(Surface_area_km2, na.rm = TRUE),	
    Depth_max_m = mean(Depth_max_m, na.rm = TRUE),	
    Depth_mean_m = mean(Depth_mean_m, na.rm = TRUE),	
    Icefree_days_peryr = mean(Icefree_days_peryr, na.rm = TRUE),
    MAT = mean(MAT, na.rm = TRUE),
    MAP = mean(MAP, na.rm = TRUE)
  )
nrow(Lakedat_by_sites2) # 1466
#write_csv(Lakedat_by_sites2, "Lakedat_by_sites2.csv")

# Test for spatial autocorrelation------
library(sf)
library(spatialreg)
library(spdep)

unique_sites <- Lsites%>%distinct(Latitude, Longitude, .keep_all = TRUE)
lake_ch4sites_sf <- st_as_sf(unique_sites, coords = c("Longitude", "Latitude"), crs = 4326)
coords <- st_coordinates(lake_ch4sites_sf)
knn_nb <- knearneigh(coords, k = 4)        # Use 4 nearest neighbors
nb <- knn2nb(knn_nb)
listw <- nb2listw(nb, style = "W") 
moran_test <- moran.test(unique_sites$Site_ID, listw)
print(moran_test) # Moran I statistic 0.765, positive spatial autocorrelation

# Linear mixed-effects models for Ea------
library(nlme)
library(lmerTest)
library(broom)
library(MuMIn)
library(respirometry)
library(ggpubr)
library(broom.mixed)
library(sjPlot)
library(ggplot2)
library(spdep)
library(car)

## check interactions between lake area/depth and Ea------
### dCH4 -----
mlm_dch4_3 <- lme(
  fixed = log(Diff_CH4_mmolm2d) ~ ikt + ikt:(log(Surface_area_km2)) ,
  random = ~1 | Site_ID/Region,
  data = Lakedat2_clean_d%>%filter(!is.na(Surface_area_km2)),
  correlation = corGaus(1, form = ~ Longitude_j + Latitude_j, nugget = TRUE),
  control = lmeControl(maxIter = 1000, opt = "optim")
)

summary(mlm_dch4_3) # interaction term ikt:log(Surface_area_km2) signif. and positive, suggest that the Ea is stronger in larger lakes
tab_model(mlm_dch4_3)

mlm_dch4_4 <- lme(
  fixed = log(Diff_CH4_mmolm2d) ~ ikt + ikt:(log(Depth_max_m)) ,
  random = ~1 | Site_ID,
  data = Lakedat2_clean_d%>%filter(!is.na(Depth_max_m)),
  correlation = corGaus(1, form = ~ Longitude_j + Latitude_j, nugget = TRUE),
  control = lmeControl(maxIter = 1000, opt = "optim")
)
summary(mlm_dch4_4) # interaction term ikt:log(Depth_max_m) signif. and positive, suggest that the Ea is stronger in deeper lakes 
tab_model(mlm_dch4_4)

### eCH4 -----
mlm_ech4_3 <- lme(
  fixed = log(Eb_CH4_mmolm2d) ~ ikt + ikt:log(Surface_area_km2) ,
  random = ~1 | Site_ID,
  data = Lakedat2_clean_e%>%filter(!is.na(Surface_area_km2)),
  correlation = corGaus(1, form = ~ Longitude_j + Latitude_j, nugget = TRUE),
  control = lmeControl(maxIter = 1000, opt = "optim")
)

summary(mlm_ech4_3) # interaction term ikt:log(Surface_area_km2) signif. and positive, suggest that the Ea is stronger in larger lakes
tab_model(mlm_ech4_3)


mlm_ech4_4 <- lme(
  fixed = log(Eb_CH4_mmolm2d) ~ ikt + ikt:log(Depth_max_m) ,
  random = ~1 | Site_ID,
  data = Lakedat2_clean_e%>%filter(!is.na(Depth_max_m)),
  correlation = corGaus(1, form = ~ Longitude_j + Latitude_j, nugget = TRUE),
  control = lmeControl(maxIter = 1000, opt = "optim")
)
summary(mlm_ech4_4) # interaction term ikt:log(Depth_max_m) signif. and positive, suggest that the Ea is stronger in deeper lakes
tab_model(mlm_ech4_4)

## apparent activation energy for each size group -----
### dCH4 ----- 
# model 1: R ~ T + (1|site); model 2: R ~ T + (ikt|site)
Lakedat2_clean_d <- Lakedat2[complete.cases(Lakedat2[, c("Diff_CH4_mmolm2d", "ikt", "Longitude", "Latitude", "Site_ID")]) & Lakedat2$Diff_CH4_mmolm2d >= 0.001, ]
Lakedat2_clean_d$Longitude_j <- jitter(Lakedat2_clean_d$Longitude, factor = 0.0005)
Lakedat2_clean_d$Latitude_j <- jitter(Lakedat2_clean_d$Latitude, factor = 0.0005)

mlm_dch4_1 <- lme(
  fixed = log(Diff_CH4_mmolm2d) ~ ikt,
  random = ~1 | Site_ID/Region,
  data = Lakedat2_clean_d,
  correlation = corGaus(1, form = ~ Longitude_j + Latitude_j, nugget = TRUE),
  control = lmeControl(maxIter = 1000, opt = "optim")
)
summary(mlm_dch4_1) # Ea=0.32

mlm_dch4_1_2 <- lme(
  fixed = log(Diff_CH4_mmolm2d) ~ ikt,
  random = ~ikt | Site_ID/Region,
  data = Lakedat2_clean_d,
  correlation = corGaus(1, form = ~ Longitude_j + Latitude_j, nugget = TRUE),
  control = lmeControl(maxIter = 1000, opt = "optim")
)
summary(mlm_dch4_1_2) # Ea=0.30
anova(mlm_dch4_1, mlm_dch4_1_2) # model 2 has slightly lower AIC

mlm_dch4_2 <- lme(
  fixed = log(Diff_CH4_mmolm2d) ~ ikt,
  random = ~1 | Site_ID/Region/Month,
  data = Lakedat2_clean_d,
  correlation = corGaus(1, form = ~ Longitude_j + Latitude_j, nugget = TRUE),
  control = lmeControl(maxIter = 1000, opt = "optim")
)
summary(mlm_dch4_2)


qqPlot(resid(mlm_dch4_1), id=FALSE, grid=FALSE, main = "Q-Q Plot, dCH4 flux, random = ~1 | Site_ID/Region")
qqPlot(resid(mlm_dch4_2), id=FALSE, grid=FALSE, main = "Q-Q Plot, dCH4 flux, random = ~1 | Site_ID/Region/Month")

plot(mlm_dch4_1)
plot_model(mlm_dch4_1, type="diag")
qqnorm(resid(mlm_dch4_1), main = "Normal Q-Q Plot for dCH4 flux model")
qqline(resid(mlm_dch4_1),col = "red", lwd = 2)

mlm_dch4_sp <- lme(fixed = log(Diff_CH4_mmolm2d) ~ ikt, random = ~1 | Site_ID/Region, data = Lakedat2_clean_d[which(Lakedat2_clean_d$sizeclass10=="small_pond"),], correlation = corGaus(1, form = ~ Longitude_j + Latitude_j, nugget = TRUE)) # no significant fit
mlm_dch4_sp_2 <- lme(fixed = log(Diff_CH4_mmolm2d) ~ ikt, random = ~ikt | Site_ID/Region, data = Lakedat2_clean_d[which(Lakedat2_clean_d$sizeclass10=="small_pond"),], correlation = corGaus(1, form = ~ Longitude_j + Latitude_j, nugget = TRUE)) # no significant fit
anova(mlm_dch4_sp, mlm_dch4_sp_2) # lower AIC for model 2, but not significant fit

mlm_dch4_mp <- lme(fixed = log(Diff_CH4_mmolm2d) ~ ikt, random = ~1 | Site_ID/Region, data = Lakedat2_clean_d[which(Lakedat2_clean_d$sizeclass10=="medium_pond"),], correlation = corGaus(1, form = ~ Longitude_j + Latitude_j, nugget = TRUE)) # no significant fit
mlm_dch4_mp_2 <- lme(fixed = log(Diff_CH4_mmolm2d) ~ ikt, random = ~ikt | Site_ID/Region, data = Lakedat2_clean_d[which(Lakedat2_clean_d$sizeclass10=="medium_pond"),], correlation = corGaus(1, form = ~ Longitude_j + Latitude_j, nugget = TRUE)) # no significant fit
anova(mlm_dch4_mp, mlm_dch4_mp_2) # no significant difference

mlm_dch4_p <- lme(fixed = log(Diff_CH4_mmolm2d) ~ ikt, random = ~1 | Site_ID/Region, data = Lakedat2_clean_d[which(Lakedat2_clean_d$sizeclass10=="pond"),], correlation = corGaus(1, form = ~ Longitude_j + Latitude_j, nugget = TRUE))
mlm_dch4_p_2 <- lme(fixed = log(Diff_CH4_mmolm2d) ~ ikt, random = ~ikt | Site_ID/Region, data = Lakedat2_clean_d[which(Lakedat2_clean_d$sizeclass10=="pond"),], correlation = corGaus(1, form = ~ Longitude_j + Latitude_j, nugget = TRUE))
anova(mlm_dch4_p, mlm_dch4_p_2) # lower AIC for model 2
qqPlot(resid(mlm_dch4_p), id=FALSE, grid=FALSE)

mlm_dch4_sl <- lme(fixed = log(Diff_CH4_mmolm2d) ~ ikt, random = ~1 | Site_ID/Region, data = Lakedat2_clean_d[which(Lakedat2_clean_d$sizeclass10=="small_lake"),], correlation = corGaus(1, form = ~ Longitude_j + Latitude_j, nugget = TRUE))
mlm_dch4_sl_2 <- lme(fixed = log(Diff_CH4_mmolm2d) ~ ikt, random = ~ikt | Site_ID/Region, data = Lakedat2_clean_d[which(Lakedat2_clean_d$sizeclass10=="small_lake"),], correlation = corGaus(1, form = ~ Longitude_j + Latitude_j, nugget = TRUE))
anova(mlm_dch4_sl, mlm_dch4_sl_2) # lower AIC for model 2
qqPlot(resid(mlm_dch4_sl), id=FALSE, grid=FALSE)

mlm_dch4_ml <- lme(fixed = log(Diff_CH4_mmolm2d) ~ ikt, random = ~1 | Site_ID/Region, data = Lakedat2_clean_d[which(Lakedat2_clean_d$sizeclass10=="medium_lake"),], correlation = corGaus(1, form = ~ Longitude_j + Latitude_j, nugget = TRUE))
mlm_dch4_ml_2 <- lme(fixed = log(Diff_CH4_mmolm2d) ~ ikt, random = ~ikt | Site_ID/Region, data = Lakedat2_clean_d[which(Lakedat2_clean_d$sizeclass10=="medium_lake"),], correlation = corGaus(1, form = ~ Longitude_j + Latitude_j, nugget = TRUE))
anova(mlm_dch4_ml, mlm_dch4_ml_2) # no significant difference
qqPlot(resid(mlm_dch4_ml), id=FALSE, grid=FALSE)

mlm_dch4_l <- lme(fixed = log(Diff_CH4_mmolm2d) ~ ikt, random = ~1 | Site_ID/Region, data = Lakedat2_clean_d[which(Lakedat2_clean_d$sizeclass10=="lake"),], correlation = corGaus(1, form = ~ Longitude_j + Latitude_j, nugget = TRUE))
mlm_dch4_l_2 <- lme(fixed = log(Diff_CH4_mmolm2d) ~ ikt, random = ~ikt | Site_ID/Region, data = Lakedat2_clean_d[which(Lakedat2_clean_d$sizeclass10=="lake"),], correlation = corGaus(1, form = ~ Longitude_j + Latitude_j, nugget = TRUE))
anova(mlm_dch4_l, mlm_dch4_l_2) # no significant difference
qqPlot(resid(mlm_dch4_l), id=FALSE, grid=FALSE)

plot_model(mlm_dch4_l, type = "diag")
qqPlot(resid(mlm_dch4_p), id=FALSE, grid=FALSE)

tidy_results_D <- as.data.frame(t(sapply(list(dch4_all_lake  = mlm_dch4_1, dch4_small_pond  = mlm_dch4_sp, dch4_medium_pond = mlm_dch4_mp, dch4_pond = mlm_dch4_p, dch4_small_lake = mlm_dch4_sl, dch4_medium_lake = mlm_dch4_ml, dch4_lake = mlm_dch4_l), 
                                         function(m) {
                                           tt <- summary(m)$tTable["ikt", ]
                                           intercept <- summary(m)$tTable["(Intercept)", "Value"]
                                           ci <- intervals(m, which = "fixed")$fixed["ikt", c("lower", "upper")]
                                           grp_var <- names(m$groups)[1]  # Usually "Site_ID"
                                           n_grp <- length(unique(m$data[[grp_var]]))
                                           c(Ea = tt["Value"],Intercept = intercept, tt["Std.Error"], tt["DF"], tt["t-value"], tt["p-value"], ci[1], ci[2], Num_Obs = nobs(m),Num_Groups = n_grp)
                                         })))

print(tidy_results_D)
write.csv(tidy_results_D, paste(dir, 'Output', 'Tables', 'lmer_dch4_temp_lake_2504v2.csv', sep = '/'))


#### Ea slide window analysis with lake surface area------
library(nlme)
library(dplyr)

sw_results_d <- lapply(seq(0.1, 10, by = 0.1), function(thr) {
  get_metrics <- function(data) {
    n <- nrow(data)
    n_sites <- length(unique(data$Site_ID))
    if (n >= 10 && n_sites >= 2) {
      fit <- try(lme(
        log(Diff_CH4_mmolm2d) ~ ikt,
        random = ~1 | Site_ID,
        data = data,
        correlation = corGaus(1, form = ~ Longitude_j + Latitude_j, nugget = TRUE),
        control = lmeControl(maxIter = 1000, opt = "optim")
      ), silent = TRUE)
      if (!inherits(fit, "try-error")) {
        coef <- fixef(fit)["ikt"]
        se <- summary(fit)$tTable["ikt", "Std.Error"]
        pval <- summary(fit)$tTable["ikt", "p-value"]
        ci_lower <- coef - 1.96 * se
        ci_upper <- coef + 1.96 * se
        return(c(n, n_sites, coef, se, ci_lower, ci_upper, pval))
      }
    }
    return(c(n, n_sites, rep(NA, 5)))
  }
  
  data_lt <- Lakedat2_clean_d %>% filter(Surface_area_km2 < thr)
  data_ge <- Lakedat2_clean_d %>% filter(Surface_area_km2 >= thr)
  c(thr, get_metrics(data_lt), get_metrics(data_ge))
})

ea_table_diff <- as.data.frame(do.call(rbind, sw_results_d))
colnames(ea_table_diff) <- c(
  "Threshold",
  "N_Less", "Sites_Less", "Ea_Less", "SE_Less", "CI_Lower_Less", "CI_Upper_Less", "P_Less",
  "N_Greater", "Sites_Greater", "Ea_Greater", "SE_Greater", "CI_Lower_Greater", "CI_Upper_Greater", "P_Greater"
)

write.csv(ea_table_diff, paste(dir, 'Output', 'Tables', "Diff_Ea_slide_window2.csv", sep = '/'), row.names = FALSE)


### eCH4 -----
Lakedat2_clean_e <- Lakedat2[complete.cases(Lakedat2[, c("Eb_CH4_mmolm2d", "ikt", "Longitude", "Latitude", "Site_ID")]) & Lakedat2$Eb_CH4_mmolm2d >= 0.001, ]
Lakedat2_clean_e$Longitude_j <- jitter(Lakedat2_clean_e$Longitude, factor = 0.0005)
Lakedat2_clean_e$Latitude_j <- jitter(Lakedat2_clean_e$Latitude, factor = 0.0005)

mlm_ech4_1 <- lme(
  fixed = log(Eb_CH4_mmolm2d) ~ ikt ,
  random = ~1 | Site_ID/Region,
  data = Lakedat2_clean_e,
  correlation = corGaus(1, form = ~ Longitude_j + Latitude_j, nugget = TRUE),
  control = lmeControl(maxIter = 1000, opt = "optim")
)
summary(mlm_ech4_1) # Ea=1.33

mlm_ech4_1_2 <- lme(
  fixed = log(Eb_CH4_mmolm2d) ~ ikt,
  random = ~ikt | Site_ID/Region,
  data = Lakedat2_clean_e,
  correlation = corGaus(1, form = ~ Longitude_j + Latitude_j, nugget = TRUE),
  control = lmeControl(maxIter = 1000, opt = "optim")
)
summary(mlm_ech4_1_2) # Ea=1.16
anova(mlm_ech4_1, mlm_ech4_1_2) # model 2 has slightly lower AIC

mlm_ech4_2 <- lme(
  fixed = log(Eb_CH4_mmolm2d) ~ ikt ,
  random = ~1 | Site_ID/Region/Month,
  data = Lakedat2_clean_e,
  correlation = corGaus(1, form = ~ Longitude_j + Latitude_j, nugget = TRUE),
  control = lmeControl(maxIter = 1000, opt = "optim")
)
summary(mlm_ech4_2)

anova(mlm_ech4_1, mlm_ech4_2)

plot_model(mlm_ech4_1, type = "diag")
qqPlot(resid(mlm_ech4_1), id=FALSE, grid=FALSE, main = "Q-Q Plot, eCH4 flux, random = ~1 | Site_ID/Region")
qqPlot(resid(mlm_ech4_2), id=FALSE, grid=FALSE, main = "Q-Q Plot, eCH4 flux, random = ~1 | Site_ID/Region/Month")
shapiro.test(resid(mlm_ech4_1))

plot(mlm_ech4_1)
qqnorm(resid(mlm_ech4_1), main = "Normal Q-Q Plot for eCH4 flux model")
qqline(resid(mlm_ech4_1),col = "red", lwd = 2)

mlm_ech4_sp <- lme(fixed = log(Eb_CH4_mmolm2d) ~ ikt, random = ~1 | Site_ID/Region, data = Lakedat2_clean_e[which(Lakedat2_clean_e$sizeclass10=="small_pond"),], correlation = corGaus(1, form = ~ Longitude_j + Latitude_j, nugget = TRUE))
mlm_ech4_sp_2 <- lme(fixed = log(Eb_CH4_mmolm2d) ~ ikt, random = ~ikt | Site_ID/Region, data = Lakedat2_clean_e[which(Lakedat2_clean_e$sizeclass10=="small_pond"),], correlation = corGaus(1, form = ~ Longitude_j + Latitude_j, nugget = TRUE))
anova(mlm_ech4_sp, mlm_ech4_sp_2) # lower AIC for model 2, but not significant fit
qqPlot(resid(mlm_ech4_sp), id=FALSE, grid=FALSE)

mlm_ech4_mp <- lme(fixed = log(Eb_CH4_mmolm2d) ~ ikt, random = ~1 | Site_ID/Region, data = Lakedat2_clean_e[which(Lakedat2_clean_e$sizeclass10=="medium_pond"),], correlation = corGaus(1, form = ~ Longitude_j + Latitude_j, nugget = TRUE))
mlm_ech4_mp_2 <- lme(fixed = log(Eb_CH4_mmolm2d) ~ ikt, random = ~ikt | Site_ID/Region, data = Lakedat2_clean_e[which(Lakedat2_clean_e$sizeclass10=="medium_pond"),], correlation = corGaus(1, form = ~ Longitude_j + Latitude_j, nugget = TRUE)) # cannot fit model 2
qqPlot(resid(mlm_ech4_mp), id=FALSE, grid=FALSE)

mlm_ech4_p <- lme(fixed = log(Eb_CH4_mmolm2d) ~ ikt, random = ~1 | Site_ID/Region, data = Lakedat2_clean_e[which(Lakedat2_clean_e$sizeclass10=="pond"),], correlation = corGaus(1, form = ~ Longitude_j + Latitude_j, nugget = TRUE))
mlm_ech4_p_2 <- lme(fixed = log(Eb_CH4_mmolm2d) ~ ikt, random = ~ikt | Site_ID/Region, data = Lakedat2_clean_e[which(Lakedat2_clean_e$sizeclass10=="pond"),], correlation = corGaus(1, form = ~ Longitude_j + Latitude_j, nugget = TRUE))
anova(mlm_ech4_p, mlm_ech4_p_2) # lower AIC for model 2
qqPlot(resid(mlm_ech4_p), id=FALSE, grid=FALSE)

mlm_ech4_sl <- lme(fixed = log(Eb_CH4_mmolm2d) ~ ikt, random = ~1 | Site_ID/Region, data = Lakedat2_clean_e[which(Lakedat2_clean_e$sizeclass10=="small_lake"),], correlation = corGaus(1, form = ~ Longitude_j + Latitude_j, nugget = TRUE))
mlm_ech4_sl_2 <- lme(fixed = log(Eb_CH4_mmolm2d) ~ ikt, random = ~ikt | Site_ID/Region, data = Lakedat2_clean_e[which(Lakedat2_clean_e$sizeclass10=="small_lake"),], correlation = corGaus(1, form = ~ Longitude_j + Latitude_j, nugget = TRUE))
anova(mlm_ech4_sl, mlm_ech4_sl_2) # no significant difference
qqPlot(resid(mlm_ech4_sl), id=FALSE, grid=FALSE)

mlm_ech4_ml <- lme(fixed = log(Eb_CH4_mmolm2d) ~ ikt, random = ~1 | Site_ID/Region, data = Lakedat2_clean_e[which(Lakedat2_clean_e$sizeclass10=="medium_lake"),], correlation = corGaus(1, form = ~ Longitude_j + Latitude_j, nugget = TRUE))
mlm_ech4_ml_2 <- lme(fixed = log(Eb_CH4_mmolm2d) ~ ikt, random = ~ikt | Site_ID/Region, data = Lakedat2_clean_e[which(Lakedat2_clean_e$sizeclass10=="medium_lake"),], correlation = corGaus(1, form = ~ Longitude_j + Latitude_j, nugget = TRUE)) # cannot fit model 2
plot(mlm_ech4_ml)
qqnorm(resid(mlm_ech4_ml), main = "Normal Q-Q Plot for dCH4 flux model")
qqline(resid(mlm_ech4_ml),col = "red", lwd = 2)
qqPlot(resid(mlm_ech4_ml), id=FALSE, grid=FALSE)

mlm_ech4_l <- lme(fixed = log(Eb_CH4_mmolm2d) ~ ikt, random = ~1 | Site_ID/Region, data = Lakedat2_clean_e[which(Lakedat2_clean_e$sizeclass10=="lake"),], correlation = corGaus(1, form = ~ Longitude_j + Latitude_j, nugget = TRUE)) # no significant fit
mlm_ech4_l_2 <- lme(fixed = log(Eb_CH4_mmolm2d) ~ ikt, random = ~ikt | Site_ID/Region, data = Lakedat2_clean_e[which(Lakedat2_clean_e$sizeclass10=="lake"),], correlation = corGaus(1, form = ~ Longitude_j + Latitude_j, nugget = TRUE)) # no significant fit
anova(mlm_ech4_l, mlm_ech4_l_2) # no significant difference

plot_model(mlm_ech4_l, type = "diag")
qqPlot(resid(mlm_ech4_l), id=FALSE, grid=FALSE)

tidy_results_E <- as.data.frame(t(sapply(list(ech4_all_lake  = mlm_ech4_1, ech4_small_pond  = mlm_ech4_sp, ech4_medium_pond = mlm_ech4_mp, ech4_pond = mlm_ech4_p, ech4_small_lake = mlm_ech4_sl, ech4_medium_lake = mlm_ech4_ml, ech4_lake = mlm_ech4_l), 
                                         function(m) {
                                           tt <- summary(m)$tTable["ikt", ]
                                           intercept <- summary(m)$tTable["(Intercept)", "Value"]
                                           ci <- intervals(m, which = "fixed")$fixed["ikt", c("lower", "upper")]
                                           grp_var <- names(m$groups)[1]  # Usually "Site_ID"
                                           n_grp <- length(unique(m$data[[grp_var]]))
                                           c(Ea = tt["Value"],Intercept = intercept, tt["Std.Error"], tt["DF"], tt["t-value"], tt["p-value"], ci[1], ci[2], Num_Obs = nobs(m),Num_Groups = n_grp)
                                         })))

print(tidy_results_E)
write.csv(tidy_results_E, paste(dir, 'Output', 'Tables', 'lmer_ech4_temp_lake_2504v2.csv', sep = '/'))

#### Ea slide window analysis with lake surface area------
sw_results_e <- lapply(seq(0.1, 10, by = 0.1), function(thr) {
  get_metrics <- function(data) {
    n <- nrow(data)
    n_sites <- length(unique(data$Site_ID))
    if (n >= 10 && n_sites >= 2) {
      fit <- try(lme(
        log(Eb_CH4_mmolm2d) ~ ikt,
        random = ~1 | Site_ID,
        data = data,
        correlation = corGaus(1, form = ~ Longitude_j + Latitude_j, nugget = TRUE),
        control = lmeControl(maxIter = 1000, opt = "optim")
      ), silent = TRUE)
      if (!inherits(fit, "try-error")) {
        coef <- fixef(fit)["ikt"]
        se <- summary(fit)$tTable["ikt", "Std.Error"]
        pval <- summary(fit)$tTable["ikt", "p-value"]
        ci_lower <- coef - 1.96 * se
        ci_upper <- coef + 1.96 * se
        return(c(n, n_sites, coef, se, ci_lower, ci_upper, pval))
      }
    }
    return(c(n, n_sites, rep(NA, 5)))
  }
  
  data_lt <- Lakedat2_clean_e %>% filter(Surface_area_km2 < thr)
  data_ge <- Lakedat2_clean_e %>% filter(Surface_area_km2 >= thr)
  c(thr, get_metrics(data_lt), get_metrics(data_ge))
})

ea_table_eb <- as.data.frame(do.call(rbind, sw_results_e))
colnames(ea_table_eb) <- c(
  "Threshold",
  "N_Less", "Sites_Less", "Ea_Less", "SE_Less", "CI_Lower_Less", "CI_Upper_Less", "P_Less",
  "N_Greater", "Sites_Greater", "Ea_Greater", "SE_Greater", "CI_Lower_Greater", "CI_Upper_Greater", "P_Greater"
)

write.csv(ea_table_eb, paste(dir, 'Output', 'Tables', "Eb_Ea_slide_window2.csv", sep = '/'), row.names = FALSE)

#### area slide window Ea plots------
ea_table_diff <- read.csv(paste(dir, 'Output', 'Tables', "Diff_Ea_slide_window2.csv", sep = '/'))
ea_table_eb <- read.csv(paste(dir, 'Output', 'Tables', "Eb_Ea_slide_window2.csv", sep = '/'))

ea_table_diff_filter <- ea_table_diff %>%
  filter(P_Less <= 0.05 | P_Greater <= 0.05) %>%
  select(Threshold,
         Ea_Less, SE_Less, P_Less,
         Ea_Greater, SE_Greater, P_Greater) %>%
  pivot_longer(
    cols = -Threshold,
    names_to = c("Metric", "Group"),
    names_pattern = "(.*)_(Less|Greater)"
  ) %>%
  pivot_wider(names_from = Metric, values_from = value) %>%
  filter(P <= 0.05) %>%
  mutate(Group = ifelse(Group == "Less", "< Threshold", "≥ Threshold"))

ea_table_diff_fig <- ggplot(ea_table_diff_filter, aes(x = Threshold, y = Ea, color = Group)) +
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = Ea - SE, ymax = Ea + SE), width = 0.03, alpha=0.5) +
  geom_smooth(method = "lm", se = TRUE) +
  scale_color_manual(values = c("< Threshold" = "#f0be89", "≥ Threshold" = "#6c3e0e")) +
  geom_rug(sides = "b", alpha = 0.5, length = unit(0.01, "npc"), color="black") + 
  labs(
    title = expression("Sliding Window"~bar(E)[dM]*' Estimates for Diffusion'),
    x = "Surface Area Threshold (km²)",
    y = expression(bar(E)[dM]*' (eV)'),
    color = "Group"
  ) +
  scale_x_continuous(trans = "log10", breaks=c(0.1,0.5,1,5,10), labels=expression(0.1,0.5,1,5,10))+
  theme_linedraw(base_size = 13) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = c(0.82,0.4))

ea_table_eb_filter <- ea_table_eb %>%
  filter(P_Less <= 0.05 | P_Greater <= 0.05) %>%
  select(Threshold,
         Ea_Less, SE_Less, P_Less,
         Ea_Greater, SE_Greater, P_Greater) %>%
  pivot_longer(
    cols = -Threshold,
    names_to = c("Metric", "Group"),
    names_pattern = "(.*)_(Less|Greater)"
  ) %>%
  pivot_wider(names_from = Metric, values_from = value) %>%
  filter(P <= 0.05) %>%
  mutate(Group = ifelse(Group == "Less", "< Threshold", "≥ Threshold"))

ea_table_eb_fig <- ggplot(ea_table_eb_filter, aes(x = Threshold, y = Ea, color = Group)) +
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = Ea - SE, ymax = Ea + SE), width = 0.03, alpha=0.5) +
  geom_smooth(method = "lm", se = TRUE) +
  scale_color_manual(values = c("< Threshold" = "#98bee1", "≥ Threshold" = "#1c3f5e")) +
  geom_rug(sides = "b", alpha = 0.5, length = unit(0.01, "npc"), color="black") + 
  labs(
    title = expression("Sliding Window"~bar(E)[eM]*' Estimates for Ebullition'),
    x = "Surface Area Threshold (km²)",
    y = expression(bar(E)[eM]*' (eV)'),
    color = "Group"
  ) +
  scale_x_continuous(trans = "log10", breaks=c(0.1,0.5,1,5,10), labels=expression(0.1,0.5,1,5,10))+
  theme_linedraw(base_size = 13) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = c(0.82,0.4))

ggsave(paste(dir, 'Output', 'Figures', 'Ea slide window2.pdf', sep = '/'), ggarrange(ea_table_diff_fig,  ea_table_eb_fig, ncol = 1, nrow = 2,  labels = c("a", "b")), width = 5.5, height = 10)


### tCH4-----
Lakedat2_clean_t <- Lakedat2[complete.cases(Lakedat2[, c("Total_CH4_mmolm2d", "ikt", "Longitude", "Latitude", "Site_ID")]) & Lakedat2$Total_CH4_mmolm2d >= 0.001, ]
Lakedat2_clean_t$Longitude_j <- jitter(Lakedat2_clean_t$Longitude, factor = 0.0005)
Lakedat2_clean_t$Latitude_j <- jitter(Lakedat2_clean_t$Latitude, factor = 0.0005)

mlm_tch4_1 <- lme(
  fixed = log(Total_CH4_mmolm2d) ~ ikt ,
  random = ~1 | Site_ID/Region,
  data = Lakedat2_clean_t,
  correlation = corGaus(1, form = ~ Longitude_j + Latitude_j, nugget = TRUE),
  control = lmeControl(maxIter = 1000, opt = "optim")
)
summary(mlm_tch4_1)
tab_model(mlm_tch4_1)
intervals(mlm_tch4_1,which="fixed")

## apparent activation energy for each depth group-----
### dCH4-----

mlm_dch4_Ld1 <-  lme(fixed = log(Diff_CH4_mmolm2d) ~ ikt, random = ~1 | Site_ID/Region, data = Lakedat2_clean_d[which(Lakedat2_clean_d$depthclass=="depth_1"),], correlation = corGaus(1, form = ~ Longitude_j + Latitude_j, nugget = TRUE))

mlm_dch4_Ld1_5 <- lme(fixed = log(Diff_CH4_mmolm2d) ~ ikt, random = ~1 | Site_ID/Region, data = Lakedat2_clean_d[which(Lakedat2_clean_d$depthclass=="depth_1_5"),], correlation = corGaus(1, form = ~ Longitude_j + Latitude_j, nugget = TRUE))

mlm_dch4_Ld5_15 <- lme(fixed = log(Diff_CH4_mmolm2d) ~ ikt, random = ~1 | Site_ID/Region, data = Lakedat2_clean_d[which(Lakedat2_clean_d$depthclass=="depth_5_15"),], correlation = corGaus(1, form = ~ Longitude_j + Latitude_j, nugget = TRUE))

mlm_dch4_Ld15 <- lme(fixed = log(Diff_CH4_mmolm2d) ~ ikt, random = ~1 | Site_ID/Region, data = Lakedat2_clean_d[which(Lakedat2_clean_d$depthclass=="depth_15"),], correlation = corGaus(1, form = ~ Longitude_j + Latitude_j, nugget = TRUE))

plot_model(mlm_dch4_Ld15, type = "diag")

tidy_results_D_depth <- as.data.frame(t(sapply(list(mlm_dch4_Ld1  = mlm_dch4_Ld1, mlm_dch4_Ld1_5 = mlm_dch4_Ld1_5, mlm_dch4_Ld5_15 = mlm_dch4_Ld5_15, mlm_dch4_Ld15 = mlm_dch4_Ld15), 
                                               function(m) {
                                                 tt <- summary(m)$tTable["ikt", ]
                                                 intercept <- summary(m)$tTable["(Intercept)", "Value"]
                                                 ci <- intervals(m, which = "fixed")$fixed["ikt", c("lower", "upper")]
                                                 grp_var <- names(m$groups)[1]  # Usually "Site_ID"
                                                 n_grp <- length(unique(m$data[[grp_var]]))
                                                 c(Ea = tt["Value"],Intercept = intercept, tt["Std.Error"], tt["DF"], tt["t-value"], tt["p-value"], ci[1], ci[2], Num_Obs = nobs(m),Num_Groups = n_grp)
                                               })))

print(tidy_results_D_depth)
write.csv(tidy_results_D_depth, paste(dir, 'Output', 'Tables', 'lmer_dch4_temp_lake_bydepth2.csv', sep = '/'))

#### Ea slide window analysis with lake max depth------
sw_results_dd <- lapply(seq(1, 15, by = 1), function(thr) {
  get_metrics <- function(data) {
    n <- nrow(data)
    n_sites <- length(unique(data$Site_ID))
    if (n >= 10 && n_sites >= 2) {
      fit <- try(lme(
        log(Diff_CH4_mmolm2d) ~ ikt,
        random = ~1 | Site_ID,
        data = data,
        correlation = corGaus(1, form = ~ Longitude_j + Latitude_j, nugget = TRUE),
        control = lmeControl(maxIter = 1000, opt = "optim")
      ), silent = TRUE)
      if (!inherits(fit, "try-error")) {
        coef <- fixef(fit)["ikt"]
        se <- summary(fit)$tTable["ikt", "Std.Error"]
        pval <- summary(fit)$tTable["ikt", "p-value"]
        ci_lower <- coef - 1.96 * se
        ci_upper <- coef + 1.96 * se
        return(c(n, n_sites, coef, se, ci_lower, ci_upper, pval))
      }
    }
    return(c(n, n_sites, rep(NA, 5)))
  }
  
  data_lt <- Lakedat2_clean_d %>% filter(Depth_max_m < thr)
  data_ge <- Lakedat2_clean_d %>% filter(Depth_max_m >= thr)
  c(thr, get_metrics(data_lt), get_metrics(data_ge))
})

ea_table_diff2 <- as.data.frame(do.call(rbind, sw_results_dd))
colnames(ea_table_diff2) <- c(
  "Threshold",
  "N_Less", "Sites_Less", "Ea_Less", "SE_Less", "CI_Lower_Less", "CI_Upper_Less", "P_Less",
  "N_Greater", "Sites_Greater", "Ea_Greater", "SE_Greater", "CI_Lower_Greater", "CI_Upper_Greater", "P_Greater"
)
ea_table_diff2
write.csv(ea_table_diff2, paste(dir, 'Output', 'Tables', "Diff_Ea_slide_window2depth.csv", sep = '/'), row.names = FALSE)

### eCH4-----


mlm_ech4_Ld1 <-  lme(fixed = log(Eb_CH4_mmolm2d) ~ ikt, random = ~1 | Site_ID/Region, data = Lakedat2_clean_e[which(Lakedat2_clean_e$depthclass=="depth_1"),], correlation = corGaus(1, form = ~ Longitude_j + Latitude_j, nugget = TRUE))

mlm_ech4_Ld1_5 <- lme(fixed = log(Eb_CH4_mmolm2d) ~ ikt, random = ~1 | Site_ID/Region, data = Lakedat2_clean_e[which(Lakedat2_clean_e$depthclass=="depth_1_5"),], correlation = corGaus(1, form = ~ Longitude_j + Latitude_j, nugget = TRUE))

mlm_ech4_Ld5_15 <- lme(fixed = log(Eb_CH4_mmolm2d) ~ ikt, random = ~1 | Site_ID/Region, data = Lakedat2_clean_e[which(Lakedat2_clean_e$depthclass=="depth_5_15"),], correlation = corGaus(1, form = ~ Longitude_j + Latitude_j, nugget = TRUE))

mlm_ech4_Ld15 <- lme(fixed = log(Eb_CH4_mmolm2d) ~ ikt, random = ~1 | Site_ID/Region, data = Lakedat2_clean_e[which(Lakedat2_clean_e$depthclass=="depth_15"),], correlation = corGaus(1, form = ~ Longitude_j + Latitude_j, nugget = TRUE))

plot_model(mlm_ech4_Ld15, type = "diag")


tidy_results_E_depth <- as.data.frame(t(sapply(list(mlm_ech4_Ld1  = mlm_ech4_Ld1, mlm_ech4_Ld1_5 = mlm_ech4_Ld1_5, mlm_ech4_Ld5_15 = mlm_ech4_Ld5_15, mlm_ech4_Ld15 = mlm_ech4_Ld15), 
                                               function(m) {
                                                 tt <- summary(m)$tTable["ikt", ]
                                                 intercept <- summary(m)$tTable["(Intercept)", "Value"]
                                                 ci <- intervals(m, which = "fixed")$fixed["ikt", c("lower", "upper")]
                                                 grp_var <- names(m$groups)[1]  # Usually "Site_ID"
                                                 n_grp <- length(unique(m$data[[grp_var]]))
                                                 c(Ea = tt["Value"],Intercept = intercept, tt["Std.Error"], tt["DF"], tt["t-value"], tt["p-value"], ci[1], ci[2], Num_Obs = nobs(m),Num_Groups = n_grp)
                                               })))

print(tidy_results_E_depth)
write.csv(tidy_results_E_depth, paste(dir, 'Output', 'Tables', 'lmer_ech4_temp_lake_bydepth2.csv', sep = '/'))



#### Ea slide window analysis with lake max depth------
sw_results_ed <- lapply(seq(1, 15, by = 1), function(thr) {
  get_metrics <- function(data) {
    n <- nrow(data)
    n_sites <- length(unique(data$Site_ID))
    if (n >= 10 && n_sites >= 2) {
      fit <- try(lme(
        log(Eb_CH4_mmolm2d) ~ ikt,
        random = ~1 | Site_ID,
        data = data,
        correlation = corGaus(1, form = ~ Longitude_j + Latitude_j, nugget = TRUE),
        control = lmeControl(maxIter = 1000, opt = "optim")
      ), silent = TRUE)
      if (!inherits(fit, "try-error")) {
        coef <- fixef(fit)["ikt"]
        se <- summary(fit)$tTable["ikt", "Std.Error"]
        pval <- summary(fit)$tTable["ikt", "p-value"]
        ci_lower <- coef - 1.96 * se
        ci_upper <- coef + 1.96 * se
        return(c(n, n_sites, coef, se, ci_lower, ci_upper, pval))
      }
    }
    return(c(n, n_sites, rep(NA, 5)))
  }
  
  data_lt <- Lakedat2_clean_e %>% filter(Depth_max_m < thr)
  data_ge <- Lakedat2_clean_e %>% filter(Depth_max_m >= thr)
  c(thr, get_metrics(data_lt), get_metrics(data_ge))
})

ea_table_eb2 <- as.data.frame(do.call(rbind, sw_results_ed))
colnames(ea_table_eb2) <- c(
  "Threshold",
  "N_Less", "Sites_Less", "Ea_Less", "SE_Less", "CI_Lower_Less", "CI_Upper_Less", "P_Less",
  "N_Greater", "Sites_Greater", "Ea_Greater", "SE_Greater", "CI_Lower_Greater", "CI_Upper_Greater", "P_Greater"
)
ea_table_eb2
write.csv(ea_table_eb2, paste(dir, 'Output', 'Tables', "Eb_Ea_slide_window2depth.csv", sep = '/'), row.names = FALSE)


#### depth slide window Ea plots------
ea_table_diff2 <- read.csv(paste(dir, 'Output', 'Tables', "Diff_Ea_slide_window2depth.csv", sep = '/'))
ea_table_eb2 <- read.csv(paste(dir, 'Output', 'Tables', "Eb_Ea_slide_window2depth.csv", sep = '/'))

ea_table_diff_filter2 <- ea_table_diff2 %>%
  filter(P_Less <= 0.05 | P_Greater <= 0.05) %>%
  select(Threshold,
         Ea_Less, SE_Less, P_Less,
         Ea_Greater, SE_Greater, P_Greater) %>%
  pivot_longer(
    cols = -Threshold,
    names_to = c("Metric", "Group"),
    names_pattern = "(.*)_(Less|Greater)"
  ) %>%
  pivot_wider(names_from = Metric, values_from = value) %>%
  filter(P <= 0.05) %>%
  mutate(Group = ifelse(Group == "Less", "< Threshold", "≥ Threshold"))

ea_table_diff_fig2 <- ggplot(ea_table_diff_filter2, aes(x = Threshold, y = Ea, color = Group)) +
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = Ea - SE, ymax = Ea + SE), width = 0.03, alpha=0.5) +
  geom_smooth(method = "lm", se = TRUE) +
  scale_color_manual(values = c("< Threshold" = "#f0be89", "≥ Threshold" = "#6c3e0e")) +
  geom_rug(sides = "b", alpha = 0.5, length = unit(0.01, "npc"), color="black") + 
  labs(
    title = expression("Sliding Window"~bar(E)[dM]*' Estimates for Diffusion'),
    x = "Maximum Depth Threshold (m)",
    y = expression(bar(E)[dM]*' (eV)'),
    color = "Group"
  ) +
  theme_linedraw(base_size = 13) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = c(0.17,0.85))

ea_table_eb_filter2 <- ea_table_eb2 %>%
  filter(P_Less <= 0.05 | P_Greater <= 0.05) %>%
  select(Threshold,
         Ea_Less, SE_Less, P_Less,
         Ea_Greater, SE_Greater, P_Greater) %>%
  pivot_longer(
    cols = -Threshold,
    names_to = c("Metric", "Group"),
    names_pattern = "(.*)_(Less|Greater)"
  ) %>%
  pivot_wider(names_from = Metric, values_from = value) %>%
  filter(P <= 0.05) %>%
  mutate(Group = ifelse(Group == "Less", "< Threshold", "≥ Threshold"))

ea_table_eb_fig2 <- ggplot(ea_table_eb_filter2, aes(x = Threshold, y = Ea, color = Group)) +
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = Ea - SE, ymax = Ea + SE), width = 0.03, alpha=0.5) +
  geom_smooth(method = "lm", se = TRUE) +
  scale_color_manual(values = c("< Threshold" = "#98bee1", "≥ Threshold" = "#1c3f5e")) +
  geom_rug(sides = "b", alpha = 0.5, length = unit(0.01, "npc"), color="black") + 
  labs(
    title = expression("Sliding Window"~bar(E)[eM]*' Estimates for Ebullition'),
    x = "Maximum Depth Threshold (m)",
    y = expression(bar(E)[eM]*' (eV)'),
    color = "Group"
  ) +
  theme_linedraw(base_size = 13) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = c(0.17,0.85))

ggsave(paste(dir, 'Output', 'Figures', 'Ea slide window depth2.pdf', sep = '/'), ggarrange(ea_table_diff_fig2,  ea_table_eb_fig2, ncol = 1, nrow = 2,  labels = c("a", "b")), width = 5.5, height = 10)

ggsave(paste(dir, 'Output', 'Figures', 'Ea slide window area depth.pdf', sep = '/'), ggarrange(ea_table_diff_fig, ea_table_diff_fig2, ea_table_eb_fig, ea_table_eb_fig2, ncol = 2, nrow = 2,  labels = c("a", "b", "c", "d"), font.label = list(size = 20, color = "black")), width = 10, height = 10)

## Ea for each site with linear models-----
site_Ea_dch4_L <-  Lakedat2_clean_d %>%
  drop_na(ikt) %>% 
  group_by(Site_ID) %>%
  do(tidy(lm(log(Diff_CH4_mmolm2d) ~ ikt, data = .))) %>%
  filter(term == "ikt") %>%
  select(Site_ID, slope = estimate, p_value=p.value)
site_Ea_dch4_L <- as.data.frame(site_Ea_dch4_L) %>% drop_na(slope)%>% drop_na(p_value)
plot(density(site_Ea_dch4_L$slope))

site_Ea_ech4_L <-  Lakedat2_clean_e %>%
  drop_na(ikt) %>% 
  group_by(Site_ID) %>%
  do(tidy(lm(log(Eb_CH4_mmolm2d) ~ ikt, data = .))) %>%
  filter(term == "ikt") %>%
  select(Site_ID, slope = estimate, p_value=p.value)
site_Ea_ech4_L <- as.data.frame(site_Ea_ech4_L) %>% drop_na(slope) %>% drop_na(p_value)
plot(density(site_Ea_ech4_L$slope))

# Figure 1: Overall Ea plot ------

library(gridExtra)
library(ggpubr) 
library(viridis)
show_col(pal_hue()(9))

#size_range <- range(log10(Lakedat2_filtered[which(Lakedat2_filtered$Surface_area_km2>0),]$Surface_area_km2))
#color_scale <- scale_color_viridis(limits = size_range)

L_dch4_t <- Lakedat2_clean_d %>% 
  drop_na(ikt) %>% 
  drop_na(sizeclass10) %>% 
  ggplot(aes(ikt, (Diff_CH4_mmolm2d)))+
  geom_point(aes(color=log10(Surface_area_km2)), size = 2, alpha = .6)+
  geom_abline(intercept = -0.026624701, slope = 0.318849419)+
  annotate(geom = "label", x=0, y = 1800, label=expression(atop(bar(E)[dM]*' = 0.32 eV, 95% CI: 0.24 – 0.40', 'n = 2468, sites = 627, p < 0.001')), fill="white", label.size = NA)+
  scale_x_continuous(name= NULL, limits= c(-2.5, 2.5), sec.axis = sec_axis(~ (273.15*k* . + 0.0417807)/(0.00350803 - k* . ), name =expression("Water temperature " ( degree*C)), breaks = seq(-5, 30, by=5)))+ 
  scale_colour_viridis()+
  labs(color = expression('log10(lake area) (k'*m^2*')')) +
  scale_y_continuous(trans= "log", breaks = c(.001, .01, .1, 1, 10, 100), labels=expression(10^-3, 10^-2, 10^-1, 10^0, 10^1, 10^2),
                     name= expression('Diffusive C'*H[4]*' flux (mmol '*m^-2*' '*d^-1*')'), limits = c(0.0001, 10000))+
  theme_linedraw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

pds_dch4_2 <- site_Ea_dch4_L %>%
  ggplot(aes(y=slope))+
  geom_density(color="brown", fill="#f0be89", alpha=0.6) +
  geom_hline(aes(yintercept = 0), linetype="dashed") +
  ylab("Apparent activation energy (eV)")+
  xlab("Kernel density") +
  annotate(geom = "text", x=0.35, y = 7, label=expression('Diffusive C'*H[4]), size=4) +
  scale_y_continuous(breaks = seq(from = -5, to = 10, by = 1), limits = c(-5,10)) +
  theme_linedraw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), plot.margin = unit(c(1.3, 0.1, 0.3, 0.1), "cm"))


L_ech4_t <- Lakedat2_clean_e %>% 
  filter(Eb_CH4_mmolm2d > 0) %>% 
  drop_na(ikt) %>% 
  drop_na(sizeclass) %>% 
  ggplot(aes(ikt, (Eb_CH4_mmolm2d)))+
  geom_point(aes(color=log10(Surface_area_km2)), size = 2, alpha = .6)+
  geom_abline(intercept = -0.32704221, slope = 1.326629014)+
  annotate(geom = "label", x=0, y = 2000, label=expression(atop(bar(E)[eM]*' = 1.33 eV, 95% CI: 1.18 – 1.48', 'n = 2355, sites = 105, p < 0.001')), fill="white", label.size = NA)+
  scale_x_continuous(name= NULL, limits= c(-2.5, 2.5), sec.axis = sec_axis(~ (273.15*k* . + 0.0417807)/(0.00350803 - k* . ), name =NULL, breaks = seq(-5, 30, by=5)))+ 
  scale_y_continuous(trans= "log", breaks = c(.001, .01, .1, 1, 10, 100), labels=expression(10^-3, 10^-2, 10^-1, 10^0, 10^1, 10^2),
                     name= expression('Ebullitive C'*H[4]*' flux (mmol '*m^-2*' '*d^-1*')'), limits = c(0.0001, 10000))+
  scale_color_viridis() +
  labs(color = expression('log10(lake area) (k'*m^2*')')) +
  theme_linedraw() +
  theme( panel.grid.major = element_blank(), panel.grid.minor = element_blank())

pds_ech4_2 <-site_Ea_ech4_L %>%
  ggplot(aes(y=slope))+
  geom_density(color="darkblue", fill="#3d85c6", alpha=0.6) +
  geom_hline(aes(yintercept = 0), linetype="dashed") +
  ylab("Apparent activation energy (eV)")+
  xlab("Kernel density") +
  annotate(geom = "text", x=0.18, y = 7, label=expression('Ebullitive C'*H[4]), size=4) +
  scale_y_continuous(breaks = seq(from = -5, to = 10, by = 1), limits = c(-5,10)) +
  theme_linedraw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), plot.margin = unit(c(0.1, 0.1, 1.3, 0.1), "cm"))


L_tch4_t <- Lakedat2_clean_t %>% 
  filter(Total_CH4_mmolm2d > 0) %>% 
  drop_na(ikt) %>% 
  drop_na(sizeclass) %>% 
  ggplot(aes(ikt, (Total_CH4_mmolm2d)))+
  geom_point(aes(color=log10(Surface_area_km2)), size = 2, alpha = .6)+
  #geom_point(aes(color=sizeclass), size = 2, alpha = .6)+
  #geom_mark_ellipse(aes(color = as.factor(sizeclass)), expand = unit(0.5,"mm"))+
  #geom_smooth(method= "lm", aes(color=Site_Name), se= FALSE, linewidth = 0.5)+
  geom_abline(intercept = 0.1373411, slope = 1.0942553)+
  #geom_smooth(method= "lm", color="grey20", se= FALSE)+
  annotate(geom = "label", x=0, y = 2000, label=expression(atop(bar(E)[tM]*' = 1.09 eV, 95% CI: 0.87 – 1.32', 'n = 425, sites = 110, p < 0.001')), fill="white", label.size = NA)+
  scale_x_continuous(name= expression(paste("Standardized temperature ", bgroup("(", frac(1, kT[C]) - frac(1, kT), ")" ), " (eV"^-1,")")), limits= c(-2.5, 2.5), sec.axis = sec_axis(~ (273.15*k* . + 0.0417807)/(0.00350803 - k* . ), name =NULL, breaks = seq(-5, 30, by=5)))+ 
  #scale_colour_brewer(palette = "Set1")+
  scale_colour_viridis()+
  labs(color = expression('log10(lake area) (k'*m^2*')')) +
  scale_y_continuous(trans= "log", breaks = c(.001, .01, .1, 1, 10, 100), labels=expression(10^-3, 10^-2, 10^-1, 10^0, 10^1, 10^2),
                     name= expression('Total C'*H[4]*' flux (mmol '*m^-2*' '*d^-1*')'), limits = c(0.0001, 10000))+
  theme_linedraw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

pds_tch4_2 <- site_Ea_tch4_L %>%
  ggplot(aes(y=slope))+
  geom_density(color="grey20", fill="#97A2A8", alpha=0.6) +
  geom_hline(aes(yintercept = 0), linetype="dashed") +
  ylab("Apparent activation energy (eV)")+
  xlab("Kernel density") +
  annotate(geom = "text", x=0.3, y = 7, label=expression('Total C'*H[4]), size=4) +
  scale_y_continuous(breaks = seq(from = -5, to = 10, by = 1), limits = c(-5,10)) +
  theme_linedraw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), plot.margin = unit(c(-0.9, 0.1, 2.5, 0.1), "cm"))


ggsave(paste(dir, 'Output', 'Figures', 'lake standardized temp vs. flux10.pdf', sep = '/'), ggarrange(ggarrange(L_dch4_t, L_ech4_t, L_tch4_t, ncol = 1, nrow = 3, common.legend = TRUE, legend="bottom", labels = c("a", "b", "c"), heights = c(0.85,0.81,0.92)) , ggarrange(pds_dch4_2, pds_ech4_2, pds_tch4_2, ncol = 1, nrow = 3, labels = c("d", "", "")),  ncol=2, nrow=1, widths = c(1,0.7), align = "hv"), width = 6.1, height = 11.5)


# Figure 2: Size group plot ------
library(cowplot)
library(grid)
library(gridExtra)
library(svglite)

size_label <- as_labeller(c(small_pond="Area~'<'~'0.001'~km^2", 
                            medium_pond="'0.001'~km^2~'\u2264'~Area~'<'~0.01~km^2", 
                            pond="0.01~km^2~'\u2264'~Area~'<'~0.1~km^2", 
                            small_lake="0.1~km^2~'\u2264'~Area~'<'~1~km^2", 
                            medium_lake="1~km^2~'\u2264'~Area~'<'~10~km^2", 
                            lake="Area~'\u2265'~10~km^2"),
                          default = label_parsed)
## dCH4

line_data_d <- data.frame(sizeclass10 = c("small_pond", "medium_pond", "pond", "small_lake", "medium_lake", "lake"),
                          slope = c(NA, NA, 0.325375825, 0.337634426, 0.834021964, 1.36356398),
                          intercept = c(NA, NA, -0.468135251, -0.218908406, -0.784630772, -1.678811492))

text_d <- data.frame(sizeclass10 = c("small_pond", "medium_pond", "pond", "small_lake", "medium_lake", "lake"),
                     x = rep(0, 6),  # x position of the annotation, adjust as needed
                     y = rep(10000, 6),  # y position of the annotation, adjust as needed
                     label = c("atop(bar(E)[dM]*': Non-significant fit', 'n = 296, sites = 165, p = 0.15')",
                               "atop(bar(E)[dM]*': Non-significant fit', 'n = 564, sites = 145, p = 0.18')",
                               "atop(bar(E)[dM]*' = 0.33 eV, 95% CI: 0.20 – 0.45', 'n = 705, sites = 131, p < 0.001')",
                               "atop(bar(E)[dM]*' = 0.34 eV, 95% CI: 0.18 – 0.49', 'n = 318, sites = 94, p < 0.001')",
                               "atop(bar(E)[dM]*' = 0.83 eV, 95% CI: 0.55 – 1.12', 'n = 450, sites = 50, p < 0.001')",
                               "atop(bar(E)[dM]*' = 1.36 eV, 95% CI: 0.85 – 1.88', 'n = 123, sites = 41, p < 0.001')")
)


p.dch4 <- Lakedat2_clean_d %>% 
  filter(Diff_CH4_mmolm2d > 0) %>% 
  drop_na(ikt) %>% 
  drop_na(sizeclass10) %>% 
  ggplot()+
  facet_wrap(~factor(sizeclass10, c("small_pond", "medium_pond", "pond", "small_lake", "medium_lake", "lake")), scales = "free_y", strip.position = "top", labeller = size_label, nrow=2, ncol=3, dir="h") +
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

## eCH4
line_data_e <- data.frame(sizeclass10 = c("small_pond", "medium_pond", "pond", "small_lake", "medium_lake", "lake"),
                          slope = c(0.282314704, 1.376305834, 1.982594157, 1.612528039, 2.361858522, NA),
                          intercept = c(-1.625938889, 0.284279404, -0.504703791, -0.33474728, 1.194304702,NA))

text_e <- data.frame(sizeclass10 = c("small_pond", "medium_pond", "pond", "small_lake", "medium_lake", "lake"),
                     x = rep(0, 6),  # x position of the annotation, adjust as needed
                     y = rep(10000, 6),  # y position of the annotation, adjust as needed
                     label = c("atop(bar(E)[eM]*' = 0.28 eV, 95% CI: 0.00 – 0.57', 'n = 1008, sites = 13, p = 0.05')",
                               "atop(bar(E)[eM]*' = 1.38 eV, 95% CI: 0.89 – 1.86', 'n = 93, sites = 8, p < 0.001')",
                               "atop(bar(E)[eM]*' = 1.98 eV, 95% CI: 1.74 – 2.22', 'n = 853, sites = 32, p < 0.001')",
                               "atop(bar(E)[eM]*' = 1.61 eV, 95% CI: 1.32 – 1.90', 'n = 364, sites = 18, p < 0.001')",
                               "atop(bar(E)[eM]*' = 2.36 eV, 95% CI: 1.85 – 2.87', 'n = 19, sites = 11, p < 0.001')",
                               "atop(bar(E)[eM]*': Non-significant fit', 'n = 23, sites = 3, p = 0.84')")
)


p.ech4 <- Lakedat2_clean_e %>% 
  filter(Eb_CH4_mmolm2d > 0) %>% 
  drop_na(ikt) %>% 
  ggplot()+
  facet_wrap(~factor(sizeclass10, c("small_pond", "medium_pond", "pond", "small_lake", "medium_lake", "lake")), scales = "free_y", strip.position = "top", labeller = size_label, nrow=2, ncol = 3, dir="h") +
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


plot <- plot_grid(p.dch4, p.ech4, ncol=1, align = 'h', labels = "auto" ) + theme(plot.margin = unit(c(0, 0, 0, 0), "pt"))
x.grob <- textGrob(expression(paste("Standardized temperature ", bgroup("(", frac(1, kT[C]) - frac(1, kT), ")" ), " (eV"^-1,")")), gp=gpar(fontface="bold",  fontsize=14))
top.grob <- textGrob(expression("Water temperature " ( degree*C)), gp=gpar(fontface="bold",  fontsize=14))


ggsave(paste(dir, 'Output', 'Figures', 'lake size temp vs. log10 flux5-1.svg', sep = '/'), grid.arrange(arrangeGrob(plot, bottom = x.grob, top = top.grob)), device = "svg", width = 9.3, height = 13.5)
