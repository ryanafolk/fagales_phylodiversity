library(data.table)
library(rnaturalearth)
library(dplyr)
library(ggplot2)
library(raster)

# basemap of the world
world <- ne_countries(returnclass = c("sf"))

# PD_P
pd <- raster("Biodiverse_Outputs/res15km_PD_P.tif")
pd_df <- raster::as.data.frame(pd, xy = T) %>% 
  na.omit() %>% 
  rename(PD_P = 3)

pd_plot <- ggplot() +
  geom_tile(pd_df, mapping = aes(x = x, y = y, fill = PD_P, color = PD_P)) +
  scale_fill_viridis_c(trans = "log", option = "plasma", breaks = c(0.006, .02, 0.05)) +
  scale_color_viridis_c(trans = "log", option = "plasma", breaks = c(0.006, .02, 0.05)) +
  geom_sf(world, mapping = aes(), fill = "transparent", size = 0.05) +
  coord_sf(ylim = c(-57, 83)) +
  theme_void()

ggsave(plot = pd_plot, filename = "Fagales_Figures/PD_P.png",
       width = 6, height = 3.5)


## RPD
rpd <- raster("Biodiverse_Outputs/res15km_PHYLO_RPD2.tif")
rpd_df <- raster::as.data.frame(rpd, xy = T) %>% 
  na.omit() %>% 
  rename(RPD = 3)

rpd_plot <- ggplot() +
  geom_tile(rpd_df, mapping = aes(x = x, y = y, fill = RPD, color = RPD)) +
  scale_fill_viridis_c(trans = "log", option = "mako", breaks = c(0.4, 1.0, 2.7)) +
  scale_color_viridis_c(trans = "log", option = "mako", breaks = c(0.4, 1.0, 2.7)) +
  geom_sf(world, mapping = aes(), fill = "transparent", size = 0.05) +
  coord_sf(ylim = c(-57, 83)) +
  theme_void()

ggsave(plot = rpd_plot, filename = "Fagales_Figures/RPD.pdf",
       width = 6, height = 3.5, dpi = 600)

## cwe
cwe <- raster("Biodiverse_Outputs/res15km_PE_CWE.tif")
cwe_df <- raster::as.data.frame(cwe, xy = T) %>% 
  na.omit() %>% 
  rename(CWE = 3)

cwe_plot <- ggplot() +
  geom_tile(cwe_df, mapping = aes(x = x, y = y, fill = CWE, color = CWE)) +
  scale_fill_viridis_c(trans = "log", option = "E", breaks = c(0.00005,0.0009, 0.02)) +
  scale_color_viridis_c(trans = "log", option = "E", breaks = c(0.00005,0.0009, 0.02)) +
  geom_sf(world, mapping = aes(), fill = "transparent", size = 0.05) +
  coord_sf(ylim = c(-57, 83)) +
  theme_void()

ggsave(plot = cwe_plot, filename = "Fagales_Figures/CWE.png",
       width = 6, height = 3.5, dpi = 600)


## RPE
rpe <- raster("Biodiverse_Outputs/rand50km_PHYLO_RPE2.tif")
rpe_df <- raster::as.data.frame(rpe, xy = T) %>% 
  na.omit() %>% 
  rename(RPE = 3)

rpe_plot <- ggplot() +
  geom_tile(rpe_df, mapping = aes(x = x, y = y, fill = RPE, color = RPE)) +
  scale_fill_viridis_c(trans = "log", breaks = c(0.1,2.7, 54.6)) +
  scale_color_viridis_c(trans = "log", breaks = c(0.1,2.7, 54.6)) +
  geom_sf(world, mapping = aes(), fill = "transparent", size = 0.05) +
  coord_sf(ylim = c(-57, 83)) +
  theme_void()

ggsave(plot = rpe_plot, filename = "Fagales_Figures/RPE.pdf",
       width = 6, height = 3.5, dpi = 600)


# PMPD
pmpd <- raster("Biodiverse_Outputs/res50km_PMPD1_MEAN.tif")
pmpd_df <- raster::as.data.frame(pmpd, xy = T) %>% 
  na.omit() %>% 
  rename(PMPD = 3)

pmpd_plot <- ggplot() +
  geom_tile(pmpd_df, mapping = aes(x = x, y = y, fill = PMPD, color = PMPD)) +
  scale_fill_viridis_c(option = "turbo") +
  scale_color_viridis_c(option = "turbo") +
  geom_sf(world, mapping = aes(), fill = "transparent", size = 0.05) +
  coord_sf(ylim = c(-57, 83)) +
  theme_void()

ggsave(plot = pmpd_plot, filename = "Fagales_Figures/PMPD.pdf",
       width = 6, height = 3.5, dpi = 600)


## PD_RAND
pd_rand <- raster("Randomization_Outputs/rand_50km_PD_P.tif")
pd_rand_df <- raster::as.data.frame(pd_rand, xy = T) %>% 
  na.omit() %>% 
  rename(PD_Sig = 3)

pd_rand_df <- pd_rand_df %>% 
  mutate(Significance = case_when(PD_Sig <= 0.05 ~ "Low",
                                  PD_Sig >= 0.95 ~ "High")) %>% 
  filter(!is.na(PD_Sig)) 

pd_rand_plot <- ggplot() +
  geom_tile(pd_rand_df, mapping = aes(x = x, y = y,
                                      fill = factor(Significance, levels = c("High", "Low")))) +
  scale_fill_manual(values = c("dodgerblue2", "red1")) +
  labs(fill = "Significance") +
  geom_sf(world, mapping = aes(), fill = "transparent", size = 0.05) +
  coord_sf(ylim = c(-57, 83)) +
  theme_void()

ggsave(plot = pd_rand_plot, filename = "Fagales_Figures/PD_Randomization.png",
       width = 6, height = 3.5, dpi = 600)


# RPD Randomization
rpd_rand <- raster("Randomization_Outputs/rand_50km_PHYLO_RPD2.tif")
rpd_rand_df <- raster::as.data.frame(rpd_rand, xy = T) %>% 
  na.omit() %>% 
  rename(RPD_Sig = 3)

rpd_rand_df <- rpd_rand_df %>% 
  mutate(Significance = case_when(RPD_Sig <= 0.05 ~ "Low",
                                  RPD_Sig >= 0.95 ~ "High")) %>% 
  filter(!is.na(RPD_Sig)) 

rpd_rand_plot <- ggplot() +
  geom_tile(rpd_rand_df, mapping = aes(x = x, y = y,
                                       fill = factor(Significance, levels = c("High", "Low")))) +
  scale_fill_manual(values = c("dodgerblue2", "red1")) +
  labs(fill = "Significance") +
  geom_sf(world, mapping = aes(), fill = "transparent", size = 0.05) +
  coord_sf(ylim = c(-57, 83)) +
  theme_void()

ggsave(plot = rpd_rand_plot, filename = "Fagales_Figures/RPD_Randomization.png",
       width = 6, height = 3.5, dpi = 600)


## CANAPE
xx <- raster("Randomization_Outputs/rand_50km_PE_WE_P.tif")
xx_df <- raster::as.data.frame(xx, xy = TRUE) %>% 
  dplyr::rename(PE_WE_P = 3)

yy <- raster("Randomization_Outputs/rand_50km_PHYLO_RPE_NULL2.tif")
yy_df <- raster::as.data.frame(yy, xy = TRUE) %>% 
  dplyr::rename(RPE_NULL2 = 3)

zz <- raster("Randomization_Outputs/rand_50km_PHYLO_RPE2.tif")
zz_df <- raster::as.data.frame(zz, xy = TRUE) %>% 
  dplyr::rename(RPE2 = 3)


significance_fun <- function(x, y, z){
  #  simplify the logic below
  ifelse(test = is.na(x), yes = 0, no = x)
  ifelse(test = is.na(y), yes = 0, no = x)
  ifelse(test = is.na(z), yes = 0.5, no = x)
  
  ifelse(test = x <= 0.95 & y <= 0.95, yes = "Not Sig",
         no = 
           ifelse(z < 0.025, yes = "Neo",
                  no = 
                    ifelse(z > 0.975, yes = "Paleo",
                           no = "Mixed")
           ))
}

Significance <- significance_fun(x = xx_df$PE_WE_P, 
                                 y = yy_df$RPE_NULL2, 
                                 z = zz_df$RPE2)

df2 <- left_join(xx_df, yy_df) 
df2 <- left_join(df2, zz_df)

df3 <- df2 %>% 
  mutate(PE_WE_P = tidyr::replace_na(PE_WE_P, 0),
         RPE_NULL2 = tidyr::replace_na(RPE_NULL2, 0),
         RPE2 = tidyr::replace_na(RPE2, 0.5)) 

Significance <- significance_fun(x = df3$PE_WE_P, y = df3$RPE_NULL2,
                                 z = df3$RPE2)

canape_csv <- cbind(df3, Significance) %>% 
  dplyr::select(x, y, Significance) %>% 
  dplyr::filter(Significance != "Not Sig")

canape_plot <- ggplot() + 
  geom_tile(canape_csv, mapping = aes(x = x, y = y, fill = Significance)) + 
  scale_fill_manual(values = c("#CB7FFF", "red", "royalblue1")) +
  geom_sf(world, mapping = aes(), fill = "transparent", size = 0.05) +
  coord_sf(ylim = c(-57, 83)) +
  theme_void()

ggsave(plot = canape_plot, filename = "Fagales_Figures/CANAPE.png",
       width = 6, height = 3.5, dpi = 600)

## Richness
fagales_15km <- data.table::fread("data/SDM_Results/Fagales_ModelResults_AsCSV_15km.csv")
rich_15 <- fagales_15km %>% 
  group_by(x, y) %>% 
  summarise(richness = n())

rich_15 <- na.omit(rich_15)

p15 <- ggplot() +
  geom_tile(rich_15, mapping = aes(x = x , y = y, fill = richness, color = richness)) +
  scale_fill_viridis_c(option = "inferno", trans = "log", na.value = "transparent", breaks = c(1,7, 50)) +
  scale_color_viridis_c(option = "inferno", trans = "log", na.value = "transparent", breaks = c(1,7, 50)) +
  geom_sf(world, mapping = aes(), fill = "transparent", size = 0.05) +
  coord_sf(ylim = c(-57, 83)) +
  theme_void()

ggsave(plot = p15, filename = "Fagales_Figures/richness.pdf",
       width = 6, height = 3.5, dpi = 600)
