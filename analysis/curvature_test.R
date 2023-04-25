# This script evaluates the model and compares the target variables selected

# load packages
library(dplyr)
library(tibble)
library(rsofun)
library(ggplot2)
library(multidplyr)
library(patchwork)

load("~/GFDY/data/inputs_mod/ddf_obs.RData")

# For DBH mortality ####
# DBH mortality has the shape params: p1=1.5, p2=2.5, p3=4.0
# The calibration was done with p2=2.5
load("~/GFDY/data/inputs_mod/df_drivers_DBH_gs.RData")
load("~/GFDY/data/inputs_mod/settings_calib_DBH_gs_uniq_euler.RData")

df_drivers$params_species[[1]]$phiRL      <-  settings_calib_DBH_gs$par_opt["phiRL"]  
df_drivers$params_species[[1]]$LAI_light  <-  settings_calib_DBH_gs$par_opt["LAI_light"]
df_drivers$params_tile[[1]]$tf_base       <-  settings_calib_DBH_gs$par_opt["tf_base"]
df_drivers$params_tile[[1]]$par_mort      <-  settings_calib_DBH_gs$par_opt["par_mort"]
df_drivers$params_tile[[1]]$par_mort_under<-  settings_calib_DBH_gs$par_opt["par_mort_under"]

df_calib_DBH_gs <- runread_lm3ppa_f(
  df_drivers,
  makecheck = TRUE,
  parallel = FALSE
)

df_calib_DBH_gs$data[[1]]$output_annual_tile %>% 
  ggplot() +
  geom_line(aes(x = year, y = plantC)) +
  theme_classic()+labs(x = "Year", y = "plantC")

df_calib_DBH_gs$data[[1]]$output_annual_tile %>% 
  ggplot() +
  geom_line(aes(x = year, y = NPP)) +
  theme_classic()+labs(x = "Year", y = "NPP")

df_calib_DBH_gs$data[[1]]$output_annual_tile %>% 
  ggplot() +
  geom_line(aes(x = year, y = c_deadtrees+m_turnover)) +
  theme_classic()+labs(x = "Year", y = "c_deadtrees+m_turnover")

# DBH pX1=0.06
# LUE control
write.csv(df_calib_DBH_gs$data[[1]]$output_annual_tile,   "~/GFDY/data/outputs_mod/ea1sa1DBHpX1gl_out_annual_tile.csv")
write.csv(df_calib_DBH_gs$data[[1]]$output_annual_cohorts,"~/GFDY/data/outputs_mod/ea1sa1DBHpX1gl_out_annual_cohorts.csv")
# LUE +15% (modified in the model)
write.csv(df_calib_DBH_gs$data[[1]]$output_annual_tile,   "~/GFDY/data/outputs_mod/ea2sa1DBHpX1gl_out_annual_tile.csv")
write.csv(df_calib_DBH_gs$data[[1]]$output_annual_cohorts,"~/GFDY/data/outputs_mod/ea2sa1DBHpX1gl_out_annual_cohorts.csv")
# LUE +30% (modified in the model)
write.csv(df_calib_DBH_gs$data[[1]]$output_annual_tile,   "~/GFDY/data/outputs_mod/ea3sa1DBHpX1gl_out_annual_tile.csv")
write.csv(df_calib_DBH_gs$data[[1]]$output_annual_cohorts,"~/GFDY/data/outputs_mod/ea3sa1DBHpX1gl_out_annual_cohorts.csv")

# DBH pX2=0.10
# LUE control
write.csv(df_calib_DBH_gs$data[[1]]$output_annual_tile,   "~/GFDY/data/outputs_mod/ea1sa1DBHpX2gl_out_annual_tile.csv")
write.csv(df_calib_DBH_gs$data[[1]]$output_annual_cohorts,"~/GFDY/data/outputs_mod/ea1sa1DBHpX2gl_out_annual_cohorts.csv")
# LUE +15%
write.csv(df_calib_DBH_gs$data[[1]]$output_annual_tile,   "~/GFDY/data/outputs_mod/ea2sa1DBHpX2gl_out_annual_tile.csv")
write.csv(df_calib_DBH_gs$data[[1]]$output_annual_cohorts,"~/GFDY/data/outputs_mod/ea2sa1DBHpX2gl_out_annual_cohorts.csv")
# LUE +30%
write.csv(df_calib_DBH_gs$data[[1]]$output_annual_tile,   "~/GFDY/data/outputs_mod/ea3sa1DBHpX2gl_out_annual_tile.csv")
write.csv(df_calib_DBH_gs$data[[1]]$output_annual_cohorts,"~/GFDY/data/outputs_mod/ea3sa1DBHpX2gl_out_annual_cohorts.csv")

# DBH pX3=0.15
# LUE control
write.csv(df_calib_DBH_gs$data[[1]]$output_annual_tile,   "~/GFDY/data/outputs_mod/ea1sa1DBHpX3gl_out_annual_tile.csv")
write.csv(df_calib_DBH_gs$data[[1]]$output_annual_cohorts,"~/GFDY/data/outputs_mod/ea1sa1DBHpX3gl_out_annual_cohorts.csv")
# LUE +15%
write.csv(df_calib_DBH_gs$data[[1]]$output_annual_tile,   "~/GFDY/data/outputs_mod/ea2sa1DBHpX3gl_out_annual_tile.csv")
write.csv(df_calib_DBH_gs$data[[1]]$output_annual_cohorts,"~/GFDY/data/outputs_mod/ea2sa1DBHpX3gl_out_annual_cohorts.csv")
# LUE +30%
write.csv(df_calib_DBH_gs$data[[1]]$output_annual_tile,   "~/GFDY/data/outputs_mod/ea3sa1DBHpX3gl_out_annual_tile.csv")
write.csv(df_calib_DBH_gs$data[[1]]$output_annual_cohorts,"~/GFDY/data/outputs_mod/ea3sa1DBHpX3gl_out_annual_cohorts.csv")

# For GR mortality ####

load("~/GFDY/data/inputs_mod/df_drivers_GR_gs.RData")
load("~/GFDY/data/inputs_mod/settings_calib_GR_gs_uniq_euler.RData")

df_drivers$params_species[[1]]$phiRL      <-  settings_calib_GR_gs$par_opt["phiRL"]
df_drivers$params_species[[1]]$LAI_light  <-  settings_calib_GR_gs$par_opt["LAI_light"]
df_drivers$params_tile[[1]]$tf_base       <-  settings_calib_GR_gs$par_opt["tf_base"]
df_drivers$params_tile[[1]]$par_mort      <-  settings_calib_GR_gs$par_opt["par_mort"]
df_drivers$params_tile[[1]]$par_mort_under<-  settings_calib_GR_gs$par_opt["par_mort_under"]

df_calib_GR_gs <- runread_lm3ppa_f(
  df_drivers,
  makecheck = TRUE,
  parallel = FALSE
)

df_calib_GR_gs$data[[1]]$output_annual_tile %>% 
  ggplot() +
  geom_line(aes(x = year, y = plantC)) +
  theme_classic()+labs(x = "Year", y = "plantC")

# GR p1=-0.5
# LUE control
write.csv(df_calib_GR_gs$data[[1]]$output_annual_tile,   "~/GFDY/data/outputs_mod/ea1sa1GRpX1gl_out_annual_tile.csv")
write.csv(df_calib_GR_gs$data[[1]]$output_annual_cohorts,"~/GFDY/data/outputs_mod/ea1sa1GRpX1gl_out_annual_cohorts.csv")
# LUE +15%
write.csv(df_calib_GR_gs$data[[1]]$output_annual_tile,   "~/GFDY/data/outputs_mod/ea2sa1GRpX1gl_out_annual_tile.csv")
write.csv(df_calib_GR_gs$data[[1]]$output_annual_cohorts,"~/GFDY/data/outputs_mod/ea2sa1GRpX1gl_out_annual_cohorts.csv")
# LUE +30%
write.csv(df_calib_GR_gs$data[[1]]$output_annual_tile,   "~/GFDY/data/outputs_mod/ea3sa1GRpX1gl_out_annual_tile.csv")
write.csv(df_calib_GR_gs$data[[1]]$output_annual_cohorts,"~/GFDY/data/outputs_mod/ea3sa1GRpX1gl_out_annual_cohorts.csv")

# GR p2=-0.8
# LUE control
write.csv(df_calib_GR_gs$data[[1]]$output_annual_tile,   "~/GFDY/data/outputs_mod/ea1sa1GRpX2gl_out_annual_tile.csv")
write.csv(df_calib_GR_gs$data[[1]]$output_annual_cohorts,"~/GFDY/data/outputs_mod/ea1sa1GRpX2gl_out_annual_cohorts.csv")
# LUE +15%
write.csv(df_calib_GR_gs$data[[1]]$output_annual_tile,   "~/GFDY/data/outputs_mod/ea2sa1GRpX2gl_out_annual_tile.csv")
write.csv(df_calib_GR_gs$data[[1]]$output_annual_cohorts,"~/GFDY/data/outputs_mod/ea2sa1GRpX2gl_out_annual_cohorts.csv")
# LUE +30%
write.csv(df_calib_GR_gs$data[[1]]$output_annual_tile,   "~/GFDY/data/outputs_mod/ea3sa1GRpX2gl_out_annual_tile.csv")
write.csv(df_calib_GR_gs$data[[1]]$output_annual_cohorts,"~/GFDY/data/outputs_mod/ea3sa1GRpX2gl_out_annual_cohorts.csv")

# GR p3=-1.4
# LUE control
write.csv(df_calib_GR_gs$data[[1]]$output_annual_tile,   "~/GFDY/data/outputs_mod/ea1sa1GRpX3gl_out_annual_tile.csv")
write.csv(df_calib_GR_gs$data[[1]]$output_annual_cohorts,"~/GFDY/data/outputs_mod/ea1sa1GRpX3gl_out_annual_cohorts.csv")
# LUE +15%
write.csv(df_calib_GR_gs$data[[1]]$output_annual_tile,   "~/GFDY/data/outputs_mod/ea2sa1GRpX3gl_out_annual_tile.csv")
write.csv(df_calib_GR_gs$data[[1]]$output_annual_cohorts,"~/GFDY/data/outputs_mod/ea2sa1GRpX3gl_out_annual_cohorts.csv")
# LUE +30%
write.csv(df_calib_GR_gs$data[[1]]$output_annual_tile,   "~/GFDY/data/outputs_mod/ea3sa1GRpX3gl_out_annual_tile.csv")
write.csv(df_calib_GR_gs$data[[1]]$output_annual_cohorts,"~/GFDY/data/outputs_mod/ea3sa1GRpX3gl_out_annual_cohorts.csv")
