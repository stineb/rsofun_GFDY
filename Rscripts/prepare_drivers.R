
# Prepare drivers and inputs for calibrating LM3-PPA. Vignette prepare_inputs_rsofun.Rmd used as reference.

library(rsofun)
library(dplyr)
library(readr)
library(ingestr)

## Site selection and meta data

sitename <- "CH-Lae"

# Take only year 2004 to 2014, corresponding to subset of data for site CH-Lae
siteinfo <- data.frame(sitename="CH-Lae", lon = 8.365, lat = 47.47808, elv = 700, year_start = 2004, year_end = 2014, classid = NA, c4 = FALSE, whc = NA, koeppen_code = NA, igbp_land_use = "Mixed Forests", plant_functional_type = "Broadleaf trees")

siteinfo <- as_tibble(siteinfo)
siteinfo <- siteinfo %>% 
  dplyr::mutate(date_start = lubridate::ymd(paste0(year_start, "-01-01"))) %>%
  dplyr::mutate(date_end = lubridate::ymd(paste0(year_end, "-12-31")))


## Simulation settings

params_siml <- list(
  spinup                = TRUE,
  spinupyears           = 1800, #1793,
  recycle               = 1, # 9 or 11 changed to 1 when aggregating forcing into 1 year
  firstyeartrend        = 1998,
  nyeartrend            = 1, #9 or 11 (longer transient years)
  outputhourly          = TRUE,
  outputdaily           = TRUE,
  do_U_shaped_mortality = FALSE,
  update_annualLAImax   = FALSE,
  do_closedN_run        = TRUE,
  method_photosynth     = "gs_leuning", # gs_leuning or pmodel
  method_mortality      = "dbh" # dbh or cstarvation or growthrate or const_selfthin
)

siteinfo <- tibble(siteinfo, params_siml = list(tibble(params_siml)))

## Define model parameters

#### Tile-level parameters

params_tile <- list(
  soiltype     = 3,     # Sand = 1, LoamySand = 2, SandyLoam = 3, SiltLoam = 4, FrittedClay = 5, Loam = 6, Clay = 7
  FLDCAP       = 0.4,   # soil property: field capacity 
  WILTPT       = 0.05,  # soil property: wilting point
  K1           = 2.0,   # turnover rate of fast SOM per year
  K2           = 0.05,  # turnover rate of slow SOM per year
  K_nitrogen   = 8.0,   # mineral Nitrogen turnover rate
  MLmixRatio   = 0.8,   # the ratio of C and N returned to litters from microbes
  etaN         = 0.025, # loss rate with runoff
  LMAmin       = 0.02,  # minimum LMA, boundary condition
  fsc_fine     = 1.0,   # fraction of fast turnover carbon in fine biomass
  fsc_wood     = 0.0,   # fraction of fast turnover carbon in wood biomass
  GR_factor    = 0.33,  # growth respiration factor
  l_fract      = 0.0,   # fraction of the carbon retained after leaf drop
  retransN     = 0.0,   # retranslocation coefficient of Nitrogen
  f_initialBSW = 0.2,
  f_N_add      = 0.02   # re-fill of N for sapwood
)

#### Species-level parameters

params_species <- tibble(
  lifeform      = rep(1,16),                 # 0 for grasses; 1 for trees
  phenotype     = rep(0,16),                 # 0 for Deciduous; 1 for Evergreen
  pt            = rep(0,16),                 # 0 for C3; 1 for C4
  seedlingsize  = rep(0.05,16),              # initial size of seedlings
  LMA           = rep(0.05,16),             # Leaf mass per unit area: 0.035, 0.085, 0.135
  phiRL         = rep(3.5,16),               # Root:Shoot ratio:  4.0, 6.0, 8.0
  LNbase        = rep(0.8E-3,16),            # kgN m-2 leaf, Vmax = 0.03125*LNbase
  laimax        = rep(3.5,16),               # maximum crown LAI
  LAI_light     = rep(4.0,16),               # Light-limited crown LAI
  Nfixrate0     = rep(0,16),                 # 0.03 kgN kgRootC-1 yr-1
  NfixCost0     = rep(12,16),                # 12, 24 gC/gN
  phiCSA        = rep(0.25E-4,16),           # ratio of sapwood area to leaf area
  mortrate_d_c  = rep(0.01,16),              # canopy tree mortality rate, year-1
  mortrate_d_u  = rep(0.075,16),             # understory tree mortality rate, year-1
  maturalage    = rep(5,16),                 # the age that can reproduce
  fNSNmax       = rep(5,16)                  # multiplier for NSNmax as sum of potential bl and br
) 

## Define soil parameters

#### Soil parameters

# adopted from datatypes.mod.f90 l.538
params_soil <- tibble(
  type              = c("Coarse","Medium","Fine","CM","CF","MF","CMF","Peat","MCM"),
  GMD               = c(0.7, 0.4, 0.3, 0.1, 0.1, 0.07, 0.007, 0.3, 0.3),
  GSD               = c(5.0, 5.3, 7.4, 6.1, 6.1, 14.0, 15.0, 7.4, 7.4),
  vwc_sat           = c(0.380, 0.445, 0.448, 0.412, 0.414, 0.446, 0.424, 0.445, 0.445),
  chb               = c(3.5,6.4,11.0,4.8,6.3,8.4,6.3,6.4,6.4),
  psi_sat_ref       = c(-600, -790, -910, -1580, -1680, -1880, -5980, -790, -790), # Pa
  k_sat_ref         = c(130.8, 75.1, 53.2, 12.1, 11.1, 12.7, 1.69, 53.2, 53.2), # mol/(s MPa m)
  alphaSoil         = rep(1, 9),
  heat_capacity_dry = c(1.2e6, 1.1e6, 1.1e6, 1.1e6, 1.1e6, 1.1e6, 1.1e6, 1.4e6, 1.0)
)

#### Initial cohort specification

init_cohort <- tibble(
  init_cohort_species = rep(2, 10),
  init_cohort_nindivs = rep(1,10),    # initial individual density, individual/m2
  init_cohort_bsw     = rep(0.05,10), # initial biomass of sapwood, kg C/individual
  init_cohort_bHW     = rep(0.0, 10), # initial biomass of heartwood, kg C/tree
  init_cohort_nsc     = rep(0.05,10)  # initial non-structural biomass
)

#### Initial soil pools

# high N input --> Deciduous, low  N input --> Evergreen

init_soil <- list(
  init_fast_soil_C    = 0.0,    # initial fast soil C, kg C/m2
  init_slow_soil_C    = 0.0,    # initial slow soil C, kg C/m2
  init_Nmineral       = 0.015,  # Mineral nitrogen pool, (kg N/m2)
  N_input             = 0.0008  # annual N input to soil N pool, kgN m-2 yr-1
)

### Define soil parameters

# For now, this is implemented as an illustration. Should be made site-specific.

df_soiltexture <- bind_rows(
  top    = tibble(layer = "top", fsand = 0.4, fclay = 0.3, forg = 0.1, fgravel = 0.1),
  bottom = tibble(layer = "bottom", fsand = 0.4, fclay = 0.3, forg = 0.1, fgravel = 0.1)
)

## Get input (Forcing data)

## Meteo data

library(tidyverse)
FLX_HH_LAE <- read.csv("~/data/FLUXNET-2015_Tier1/20191024/HH/FLX_CH-Lae_FLUXNET2015_FULLSET_HH_2004-2014_1-3.csv")
names(FLX_HH_LAE)

FluxForcing <- FLX_HH_LAE %>% dplyr::select(TIMESTAMP_START,PPFD_IN,SW_IN_F,TA_F,TS_F_MDS_1,RH,P_F,WS_F,PA_F,SWC_F_MDS_1)
FluxForcing <- FluxForcing %>% dplyr::mutate_all(funs(ifelse(.==-9999, NA, .))) %>% mutate(YEAR=str_sub(TIMESTAMP_START, 1, 4)) %>% mutate(MONTH=str_sub(TIMESTAMP_START, 5, 6)) %>% mutate(DOY=str_sub(TIMESTAMP_START, 7, 8)) %>% mutate(HOUR=str_sub(TIMESTAMP_START, 9, 10)) %>% relocate(YEAR, MONTH, DOY, HOUR) %>% mutate(YEAR=as.integer(YEAR),MONTH=as.numeric(MONTH),DOY=as.numeric(DOY),HOUR=as.numeric(HOUR))

# Half-hurly data
FluxForcing <- FluxForcing %>% dplyr::rename(PAR=PPFD_IN,Swdown=SW_IN_F,TEMP=TA_F,SoilT=TS_F_MDS_1,RH=RH,RAIN=P_F,WIND=WS_F,PRESSURE=PA_F,SWC=SWC_F_MDS_1) %>% dplyr::select(-TIMESTAMP_START) %>% 
  mutate(datehour = make_datetime(YEAR, MONTH, DOY, HOUR))  %>% mutate(date = make_datetime(YEAR, MONTH, DOY)) %>%  mutate(PRESSURE = PRESSURE*1000) %>%  mutate(PAR = PAR*2.02) %>%
  dplyr::filter(!(mday(datehour)==29 & month(datehour)==2)) %>% mutate(DOY=lubridate::yday(datehour)) %>% dplyr::select(-MONTH) #%>% drop_na()

# Hourly data
FluxForcing <- FluxForcing %>% 
  dplyr::group_by(lubridate::year(datehour),lubridate::month(datehour),lubridate::day(datehour),lubridate::hour(datehour)) %>% 
  summarise_at(vars(1:14), list(~mean(., na.rm = TRUE)))  %>% mutate(sitename = "CH-Lae") 

## convert rain to units per seconds
dt_secs <- lubridate::interval(FluxForcing$datehour[1], FluxForcing$datehour[2]) %>% 
  time_length("seconds")
FluxForcing <- FluxForcing %>% mutate(RAIN = RAIN / dt_secs)

FluxForcing <- FluxForcing[,-c(1:4)]

## CO2

# Ingesting CO2 data is particularly simple. We can safely assume it's well mixed in the atmosphere (independent of site location), and we can use a annual mean value for all days in respective years.  

# General for several sites
df_co2 <- ingestr::ingest(
  siteinfo,
  source  = "co2_mlo",
  verbose = FALSE
  )
save(df_co2, file = "~/data/df_co2_LAE.RData")
df_co2$data[[1]] # $co2 # To select CO2 

# For one specific site
df_co2 <- ingestr::ingest_bysite(
  sitename  = "CH-Lae",
  source  = "co2_mlo",
  year_start= 2007,
  year_end  = 2014,
  verbose = FALSE
  )

# Combine the two FLUXNET and CO2 to create the complete forcing data.

FluxForcing <- FluxForcing %>% 
  #tidyr::unnest(data) %>% 
  left_join(
    df_co2 %>% 
      tidyr::unnest(data),
    by = c("sitename", "date")
  ) %>% rename(aCO2_AW=co2) %>% relocate(aCO2_AW, .after = PRESSURE) 
  #group_by(sitename) %>% 
  #tidyr::nest()

FluxForcing <- FluxForcing %>% relocate(aCO2_AW, .after = PRESSURE) 

forcingLAE <- FluxForcing
save(forcingLAE, file = "~/data/forcingLAE.RData")

if (params_siml$method_photosynth == "gs_leuning"){
  forcingLAE <- forcingLAE %>% 
    dplyr::group_by(lubridate::month(datehour),lubridate::day(datehour),lubridate::hour(datehour)) %>% 
    summarise_at(vars(1:13), list(~mean(., na.rm = TRUE)))
  forcingLAE <- forcingLAE[,-c(1:3)]
}

if (params_siml$method_photosynth == "pmodel" && dt_secs != (60*60*24)){
  forcingLAE <- forcingLAE %>% 
    dplyr::group_by(lubridate::month(datehour),lubridate::day(datehour)) %>% 
    summarise_at(vars(1:13), list(~mean(., na.rm = TRUE)))
  forcingLAE <- forcingLAE[,-c(1:2)]
}
str(forcingLAE)


