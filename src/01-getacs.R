library(tidycensus)
library(tidyverse)
library(sf)
library(readxl)
library(leaflet)


#
# API key ------------------------------------------------------------------------
#

readRenviron("~/.Renviron")
Sys.getenv("CENSUS_API_KEY")


#
# Select variables ------------------------------------------------------------------------
#

# Total population
# B01003_001
# Population age 65+
# B01001_020:25 (male), B01001_044:49 (female) / B01001_001    
# Population age <=18
# B01001_003:006 (male), B01001_027:30 (female) / B01001_001
# Population Hispanic or Latino
# B03001_003 / B03001_001
# Population Black
# B02001_003 / B02001_001
# Gender (male)
# B01001_002 / B01001_001
# Income below poverty in last 12 months
# B17001_002 / B17001_001
# Population over age 25 without a BA degree
# B15003_002:021 / B15003_001
# Population in labor force that is unemployed
# B23025_005 / B23025_002
# Renters
# B25003_003 / B25003_001

# Select variables
acsvars <- c(
  # total pop
  "B01003_001",
  # age 65 +
  "B01001_020", "B01001_021", "B01001_022", "B01001_023", "B01001_024", "B01001_025",
  "B01001_044", "B01001_045", "B01001_046", "B01001_047", "B01001_048", "B01001_049",
  "B01001_001",
  # age <18
  "B01001_003", "B01001_004", "B01001_005", "B01001_006",
  "B01001_027", "B01001_028", "B01001_029", "B01001_030",
  # Hispanic
  "B03001_003", "B03001_001",
  # Black
  "B02001_003", "B02001_001",
  # Gender (male)
  "B01001_002", "B01001_001",
  # Income below poverty
  "B17001_002", "B17001_001",
  # Without BA
  "B15003_002", "B15003_003", "B15003_004", "B15003_005", "B15003_006", "B15003_007",
  "B15003_008", "B15003_009", "B15003_010", "B15003_011", "B15003_012", "B15003_013",
  "B15003_014", "B15003_015", "B15003_016", "B15003_017", "B15003_018", "B15003_019",
  "B15003_020", "B15003_021", "B15003_001",
  # Unemployed
  "B23025_005", "B23025_002",
  # Renters
  "B25003_003", "B25003_001"
)


#
# Get data ------------------------------------------------------------------------
#

# Get data from 2014/18 5-year estimates at tract level
data_tract <- get_acs(geography = "tract", state = "41",
                      variables = acsvars,
                      year = 2019, survey = "acs5",
                      cache_table = TRUE, output = "wide", geometry = TRUE,
                      keep_geo_vars = TRUE)

# ZIPs of interest
zips <- c("97203", "97208", "97209", "97210", "97211", "97212", "97217", "97227", "97228", "97256", "97231", "97283")

# HUD ZIP to tract, 4th quarter of 2020: https://www.huduser.gov/portal/datasets/usps_crosswalk.html
xwalk <- read_xlsx("./data/ZIP_TRACT_122020.xlsx", col_types = "text")

xwalk <- xwalk %>% filter(ZIP %in% zips)
duplicated(xwalk$TRACT) # 29 duplicated, 54 unique
unique(xwalk$TRACT)

data_tract <- data_tract %>% filter(GEOID %in% xwalk$TRACT)


#
# Calculate ------------------------------------------------------------------------
#

# Tract level 
data_final <- data_tract %>% transmute(
  STATEFP = STATEFP,
  COUNTYFP = COUNTYFP,
  TRACTCE = TRACTCE,
  GEOID = GEOID,
  NAME.x = NAME.x,
  NAME.y = NAME.y,
  geometry = geometry,
  totalpop = B01003_001E,
  age65 = (B01001_020E + B01001_021E + B01001_022E + B01001_023E + B01001_024E + B01001_025E +
             B01001_044E + B01001_045E + B01001_046E + B01001_047E + B01001_048E + B01001_049E) / B01001_001E * 100,
  under18 = (B01001_003E + B01001_004E + B01001_005E + B01001_006E +
               B01001_027E + B01001_028E + B01001_029E + B01001_030E) / B01001_001E * 100,
  hispanic = B03001_003E / B03001_001E * 100,
  black = B02001_003E / B02001_001E * 100,
  female = (1 - (B01001_002E / B01001_001E)) * 100,
  inpov = B17001_002E / B17001_001E * 100,
  noba = (B15003_002E + B15003_003E + B15003_004E + B15003_005E + B15003_006E + B15003_007E + B15003_008E +
            B15003_009E + B15003_010E + B15003_011E + B15003_012E + B15003_013E + B15003_014E + B15003_015E +
            B15003_016E + B15003_017E + B15003_018E + B15003_019E + B15003_020E + B15003_021E) / B15003_001E * 100,
  unempl = B23025_005E / B23025_002E * 100,
  renters = B25003_003E / B25003_001E * 100
)

# 0 denominators NaN to NA (only Census Tract 9800, Multnomah County, Oregon)
data_final <- data_final %>% mutate(
  totalpop = ifelse(is.nan(totalpop), NA, totalpop),
  age65 = ifelse(is.nan(age65), NA, age65),
  under18 = ifelse(is.nan(under18), NA, under18),
  hispanic = ifelse(is.nan(hispanic), NA, hispanic),
  black = ifelse(is.nan(black), NA, black),
  female = ifelse(is.nan(female), NA, female),
  inpov = ifelse(is.nan(inpov), NA, inpov),
  noba = ifelse(is.nan(noba), NA, noba),
  unempl = ifelse(is.nan(unempl), NA, unempl),
  renters = ifelse(is.nan(renters), NA, renters)
)


#
# Tract to ZIP (caution: tracts to multiple ZIPs) ------------------------------------------------------------------------
#

xwalk_wide <- pivot_wider(xwalk, id_cols = TRACT, names_from = ZIP, values_from = ZIP, names_prefix = "zip")
xwalk_list <- xwalk_wide %>% unite(whichzips, 2:12, sep = ", ", na.rm = TRUE)

data_final <- left_join(data_final, xwalk_list, by = c("GEOID" = "TRACT"))


#
# Add some variables ------------------------------------------------------------------------
#

data_final$countyfips <- as.character(paste0("41", data_final$COUNTYFP))


#
# Write ------------------------------------------------------------------------
#

write_rds(data_final, "./data/oregon.Rds")