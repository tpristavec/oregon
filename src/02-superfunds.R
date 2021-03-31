library(tidyverse)
library(sf)
library(leaflet)

# This file is from https://catalog.data.gov/dataset/superfund-sites (NOAA)

# https://en.wikipedia.org/wiki/List_of_Superfund_sites_in_Oregon
# Latest from FOIA 2021 shows 12: https://semspub.epa.gov/work/HQ/201360.pdf
# EPA shows 19: https://www.epa.gov/superfund/search-superfund-sites-where-you-live
# https://epa.maps.arcgis.com/apps/webappviewer/index.html?id=33cebcdfdd1b4c3a8b51d416956c41f1
# Status and site search: https://cumulis.epa.gov/supercpad/cursites/srchrslt.cfm?start=1
# https://coast.noaa.gov/digitalcoast/data/

# Official: https://www.epa.gov/frs/geospatial-data-download-service look at SEMS NPL
# https://www.epa.gov/enviro/sems-search SEMS
# https://enviro.epa.gov/enviro/efsystemquery.sems?fac_search=primary_name&fac_value=&fac_search_type=Beginning&postal_code=&location_address=&add_search_type=Beginning2&city_name=&county_name=&state_code=OR&program_search=sems&report=facdetail&page_no=1&output_sql_switch=TRUE&database_type=SEMS

# Read in
oregon <- read_rds("./data/oregon.Rds")
data <- read_sf("./data/superfunds/SuperfundSites.gdb-point.shp")

data <- data %>% filter(FIPSCode %in% oregon$countyfips)

# Write
write_rds(data, "./data/superfunds.Rds")