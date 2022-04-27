library(tidyverse)
library(tidycensus)
library(sf)
library(terra)

red = rast("sacramento_landsat_red.tif")
infrared = rast("sacramento_landsat_near_infrared.tif")

coltab(red) <- NULL
coltab(infrared) <- NULL

plot(red)
plot(infrared)

new = (red - infrared)/(red + infrared)
plot(new)

acs_vars = load_variables(2019, "acs5")

Sacra = get_acs(
  geography= "tract",  # could be tract, block group, etc.
  variables=c(
    "Income"="B19127_001",
    "Population" = "B01003_001"
  ),
  year=2019,
  state="CA",
  county = "Sacramento",
  survey="acs5",
  output="wide"
)

View(Sacra)

counties = read_sf("tl_2019_us_county/tl_2019_us_county.shp")
SACR_counties = filter(counties, GEOID== '06067')
Sacra$GEOID = str_match(Sacra$GEOID, "^[0-9]{5}")

SACR_counties = left_join(SACR_counties, Sacra, by="GEOID")

SACR_counties$below_35 = (SACR_counties$IncomeE / SACR_counties$PopulationE)

# project the data to North Carolina State Plane
SACR_counties = st_transform(SACR_counties, 3489)

plot(SACR_counties["below_35"])

ggplot() +
  geom_sf(data=SACR_counties, aes(fill=below_35)) +
  scale_fill_distiller()

  
