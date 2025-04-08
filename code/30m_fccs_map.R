## SPATIAL
library(sf)
library(terra)

## DATA MANAGEMENT
library(tidyverse)
library(readxl)
library(kableExtra)

# assign params to objects within rmd
UNIT <- params$UNIT
AGENCY <- params$AGENCY

# load distrubance codes
dist_codes <- read_csv("../raw_data/DisturbanceCodes.csv") %>% 
  fill(Type) %>% 
  fill(Severity) %>%  
  select(dist_id = DistCode,
         type = Type) %>% 
  mutate(dist_id = as.character(dist_id))

# read in new and old FCCS data
conus <- rast("../gis/LF2022_FCCS_220_CONUS/LF2022_FCCS_220_CONUS/Tif/LC22_FCCS_220.tif")
new_fac <- conus

old <- rast("../gis/old2_project_30m.tif")
colnames(levels(old)[[1]])[2] <- "base_fccs"

# aggregate new data to parent FCCS fuelbed and assign to new_fac
base_fccs <- cats(new_fac)[[1]] %>% 
  select(FCCS, FCCSID, R, G, B) %>% 
  rowwise() %>% 
  mutate(FCCSID = gsub("_", "-", FCCSID)) %>% 
  separate(FCCSID, into = c("base_fccs", "dist_id"), sep = "-") %>% 
  mutate(base_fccs = ifelse(base_fccs == "Fill", FCCS, base_fccs)) %>% 
  select(-dist_id) %>% 
  mutate(base_fccs = as.numeric(base_fccs)) %>% 
  arrange(base_fccs) %>% 
  mutate(base_fccs = as.character(base_fccs)) 

# create 
levels(new_fac) <- base_fccs
activeCat(new_fac) <- 1

# read in df of colors for all beds
col_df <- read_csv("../data/col_df.csv",
                   col_types = list("c", "c")) %>% 
  as.data.frame() 


# assign color values to both old and new data
coltab(new_fac) <- col_df
coltab(old) <- col_df

# create dataframe of available gis admin data and filter to specified agency
sh_ls <- data.frame(agency = c("USFS", "NPS", "DoD", "BLM", "PSA"),
                    path = c("../gis/S_USA.AdministrativeForest",
                             "../gis/nps_boundary",
                             "../gis/tl_2019_us_mil",
                             "../gis/BLM_Natl_NLCS_National_Monuments_National_Conservation_Areas_Polygons_2978908493776641245.geojson",
                             "../gis/National_Predictive_Service_Areas_Boundaries.geojson"),
                    unit_var = c("FORESTNAME", "UNIT_NAME", "FULLNAME", "NCA_NAME", "OBJECTID")) %>% 
  filter(agency == AGENCY)

# filter to AGENCY/UNIT
unit_unproj <- st_read(sh_ls$path) %>%
  filter(get(sh_ls$unit_var) == UNIT)
unit <- st_transform(unit_unproj, crs = crs(conus)) %>%
  mutate(ID = seq(1, n(), 1))
unit_area <- formatC(round(as.numeric(st_area(unit)*0.000247105), digits = 0), format = "d", big.mark = ",")

# crop old rasters with unit
unit_old_cr <- crop(old, unit, snap="out")                    
unit_old_fr <- rasterize(unit, unit_old_cr) 
unit_old <- mask(x=unit_old_cr, mask=unit_old_fr)


# crop new rasters with unit
unit_new_cr <- crop(new_fac, unit, snap="out")                    
unit_new_fr <- rasterize(unit, unit_new_cr) 
unit_new <- mask(x=unit_new_cr, mask=unit_new_fr)




png(filename=paste0("../federal_unit_reports_v2/",
                    AGENCY,
                    "/",
                    UNIT,
                    "_30m_fccs_map.png"))
terra::plot(unit_new, legend = FALSE, axes = FALSE, box = FALSE, maxcell = Inf, oma=c(0,0,0,0),mar = c(0,0,0,0))
plot(st_geometry(unit), add = TRUE)
dev.off()