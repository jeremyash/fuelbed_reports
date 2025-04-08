## SPATIAL
library(sf)
library(terra)

## DATA MANAGEMENT
library(tidyverse)
library(readxl)


##  function to create image of 30m fccs for PSAs  ----------------------------------------------

conus_fccs_map_fun <- function(AGENCY, UNIT) {
  
  # load disturbance codes
  dist_codes <- read_csv("raw_data/DisturbanceCodes.csv") %>% 
    fill(Type) %>% 
    fill(Severity) %>%  
    select(dist_id = DistCode,
           type = Type) %>% 
    mutate(dist_id = as.character(dist_id))
  
  # read in new and old FCCS data
  conus <- rast("gis/LF2022_FCCS_220_CONUS/LF2022_FCCS_220_CONUS/Tif/LC22_FCCS_220.tif")
  new_fac <- conus
  
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
  col_df <- read_csv("data/col_df.csv",
                     col_types = list("c", "c")) %>% 
    as.data.frame() 
  
  
  # assign color values to both old and new data
  coltab(new_fac) <- col_df
  
  # create dataframe of available gis admin data and filter to specified agency
  sh_ls <- data.frame(agency = c("USFS", "NPS", "DoD", "BLM", "PSA"),
                      path = c("gis/S_USA.AdministrativeForest",
                               "gis/nps_boundary",
                               "gis/tl_2019_us_mil",
                               "gis/BLM_Natl_NLCS_National_Monuments_National_Conservation_Areas_Polygons_2978908493776641245.geojson",
                               "gis/National_Predictive_Service_Areas_Boundaries.geojson"),
                      unit_var = c("FORESTNAME", "UNIT_NAME", "FULLNAME", "NCA_NAME", "OBJECTID")) %>% 
    filter(agency == AGENCY)
  
  # filter to AGENCY/UNIT
  unit_unproj <- st_read(sh_ls$path) %>%
    filter(get(sh_ls$unit_var) == UNIT)
  unit <- st_transform(unit_unproj, crs = crs(conus)) %>%
    mutate(ID = seq(1, n(), 1))
  unit_area <- formatC(round(as.numeric(st_area(unit)*0.000247105), digits = 0), format = "d", big.mark = ",")
  
  # crop new rasters with unit
  unit_new_cr <- crop(new_fac, unit, snap="out")                    
  unit_new_fr <- rasterize(unit, unit_new_cr) 
  unit_new <- mask(x=unit_new_cr, mask=unit_new_fr)
  
  
  
  # write image to file
  png(filename=paste0("federal_unit_reports_v2/",
                      AGENCY,
                      "/",
                      UNIT,
                      "_30m_fccs_map.png"))
  terra::plot(unit_new, legend = FALSE, axes = FALSE, box = FALSE, maxcell = Inf, oma=c(5,5,5,5),mar = c(2,2,2,2))
  plot(st_geometry(unit), add = TRUE)
  dev.off()
}



ak_fccs_map_fun <- function(AGENCY, UNIT) {
  
  # load disturbance codes
  dist_codes <- read_csv("raw_data/DisturbanceCodes.csv") %>% 
    fill(Type) %>% 
    fill(Severity) %>%  
    select(dist_id = DistCode,
           type = Type) %>% 
    mutate(dist_id = as.character(dist_id))
  
  # read in new and old FCCS data
  conus <- rast("gis/LF2022_FCCS_220_AK/LF2022_FCCS_220_AK/Tif/LA22_FCCS_220.tif")
  new_fac <- conus
  
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
  col_df <- read_csv("data/col_df.csv",
                     col_types = list("c", "c")) %>% 
    as.data.frame() 
  
  
  # assign color values to both old and new data
  coltab(new_fac) <- col_df
  
  # create dataframe of available gis admin data and filter to specified agency
  sh_ls <- data.frame(agency = c("USFS", "NPS", "DoD", "BLM", "PSA"),
                      path = c("gis/S_USA.AdministrativeForest",
                               "gis/nps_boundary",
                               "gis/tl_2019_us_mil",
                               "gis/BLM_Natl_NLCS_National_Monuments_National_Conservation_Areas_Polygons_2978908493776641245.geojson",
                               "gis/National_Predictive_Service_Areas_Boundaries.geojson"),
                      unit_var = c("FORESTNAME", "UNIT_NAME", "FULLNAME", "NCA_NAME", "OBJECTID")) %>% 
    filter(agency == AGENCY)
  
  # filter to AGENCY/UNIT
  unit_unproj <- st_read(sh_ls$path) %>%
    filter(get(sh_ls$unit_var) == UNIT)
  unit <- st_transform(unit_unproj, crs = crs(conus)) %>%
    mutate(ID = seq(1, n(), 1))
  unit_area <- formatC(round(as.numeric(st_area(unit)*0.000247105), digits = 0), format = "d", big.mark = ",")
  
  # crop new rasters with unit
  unit_new_cr <- crop(new_fac, unit, snap="out")                    
  unit_new_fr <- rasterize(unit, unit_new_cr) 
  unit_new <- mask(x=unit_new_cr, mask=unit_new_fr)
  
  
  
  # write image to file
  png(filename=paste0("federal_unit_reports_v2/",
                      AGENCY,
                      "/",
                      UNIT,
                      "_30m_fccs_map.png"))
  terra::plot(unit_new, legend = FALSE, axes = FALSE, box = FALSE, maxcell = Inf, oma=c(5,5,5,5),mar = c(2,2,2,2))
  plot(st_geometry(unit), add = TRUE)
  dev.off()
}


hi_fccs_map_fun <- function(AGENCY, UNIT) {
  
  # load disturbance codes
  dist_codes <- read_csv("raw_data/DisturbanceCodes.csv") %>% 
    fill(Type) %>% 
    fill(Severity) %>%  
    select(dist_id = DistCode,
           type = Type) %>% 
    mutate(dist_id = as.character(dist_id))
  
  # read in new and old FCCS data
  conus <- rast("gis/LF2022_FCCS_220_HI/Tif/LH22_FCCS_220.tif")
  new_fac <- conus
  
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
  col_df <- read_csv("data/col_df.csv",
                     col_types = list("c", "c")) %>% 
    as.data.frame() 
  
  
  # assign color values to both old and new data
  coltab(new_fac) <- col_df
  
  # create dataframe of available gis admin data and filter to specified agency
  sh_ls <- data.frame(agency = c("USFS", "NPS", "DoD", "BLM", "PSA"),
                      path = c("gis/S_USA.AdministrativeForest",
                               "gis/nps_boundary",
                               "gis/tl_2019_us_mil",
                               "gis/BLM_Natl_NLCS_National_Monuments_National_Conservation_Areas_Polygons_2978908493776641245.geojson",
                               "gis/National_Predictive_Service_Areas_Boundaries.geojson"),
                      unit_var = c("FORESTNAME", "UNIT_NAME", "FULLNAME", "NCA_NAME", "OBJECTID")) %>% 
    filter(agency == AGENCY)
  
  # filter to AGENCY/UNIT
  unit_unproj <- st_read(sh_ls$path) %>%
    filter(get(sh_ls$unit_var) == UNIT)
  unit <- st_transform(unit_unproj, crs = crs(conus)) %>%
    mutate(ID = seq(1, n(), 1))
  unit_area <- formatC(round(as.numeric(st_area(unit)*0.000247105), digits = 0), format = "d", big.mark = ",")
  
  # crop new rasters with unit
  unit_new_cr <- crop(new_fac, unit, snap="out")                    
  unit_new_fr <- rasterize(unit, unit_new_cr) 
  unit_new <- mask(x=unit_new_cr, mask=unit_new_fr)
  
  
  # sum data by fuelbed x dataset
  new_sum <- freq(unit_new)
  
  #calculate area for new fccs
  new_sum_df <- new_sum %>% 
    rename(fccs = value) %>% 
    group_by(fccs) %>% 
    summarise(new_count = sum(count, na.rm=TRUE)) %>% 
    ungroup() %>% 
    rowwise() %>% 
    mutate(new_area_acres = new_count*30*30*0.000247105)

  
  # write area to file
  new_fccs_area_df <- new_sum_df %>% 
    rename(base_fccs = fccs) %>% 
    mutate(new_area_per = round(new_area_acres/sum(new_sum_df$new_area_acres)*100, digits = 0)) %>% 
    mutate(new_area_char = ifelse(new_area_per < 1, "<1", as.character(new_area_per))) %>% 
    arrange(desc(new_area_per)) %>% 
    select(base_fccs,
           area_percentage = new_area_char)
  
  write_csv(new_fccs_area_df,
            paste0("../federal_unit_reports_v2/",
                   AGENCY,
                   "/",
                   UNIT,
                   "_30m_fccs_fuelbed_area.csv"))
  
  
  # write image to file
  png(filename=paste0("federal_unit_reports_v2/",
                      AGENCY,
                      "/",
                      UNIT,
                      "_30m_fccs_map.png"))
  terra::plot(unit_new, legend = FALSE, axes = FALSE, box = FALSE, maxcell = Inf, oma=c(5,5,5,5),mar = c(2,2,2,2))
  plot(st_geometry(unit), add = TRUE)
  dev.off()
}

## apply functions to PSAs ----------------------------------------------

# read in PSA geojson and filter out polygons not in CONUS
conus_psa <- st_read("gis/National_Predictive_Service_Areas_Boundaries.geojson") %>% 
  filter(!(OBJECTID %in% c(seq(152,157,1)))) %>% # Puerto Rico
  filter(!(OBJECTID %in% c(seq(112,116,1)))) %>% # HI
  filter(!(GACCName == "Alaska Interagency Coordination Center")) %>% # Alaska
  st_drop_geometry() %>% 
  pull(OBJECTID)

ak_psa <- st_read("gis/National_Predictive_Service_Areas_Boundaries.geojson") %>% 
  filter(GACCName == "Alaska Interagency Coordination Center") %>% # Alaska
  st_drop_geometry() %>% 
  pull(OBJECTID)

hi_psa <- st_read("gis/National_Predictive_Service_Areas_Boundaries.geojson") %>% 
  filter(OBJECTID %in% c(seq(112,116,1))) %>% # HI
  st_drop_geometry() %>% 
  pull(OBJECTID)


# apply render function to create reports
lapply(conus_psa, function(x) conus_fccs_map_fun(x, "PSA"))
lapply(ak_psa, function(x) ak_fccs_map_fun(x, "PSA"))
lapply(hi_psa, function(x) hi_fccs_map_fun(x, "PSA"))
