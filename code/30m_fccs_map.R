## SPATIAL
library(sf)
library(terra)

## DATA MANAGEMENT
library(tidyverse)
library(readxl)
library(kableExtra)
library(webshot)
library(magick)
library(htmltools)

##  function to create image of 30m fccs for PSAs  ----------------------------------------------
fccs_map_fun <- function(UNIT, REGION, AGENCY) {
  
  # load disturbance codes
  dist_codes <- read_csv("raw_data/DisturbanceCodes.csv") %>% 
    fill(Type) %>% 
    fill(Severity) %>%  
    select(dist_id = DistCode,
           type = Type) %>% 
    mutate(dist_id = as.character(dist_id))
  
  # create dataframe of available FCCS Raster data and filter to specified region
  ras_df <- data.frame(region = c("CONUS", "AK", "HI", "PR"),
                      path = c("gis/LF2022_FCCS_220_CONUS/LF2022_FCCS_220_CONUS/Tif/LC22_FCCS_220.tif",
                               "gis/LF2022_FCCS_220_AK/LF2022_FCCS_220_AK/Tif/LA22_FCCS_220.tif",
                               "gis/LF2022_FCCS_220_HI/Tif/LH22_FCCS_220.tif",
                               "gis/LF2023_PRVI_240_IA/LF2023_FCCS_240_PRVI/Tif/LV23_FCCS_240.tif")) %>% 
    filter(region == REGION)
  
  new_fac <- rast(ras_df$path)
  
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
  unit <- st_transform(unit_unproj, crs = crs(new_fac)) %>%
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
            paste0("federal_unit_reports_v2/",
                   AGENCY,
                   "/",
                   UNIT,
                   "_30m_fccs_fuelbed_area.csv"))
  
  
  # create new legend table
  new_leg_max <- ifelse(dim(new_sum_df)[1] <= 20, dim(new_sum_df)[1], 20)
  new_col_df <- new_sum_df %>% 
    rename(base_fccs = fccs) %>% 
    mutate(new_area_per = round(new_area_acres/sum(new_sum_df$new_area_acres)*100, digits = 0)) %>% 
    mutate(new_area_char = ifelse(new_area_per < 1, "<1", as.character(new_area_per))) %>% 
    arrange(desc(new_area_per)) %>% 
    left_join(., col_df) %>% 
    mutate(blank = rep("", n())) %>% 
    as.data.frame()
  
  new_col_leg <- new_col_df[1:new_leg_max, c("blank", "base_fccs", "new_area_char", "hex")]
  
  # text of other fuelbeds
  new_other_fccs <- if(dim(new_col_df)[1] > 20) {
    sort(as.numeric(new_col_df[c((new_leg_max + 1)):dim(new_col_df)[1], "base_fccs"]))} else{NULL}
  
  
  new_other_statement <- ifelse(is.null(new_other_fccs),
                                "", ifelse(length(new_other_fccs) == 1,
                                           paste0(paste0("Other fuelbeds include: "), 
                                                  paste0(new_other_fccs, ".")), 
                                           paste0(paste0("Other fuelbeds include: "), 
                                                  paste0(new_other_fccs[-length(new_other_fccs)],  collapse = ", "), 
                                                  paste0(" and ", new_other_fccs[length(new_other_fccs)], "."))))
  
  # create kable table for new legend
  new_leg <- kbl(new_col_leg[,1:3], booktabs = T, linesep = "",  col.names = linebreak(c(" ", "FCCS", "Area (%)")), align = "c", format = "html") %>%
    kable_styling(full_width = F) %>%
    kable_styling(font_size = 10, position = "center") %>% 
    kable_styling(latex_options = "hold_position") %>% 
    column_spec(1, color = "black", background = new_col_leg$hex, width = "0.5in") %>% 
    column_spec(2, width = "0.5in") %>%
    column_spec(3, width = "0.5in") %>%
    footnote(general = new_other_statement, general_title = "", threeparttable = TRUE)  
  
  
  
  # write image to file
  png(filename=paste0("temp_files/",
                      UNIT,
                      "_30m_fccs_map.png"),
      height = 1000,
      width = 1000)
  terra::plot(unit_new, 
              legend = FALSE, 
              axes = FALSE, 
              box = FALSE, 
              maxcell = 5e8, 
              oma=c(5,5,5,5),
              mar = c(2,2,2,2))
  plot(st_geometry(unit), add = TRUE)
  dev.off()
  
  # save kable to png
  save_kable(new_leg, 
             file = paste0("temp_files/",
                           UNIT,
                           "_30m_fccs_legend_test.png"),
             density = 1000,
             zoom = 1.3)
  
  # Combine side by side with magick
  map <- image_read(paste0("temp_files/",
                           UNIT,
                           "_30m_fccs_map.png"))
  
  legend <- image_read(paste0("temp_files/",
                              UNIT,
                              "_30m_fccs_legend_test.png"))
  
  combined <- image_append(c(map, legend))
  image_write(combined, paste0("federal_unit_reports_v2/",
                               AGENCY,
                               "/",
                               UNIT,
                               "_30m_fccs_map.png"))
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

pr_psa <- st_read("gis/National_Predictive_Service_Areas_Boundaries.geojson") %>% 
  filter(OBJECTID %in% c(seq(152,157,1))) %>% # HI
  st_drop_geometry() %>% 
  pull(OBJECTID)


# apply render function to create reports
lapply(as.character(conus_psa), function(x) fccs_map_fun(x, "CONUS", "PSA"))

lapply(as.character(ak_psa), function(x) fccs_map_fun(x, "AK","PSA"))

# lapply(as.character(hi_psa), function(x) fccs_map_fun(x, "HI", "PSA"))
fccs_map_fun("116", "HI", "PSA") # doesn't work
fccs_map_fun("112", "HI", "PSA") # doesn't work
fccs_map_fun("113", "HI", "PSA") 
fccs_map_fun("114", "HI", "PSA") # doesn't work
fccs_map_fun("115", "HI", "PSA") #doesn't work

lapply(as.character(pr_psa), function(x) fccs_map_fun(x, "PR", "PSA""))

