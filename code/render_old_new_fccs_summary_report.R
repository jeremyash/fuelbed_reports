# load libraries ----------------------------------------------

library(tidyverse)
library(sf)


# create directories to store reports ----------------------------------------------

dir.create("federal_unit_reports_v2")
dir.create("federal_unit_reports_v2/NPS")
dir.create("federal_unit_reports_v2/BLM")
dir.create("federal_unit_reports_v2/USFS")
dir.create("federal_unit_reports_v2/DoD")
dir.create("federal_unit_reports_v2/PSA")

# function to create report ----------------------------------------------

render_flm_report <- function(UNIT, AGENCY){
  # libraries needed
  require(rmarkdown)
  require(tidyverse)
  require(lubridate)
  
  # intermediate file names
  report_title <- str_replace_all(UNIT, "/", " ")
  report_title <- str_replace_all(report_title, " ", "_")
  
  # create html output
  render(input = "code/old_new_fccs_summary_report.Rmd",
         output_dir = paste0("federal_unit_reports_v2/", AGENCY),
         output_file = paste0(report_title, "_FCCS_2024.html"), 
         params = list(UNIT = UNIT, 
                       AGENCY = AGENCY),
         envir = new.env())
  
}

ak_render_flm_report <- function(UNIT, AGENCY){
  # libraries needed
  require(rmarkdown)
  require(tidyverse)
  require(lubridate)
  
  # intermediate file names
  report_title <- str_replace_all(UNIT, "/", " ")
  report_title <- str_replace_all(report_title, " ", "_")
  
  # create html output
  render(input = "code/ak_old_new_fccs_summary_report.Rmd",
         output_dir = paste0("federal_unit_reports_v2/", AGENCY),
         output_file = paste0(report_title, "_FCCS_2024.html"), 
         params = list(UNIT = UNIT, 
                       AGENCY = AGENCY),
         envir = new.env())
  
}


# render for USFS ----------------------------------------------

# read in NFS shapefile and filter out forests not in CONUS
nfs <- st_read("gis/S_USA.AdministrativeForest") %>% 
  filter(!(FORESTNAME %in% c("Tongass National Forest", 
                             "El Yunque National Forest", 
                             "Chugach National Forest"))) %>% 
  st_drop_geometry()

# create vector of forest names
nfs_names <- nfs$FORESTNAME

# apply render function to create reports
lapply(nfs_names, function(x) render_flm_report(x, "USFS"))


# render for PSA ----------------------------------------------

# read in PSA geojson and filter out polygons not in CONUS
conus_psa <- st_read("gis/National_Predictive_Service_Areas_Boundaries.geojson") %>% 
  filter(!(OBJECTID %in% c(seq(152,157,1)))) %>% # Puerto Rico
  filter(!(OBJECTID %in% c(seq(112,116,1)))) %>% # HI
  filter(!(GACCName == "Alaska Interagency Coordination Center")) %>% # Alaska
  st_drop_geometry() %>% 
  pull(OBJECTID)

# apply render function to create reports
lapply(conus_psa, function(x) render_flm_report(x, "PSA"))




# read in PSA geojson and filter out polygons in AK
ak_psa <- st_read("gis/National_Predictive_Service_Areas_Boundaries.geojson") %>% 
  filter(GACCName == "Alaska Interagency Coordination Center") %>% # Alaska
  st_drop_geometry() %>% 
  pull(OBJECTID)

# apply render function to create reports
lapply(ak_psa, function(x) ak_render_flm_report(x, "PSA"))






ak_render_flm_report(80, "PSA")
















