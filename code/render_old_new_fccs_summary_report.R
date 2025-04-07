# load libraries ----------------------------------------------

library(tidyverse)
library(sf)


# create directories to store reports ----------------------------------------------

dir.create("federal_unit_reports_v2")
dir.create("federal_unit_reports_v2/NPS")
dir.create("federal_unit_reports_v2/BLM")
dir.create("federal_unit_reports_v2/USFS")
dir.create("federal_unit_reports_v2/DoD")


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






