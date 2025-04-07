# load libraries ----------------------------------------------

library(tidyverse)
library(sf)


# create directories ----------------------------------------------

# dir.create("federal_unit_reports_v2")
# dir.create("federal_unit_reports_v2/NPS")
# dir.create("federal_unit_reports_v2/BLM")
# dir.create("federal_unit_reports_v2/USFS")
# dir.create("federal_unit_reports_v2/DoD")
# # dir.create("reports_v2/FWS")



# function to create smoke report ----------------------------------------------

render_flm_report <- function(UNIT, AGENCY){
  # libraries needed
  require(rmarkdown)
  require(tidyverse)
  require(lubridate)
  
  # intermediate file names
  report_title <- str_replace_all(UNIT, "/", " ")
  report_title <- str_replace_all(report_title, " ", "_")
  
  # create html output
  render(input = "code/flm_summary.Rmd",
         output_dir = paste0("federal_unit_reports_v2/", AGENCY),
         output_file = paste0(report_title, "_FCCS_2024.html"), 
         params = list(UNIT = UNIT, 
                       AGENCY = AGENCY),
         envir = new.env())
  
}


# render for USFS ----------------------------------------------

nfs <- st_read("gis/S_USA.AdministrativeForest") %>% 
  filter(!(FORESTNAME %in% c("Tongass National Forest", "El Yunque National Forest", "Chugach National Forest"))) %>% 
  # filter(REGION %in% c("06")) %>% 
  st_drop_geometry()

nfs_names <- nfs$FORESTNAME
lapply(nfs_names, function(x) render_flm_report(x, "USFS"))


nfs_names[63]
remaining_nfs <- nfs_names[63:109]
lapply(remaining_nfs, function(x) render_flm_report(x, "USFS"))



# render for NPS ----------------------------------------------

nps <- st_read("gis/nps_boundary") %>% 
  filter(!(STATE %in% c("HI", "GU", "AK", "MP", "PR"))) %>% 
  filter(UNIT_TYPE %in% c("National Park")) %>% 
  st_drop_geometry()
  
nps_names <- nps$UNIT_NAME

lapply(nps_names, function(x) render_flm_report(x, "NPS"))

nps_names[23]

remaining_nps <- nps_names[24:52]

lapply(remaining_nps, function(x) render_flm_report(x, "NPS"))

remaining_nps[25]

remaining_nps_again <- remaining_nps[26:29]

lapply(remaining_nps_again, function(x) render_flm_report(x, "NPS"))



# render for DoD ----------------------------------------------

states <- st_read("../fs_admin/gis/states") %>% 
  filter(!(STATE_NAME %in% c("Hawaii", "Alaska")))

dod <- st_read("gis/tl_2019_us_mil") %>% 
  mutate(area = st_area(dod)) 

dod_states <- st_intersection(dod, states)
  # filter(FULLNAME %in% c("Eglin AFB", "Ft Stewart", "Ft Benning")) %>% 
  # st_drop_geometry()

dod_names <- dod_states$FULLNAME  
lapply(dod_names, function(x) render_flm_report(x, "DoD"))

dod_remaining <- dod_names[330:727]
lapply(dod_remaining, function(x) render_flm_report(x, "DoD"))



# render for BLM ----------------------------------------------
blm <- st_read("gis/BLM_Natl_NLCS_National_Monuments_National_Conservation_Areas_Polygons_2978908493776641245.geojson")

blm_names <- blm$NCA_NAME
lapply(blm_names, function(x) render_flm_report(x, "BLM"))

blm_names[31]

remaining_blm <- blm_names[32:56]

lapply(remaining_blm, function(x) render_flm_report(x, "BLM"))

# ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# # FWS
# 
# 
# states <- st_read("../fs_admin/gis/states") %>% 
#   filter(!(STATE_NAME %in% c("Hawaii", "Alaska")))
# 
# fws <- st_read("gis/FWSInterest_Simplified") 
# fws <- st_transform(fws, crs = st_crs(states)) %>% 
#   filter(str_detect(ORGNAME, "NATIONAL WILDLIFE REFUGE"))
# 
# sf_use_s2(FALSE)
# fws_states <- st_intersection(fws, states)
# # filter(FULLNAME %in% c("Eglin AFB", "Ft Stewart", "Ft Benning")) %>% 
# # st_drop_geometry()



