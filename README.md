# Fuelbed reports

## Code to create html outputs showing the differences in the old and new FCCS fuelbed information for Federally-managed land units

Two key files in this repo for creating the reports: 

- `old_new_fccs_summary_report.Rmd`: this is the Rmd report template that crops the FCCS rasters and summarizes the fuelbeds within that particular unit.
- `render_old_new_fccs_summary_report.R`: this contains the functions to render the Rmd template for a given shapefile of Federally-managed land units

The GIS data needed for the report generation will be shared separately.