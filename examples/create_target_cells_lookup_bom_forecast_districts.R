
target_cells_lookup_bom_forecast_districts <- create_polygons_target_cells_lookup_from_single_shape_file(
  all_polygons_shape_file = '/mnt/projects/wine_australia/common/input/shapes/BoM-Forecast-Regions/Tasmanian_post_gfe_forecast_districts.shp',
  target_domains_files = c('TAS-10' = '/rdsi/private/climatefutures/WINE/output/TAS-10/CSIRO/MOHC-HadGEM2-CC/historical/r1i1p1/CSIRO-CCAM-r3355/v1/fx/orog/orog_TAS-10_MOHC-HadGEM2-CC_historical_r1i1p1_CSIRO-CCAM-r3355_v1_fx.nc',
                           'AUS-50' = '/rdsi/private/climatefutures/WINE/output/AUS-50/CSIRO/MOHC-HadGEM2-CC/historical/r1i1p1/CSIRO-CCAM-r3355/v1/fx/orog/orog_AUS-50_MOHC-HadGEM2-CC_historical_r1i1p1_CSIRO-CCAM-r3355_v1_fx.nc'),
  field_names = list(label = "NAME", code = "CODE"),
  preferred_domains = 'TAS-10'

)

saveRDS(target_cells_lookup_bom_forecast_districts, file.path('examples', 'target_cells_lookup_bom_forecast_districts.rds'))
