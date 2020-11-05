# Info --------------------------------------------------------------------

# How to navigate this file
# About each section
# Mention coding conventions or location of other document



#'
#' *TODO*
#'



# Spatial Functions -------------------------------------------------------

# Functions that perform common and/or complex spatial operations

#' Get all matching cells for a polygon simple feature
#'
#' Find the cells covered by the area of the polygon of an sf object in each of a list of domains
#'
#' @param polygon_sf A simple feature (sf) object representing the polygon. TODO: describe required fields (e.g. index, code, label)
#' @inheritParams convert_all_polygons_to_multipolygons
#' @inheritParams common_parameter_descriptions
#'
#'
#' @return A named list with the domains as the keys, and the values being vector of the matching cell numbers
#' @export
find_matching_cells_in_target_domains_for_polygon_sf <- function(polygon_sf, target_domains_files) {

  out_cells <- list()
  for (domain in names(target_domains_files)) {
    # domain <- names(target_domains_files)[1]
    logdebug(glue("Find matching cells in domain: {domain}"))
    geospatial_array_filename <- target_domains_files[[domain]]
    geospatial_array <- raster::raster(geospatial_array_filename, layer=1)

    ccam_proj <- raster::projection(geospatial_array)
    shape_transformed_to_ccam_proj <- sf::st_transform(x = polygon_sf, crs = ccam_proj)
    geospatial_feature_spdf <- as(shape_transformed_to_ccam_proj, 'Spatial')

    cells_with_weights <- find_matching_cells_for_polygon_from_spatial_objects(geospatial_feature_spdf, geospatial_array)

    out_cells[[domain]] <- cells_with_weights
  }

  out_cells
}


#' Find the cells in a spatial array covered by a spatial object
#'
#' The geospatial feature
#' The geospatial array can be produced from a raster of a NetCDF file.
#'
#' It is important that both parameters have the same projection.
#'
#' TODO:
#' explain the usage of cellFromPolygon with weights = TRUE and why that is important
#' allow a cutoff to be passed for the weights
#'
#'
#' @param geospatial_feature_spdf SpatialPolygons object
#' @param geospatial_array Raster\* object (or a SpatialPixels\* or SpatialGrid\* object)
#'
#' @return Vector of cells
#' @export
#'
#' @examples
#' shape_file <- '/path/to/shapes/region.shp'
#' array_file <- '/path/to/source/data/tasmax_monthly_1960-1969.nc'
#' matching_cells <- find_matching_cells_for_polygon_from_spatial_objects(shape_file, array_file)
find_matching_cells_for_polygon_from_spatial_objects <- function(geospatial_feature_spdf, geospatial_array) {
  # if the spdf has multiple polygons then there will be multiple items in the cell_weights list
  # otherwise, there will just be [[1]]
  cell_weights <- cellFromPolygon(geospatial_array, geospatial_feature_spdf, weights = TRUE)

  # if there are no cells for this shape in this domain (i.e. when creating a large lookup with different regions ins domains that may not intersect)
  # then the above result will be NULL (which needs to be handled)
  #
  # otherwise return all the results with weights

  if (is.null(unlist(cell_weights))) {
    NA
  } else {
    cells_with_weights <- lapply(cell_weights, function(e) {
      tibble(cell = e[,1], weight = e[,2])
    }) %>% bind_rows

    cells_with_weights
  }


  # # this is how it was done when not returning the weights:
  # # get all cells in this shape file (each file could have multiple polygons, stored as items in a list)
  # cells <- unlist(lapply(cell_weights, function(i) i[, 'cell']))
  #
  # # function returns NA if no cells found in domain for this shape
  # # otherwise, returns all the cells sorted in order
  # output <- if (is.null(cells)) { NA } else { sort(cells) }
  #
  # return(output)
}


#' Convert all geometries of a simple feature collection to a type of MULTIPOLYGON
#'
#' Converts a simple feature collection, where each feature has multiple polygons making up its geometry,
#' to a list of simple features only having one MULTIPOLYGON object making up its geometry.
#'
#' The field list points to the columns in the simple feature that refer to:
#' * index: the unique index (usually OBJECTID)
#' * label: the label for each feature
#'
#' And to be generated, *TODO if it does not exist TODO*:
#' * code: a unique code for each feature (this will be derived from the label)
#'
#' In most cases, the default field names of ('OBJECTID', 'CODE', 'LABEL') will be incorrect
#'
#' @param polygons_all_sf A simple feature collection
#' @param field_names A named list with the following names: 'index', 'label', and 'code'
#'
#' @return A list of simple features
#' @export
convert_all_polygons_to_multipolygons <- function(polygons_all_sf, field_names = list(index = 'OBJECTID', label = 'LABEL', code = 'CODE')) {
  # this could surely be totally optimised... but this works ok

  field_name_index <- field_names[['index']]
  field_name_code <- field_names[['code']]
  field_name_label <- field_names[['label']]

  # depending if field_name_index is provided or not
  # some grouping operations need to be handled differently
  if (is.null(field_name_index)) {
    group_by_vars <- vars(field_name_label)
    group_by_vars_with_code <- vars(field_name_label, field_name_code)
  } else {
    group_by_vars <- vars(field_name_index, field_name_label)
    group_by_vars_with_code <- vars(field_name_index, field_name_label, field_name_code)
  }

  # metadata for the polygons (without the geometry)
  polygons_meta <- polygons_all_sf %>%
    group_by_at(group_by_vars) %>%
    summarise() %>%
    mutate(!!as.character(field_name_code) := str_replace_all(str_to_lower(!!as.name(field_name_label)), pattern = ' |-|/', replacement = '_')) %>%
    ungroup()
  st_geometry(polygons_meta) <- NULL

  # build a list of polygons
  tmp_polygons <- vector("list")

  num_polygons <- dim(polygons_meta)[1]
  for(i in 1:num_polygons) {
    # i <- 1
    region <- polygons_meta[i,]
    region_label <- region[[field_name_label]]
    logdebug(glue("({i}/{num_polygons}) {region_label}"))

    # if no index column, then use the loop index
    if (is.null(field_name_index)) {
      region_catch_no <- i
      region_sf <- polygons_all_sf %>% filter(!!as.name(field_name_label) == region_label)

      region_row <- region_sf[c(field_name_index, field_name_label)] %>%
        mutate(!!as.character(field_name_code) := str_replace_all(str_to_lower(!!as.name(field_name_label)), pattern = ' |-|/', replacement = '_'))
    } else {
      region_catch_no <- region[[field_name_index]]
      region_sf <- polygons_all_sf %>% filter(!!as.name(field_name_index) == region[[field_name_index]])

      region_row <- region_sf[c(field_name_index, field_name_label)] %>%
        mutate(!!as.character(field_name_code) := str_replace_all(str_to_lower(!!as.name(field_name_label)), pattern = ' |-|/', replacement = '_'))
    }


    # combine list of geometry objects into multi polygon
    multi <- st_combine(region_row[['geometry']])

    # add the code to the group by vars
    output <- region_row %>%
      group_by_at(group_by_vars_with_code) %>%
      summarise()

    # convert from factor to label
    output[[field_name_label]] <- as.character(output[[field_name_label]])

    # save new geometry
    output$geometry <- multi

    tmp_polygons[[i]] <- st_sf(output)
  }

  lapply(tmp_polygons, st_cast, "MULTIPOLYGON")
}



# Spatial Visualisation Functions -----------------------------------------

# Functions to visualise the spatial properties of the data within lookups

#' Generate a suite of spatial coverage plots
#'
#' Creates a spatial coverage plot for all of the entities in a \code{target_cells_lookup} list.
#' Cell boundaries plots are always created. Depending on the presence/value of the \code{bg_map_sf}
#' and \code{cell_numbers} parameters, plots on a background map, and plots with cell numbers can also be created.
#'
#' These plots are useful for visualising the extent of each region. An example output structure, if "verify_spatial_coverage" is the last subdirectory in the
#' \code{spatial_coverage_figures_directory} paramater and
#' \code{bg_map_sf} is supplied and \code{cell_numbers} is TRUE, would be:
#'
#' \preformatted{
#' - verify_spatial_coverage
#'   - cell_boundaries
#'     - region_1.png
#'     - region_2.png
#'   - cell_nummbers
#'     - region_1.png
#'     - region_2.png
#'   - cells_on_bg_map
#'     - region_1.png
#'     - region_2.png}
#'
#' @inheritParams common_parameter_descriptions
#' @param spatial_coverage_figures_directory The base directory for spatial coverage plots to be saved. The base set of plots will be placed in the \code{cell_boundaries} subdirectory.
#' @param bg_map_sf If provided, then a set of spatial coverage plots will be created with this sf object as the background. These will be placed in the \code{cell_on_bg_map} subdirectory.
#' @param cell_numbers If TRUE, then a set of spatial coverage plots will be created with the cell numbers labelled. These will be placed in the \code{cell_nummbers} subdirectory.
#' @param domain Use a specific domain for the plots, otherwise the preferred domain will be used (or if only one domain is present, then that domain will be used)
#'
#' @export
generate_spatial_coverage_plots_for_all_entities_in_lookup <- function(target_cells_lookup, cell_orog_lookup, spatial_coverage_figures_directory,
                                                                       bg_map_sf = NULL, cell_numbers = FALSE, domain = NULL) {

  ensure_dir_exists(spatial_coverage_figures_directory)

  if (is.null(domain)) {
    domain <- lookup_domain_for_region(target_cells_lookup, region)
  }

  for (target_region in target_cells_lookup) {
    # target_region <- target_cells_lookup[[1]]
    target_region_code <- target_region[['code']]

    # plot the cell boundaries of each region
    {
      loginfo(glue("Plotting spatial coverage plot for {target_region_code}"))

      gg_cells <- draw_spatial_coverage_plot_for_region_in_domain(target_cells_lookup = target_cells_lookup,
                                                                  cell_orog_lookup = cell_orog_lookup,
                                                                  region = target_region_code,
                                                                  show_cell_labels = FALSE,
                                                                  domain = domain)

      save_png(gg_cells,
               file.path(spatial_coverage_figures_directory, 'cell_boundaries', glue("{target_region_code}.png")))
    }

    # if cell_numbers is true, then create a folder with the
    if (cell_numbers) {
      loginfo(glue("Plotting spatial coverage plot for {target_region_code} with cell numbers"))

      gg_cell_numbers <- draw_spatial_coverage_plot_for_region_in_domain(target_cells_lookup = target_cells_lookup,
                                                                         cell_orog_lookup = cell_orog_lookup,
                                                                         region = target_region_code,
                                                                         show_cell_labels = TRUE,
                                                                         domain = domain)

      save_png(gg_cell_numbers,
               file.path(spatial_coverage_figures_directory, 'cell_numbers', glue("{target_region_code}.png")))
    }

    # if a background map sf is provided, then plot the spatial coverage all of the entities on that as well
    if (!is.null(bg_map_sf)) {
      loginfo(glue("Plotting spatial coverage plot for {target_region_code} on background map"))

      gg_map <- draw_spatial_coverage_plot_for_region_in_domain(target_cells_lookup = target_cells_lookup,
                                                                cell_orog_lookup = cell_orog_lookup,
                                                                region = target_region_code,
                                                                bg_map_sf = bg_map_sf,
                                                                show_cell_labels = FALSE,
                                                                domain = domain)

      save_png(gg_map,
               file.path(spatial_coverage_figures_directory, 'cell_on_bg_map', glue("{target_region_code}.png")))
    }
  }
}


#' Draw a spatial coverage plot for a region on a specific domain
#'
#' This plot uses the matching cells for specified domain.
#'
#' @inheritParams common_parameter_descriptions
#' @param region Region to plot (either index in the list or region code, e.g. 37 or 'mudgee')
#' @param domain Domain to plot region and cells on
#' @param bg_map_sf An optional simple feature object to plot as a background map
#' @param show_cell_labels logical. If TRUE then shows the labels for the cell numbers. Set to FALSE by default.
#'
#' @return ggplot object for the plot
#' @export
#'
#' @examples
#' target_cells_lookup <- readRDS('/path/to/lookup_file.rds')
#' cell_orog_lookup <- readRDS('/path/to/orog_file.rds')
#'
#' draw_spatial_coverage_plot_for_region_in_domain(target_cells_lookup, cell_orog_lookup, region = 'glenrowan', domain = 'VIC-5')
#' draw_spatial_coverage_plot_for_region_in_domain(target_cells_lookup, cell_orog_lookup, region = 'glenrowan', domain = 'SEA-10')
draw_spatial_coverage_plot_for_region_in_domain <- function(target_cells_lookup, cell_orog_lookup, region, domain, bg_map_sf = NULL, show_cell_labels = FALSE) {
  plot_cell_points <- get_cells_from_orog_lookup_for_region_domain(target_cells_lookup, cell_orog_lookup, region, domain)
  draw_spatial_coverage_plot(target_cells_lookup, plot_cell_points, region, domain, bg_map_sf = bg_map_sf, show_cell_labels = show_cell_labels)
}


#' Draw a spatial coverage plot for a region
#'
#' This plot uses the matching cells for the preferred domain for the region (as per \code{target_cells_lookup}).
#'
#' @inheritParams draw_spatial_coverage_plot_for_region_in_domain
#'
#' @return ggplot object for the plot
#' @export
#'
#' @examples
#' target_cells_lookup <- readRDS('/path/to/lookup_file.rds')
#' cell_orog_lookup <- readRDS('/path/to/orog_file.rds')
#'
#' draw_spatial_coverage_plot_for_region(target_cells_lookup, cell_orog_lookup, region = 'barossa_valley')
#' draw_spatial_coverage_plot_for_region(target_cells_lookup, cell_orog_lookup, region = 'barossa_valley', show_cell_labels = TRUE)
draw_spatial_coverage_plot_for_region <- function(target_cells_lookup, cell_orog_lookup, region, bg_map_sf = NULL, show_cell_labels = FALSE) {
  domain <- lookup_domain_for_region(target_cells_lookup, region)
  draw_spatial_coverage_plot_for_region_in_domain(target_cells_lookup, cell_orog_lookup, region, domain, bg_map_sf = bg_map_sf, show_cell_labels = show_cell_labels)
}


#' Draw a spatial coverage plot for a region on a specific domain with the specified cell numbers
#'
#' This plot uses the specified cells for the specified domain
#'
#' @inheritParams draw_spatial_coverage_plot_for_region_in_domain
#' @param target_cells Vector of cells in the domain to draw on the plot of region
#'
#' @return ggplot object for the plot
#' @export
#'
#' @examples
#' target_cells_lookup <- readRDS('/path/to/lookup_file.rds')
#' cell_orog_lookup <- readRDS('/path/to/orog_file.rds')
#'
#' all_cells <- lookup_cells_for_region_with_domain(target_cells_lookup, 'glenrowan', 'VIC-5')
#' not_all_cells <- all_cells[1:(length(all_cells) %/% 2)]
#'
#' draw_spatial_coverage_plot_for_region_in_domain_with_cell_numbers(target_cells_lookup, cell_orog_lookup, region = 'glenrowan', domain = 'VIC-5', target_cells = not_all_cells, show_cell_labels = TRUE)

draw_spatial_coverage_plot_for_region_in_domain_with_cell_numbers <- function(target_cells_lookup, cell_orog_lookup, region, domain, target_cells, show_cell_labels = FALSE) {
  plot_cell_points <- get_cells_from_orog_lookup(cell_orog_lookup, target_cells, domain)
  draw_spatial_coverage_plot(target_cells_lookup, plot_cell_points, region, domain, show_cell_labels = show_cell_labels)
}


#' Draw a spatial coverage plot
#'
#' @inheritParams common_parameter_descriptions
#' @inheritParams draw_spatial_coverage_plot_for_region_in_domain
#' @param plot_cell_points Data frame with \code{lon} and \code{lat} values for each \code{cell_number} to plot
#' @param region Region (will be shown on the plot)
#' @param domain Domain (will be shown on the plot)
#'
#' @return ggplot object for the plot
#' @export
#'
#' @examples
#' target_cells_lookup <- readRDS('/path/to/lookup_file.rds')
#' cell_orog_lookup <- readRDS('/path/to/orog_file.rds')
#' region_code <- 'glenrowan'
#' domain_code <- 'VIC-5'
#'
#' plot_cell_points <- get_cells_from_orog_lookup_for_region_domain(target_cells_lookup, cell_orog_lookup, region = region_code, domain = domain_code)
#'
#' draw_spatial_coverage_plot(target_cells_lookup, plot_cell_points, region = region_code, domain = domain_code, show_cell_labels = TRUE)
draw_spatial_coverage_plot <- function(target_cells_lookup, plot_cell_points, region, domain, bg_map_sf = NULL,
                                       show_cell_labels = FALSE,
                                       proj4string = common_defaults$proj4string) {

  sp_polys <- convert_cells_to_lat_lon_polys(plot_cell_points)
  sf_cell_boxes <- st_as_sf(sp_polys)
  plot_cell_boxes <- st_transform(sf_cell_boxes, crs = proj4string)

  # the polygon for this region is either on disk at the location of 'shp_file'
  # and/or stored in the lookup as 'sf' as a sf
  if (is.null(target_cells_lookup[[region]]$sf)) {
    # no embedded sf
    plot_region_poly_file <- target_cells_lookup[[region]]$shp_file

    if (str_length(plot_region_poly_file) == 0) {
      # no poly file location either
      ##bad
      stop(glue("No embedded sf or original polygon file for {region}"))
    } else {
      sf_region <- read_sf(plot_region_poly_file)
      plot_region_poly <- st_transform(sf_region, crs = proj4string)
    }
  } else {
    # embedded sf
    plot_region_poly <- st_transform(target_cells_lookup[[region]]$sf, crs = proj4string)
  }

  region_label <- target_cells_lookup[[region]][['label']]
  num_cells <- nrow(plot_cell_points)

  g <- ggplot()

  if (!is.null(bg_map_sf)) {
    g <- g +
      geom_sf(data = bg_map_sf) +
      theme(panel.background = element_rect(fill = '#D6EBF2'))
  }

  g <- g +
    geom_sf(data = plot_region_poly) +
    geom_sf(data = plot_cell_boxes, colour="brown", alpha = 0.1) +
    theme(plot.title = element_text(hjust = 0.5),
          plot.subtitle = element_text(hjust = 0.5),
          panel.grid.major = element_line(colour = "grey80"),
          panel.grid.minor = element_line(colour = "grey80")) +

    scale_x_continuous(name = 'lon') +
    scale_y_continuous(name = 'lat') +

    ggtitle(label = glue("{region_label} Spatial Coverage"),
            subtitle = glue("Domain: {domain} ({num_cells} cell{if(num_cells==1){''}else{'s'}})"))

  if (show_cell_labels) {
    # default text_size
    text_size <- 2.2

    # scale the text size if there are a large number of cells
    num_cells <- dim(plot_cell_points)[1]
    if (num_cells > 750)  { text_size <- 2.0 }
    if (num_cells > 1000) { text_size <- 1.8 }
    if (num_cells > 2000) { text_size <- 1.5 }

    g <- g +
      # place cells in the bottom right corner
      geom_text(data = plot_cell_points,
                mapping = aes(x = lon + 0.5*resolution_lon_km - 0.025*resolution_lon_km,
                              y = lat - 0.5*resolution_lat_km + 0.025*resolution_lat_km,
                              label = as.character(round(cell_number))),
                colour = 'grey50',
                size = text_size, hjust = 1, vjust = 0)
  }

  g
}


# # draw a map and cells side by side as an example for a report
#
# comb <- cowplot::plot_grid(
#   ggplot() +
#     ggtitle('Lake Pieman Spatial Coverage',
#             'Domain: TAS-10 (25 cells)') +
#     theme(plot.margin = unit(c(1,1,0,1), "cm")) +
#     theme(plot.title = element_text(hjust = 0.5),
#           plot.subtitle = element_text(hjust = 0.5),
#           plot.background = element_blank()),
#   cowplot::plot_grid(
#     gg_map + theme(text=element_text(size=9.5)) + ggtitle(NULL, NULL),
#     gg_cell_numbers + theme(text=element_text(size=9.5)) + ggtitle(NULL, NULL),
#     ncol = 2
#   ),
#   ncol = 1,
#   rel_heights = c(1.2, 10)
# )
#
#
# save_png_a4(comb, res = 200,
#             file.path(spatial_coverage_figures_directory, 'doc', glue("{target_region_code}.png")))


#' Convert cell orography information to spatial polygons
#'
#' Takes a list of cells from a cell orography lookup and returns a list of SpatialPolygons of the dimensions of each cell
#'
#' Useful to overlay cell boundaries on a plot.
#'
#' @inheritParams common_parameter_descriptions
#' @param cells Dataframe of cells to return polygons for, taken from a cell orography lookup
#'
#' @return SpatialPolygons object with polygons for all of the cells
#' @export
#'
#' @examples
#' cell_orog_lookup <- readRDS('/path/to/orog_file.rds')
#' plot_cell_points <- cell_orog_lookup %>% filter(show_this_plot == TRUE)
#' sp_polys <- convert_cells_to_lat_lon_polys(plot_cell_points)
convert_cells_to_lat_lon_polys <- function(plot_cell_points, proj4string = common_defaults$proj4string) {

  # get the cells as spatial polygons
  polys <- list()
  num_cells <- dim(plot_cell_points)[1]

  # calculate offset from centre of cell to edge, to calculate outer points
  # assume that all cells are the same resolution
  offset_lon <- 0.5 * first(plot_cell_points$resolution_lon_km)
  offset_lat <- 0.5 * first(plot_cell_points$resolution_lat_km)

  for(i in 1:num_cells) {
    cell <- plot_cell_points[i,]

    coords = matrix(c(cell$lon - offset_lon, cell$lat - offset_lat,
                      cell$lon - offset_lon, cell$lat + offset_lat,
                      cell$lon + offset_lon, cell$lat + offset_lat,
                      cell$lon + offset_lon, cell$lat - offset_lat,
                      cell$lon - offset_lon, cell$lat - offset_lat),
                    ncol = 2, byrow = TRUE)

    p = Polygon(coords)

    polys[[i]] <- p
  }

  sp = SpatialPolygons(list(Polygons(polys, ID = "a")), proj4string=CRS(proj4string))

  return(sp)
}



# Create Lookup Functions -------------------------------------------------

#' Create cell orography lookup for a domain from a file
#'
#' @inheritParams common_parameter_descriptions
#' @param domain Name of domain to create orography lookup for
#' @param file Raster file (e.g. NetCDF file) to use as source
#'
#' @return Dataframe with \code{lat} and \code{lon} values for each \code{cell_number}, and \code{surface_height} if available
#' @export
#'
#' @examples
#' nc_orog_file <- '/path/to/orog_file.nc'
#' create_cell_orography_lookup('DOMAIN-10', nc_orog_file)
create_cell_orography_lookup <- function(domain, file, surface_height_column = 'Surface.height', proj4string = common_defaults$proj4string) {

  # use the NetCDF file to get the lat, lon, surface height data
  r <- raster::raster(file)

  # get the coordinates of the cell centres and create a Spatial object
  spts_val <- raster::rasterToPoints(r, spatial = TRUE)

  # create a mask of all values, so as to include NA values if they are present
  r_mask <- r
  values(r_mask) <- -999
  spts_all <- raster::rasterToPoints(r_mask, spatial = TRUE)

  # transform the points to desired projection
  llprj <- proj4string
  llpts_val <- spTransform(spts_val, CRS(llprj))
  llpts_all <- spTransform(spts_all, CRS(llprj))

  # build output data frame
  x_val <- as.data.frame(llpts_val)
  x_all <- as.data.frame(llpts_all)
  x_all$cell <- as.integer(rownames(x_all))

  # join to get the actual values and incorporate in the df will all points
  x <- left_join(x_all, x_val, by = c('x', 'y'), suffix = c('.mask', '.actual')) %>%
    # remove the mask values (they have realised their purpose)
    # and restore the actual values (NA values may be present)
    dplyr::select(-contains('.mask')) %>%
    dplyr::rename_with(.fn = ~ str_remove(.x, '\\.actual'),
                       .cols = contains('.actual'))

  resx <- round(xres(r), digits = 4)
  resy <- round(yres(r), digits = 4)

  # if the NetCDF file is an actual orography file, then we can include the surface height
  # otherwise, set it to NA
  if (!is.null(x[[surface_height_column]])) {
    tibble(
      domain = domain,
      cell_number = x$cell,
      resolution_lon_km = resx,
      resolution_lat_km = resy,
      lon = x$x,
      lat = x$y,
      surface_height = x[[surface_height_column]]
    )
  } else {
    tibble(
      domain = domain,
      cell_number = x$cell,
      resolution_lon_km = resx,
      resolution_lat_km = resy,
      lon = x$x,
      lat = x$y,
      surface_height = NA
    )
  }
}


#' Create cell orography lookup for a list of domains
#'
#' Takes a named list of domains with example NetCDF files and passes each to \code{create_cell_orography_lookup} and binds the entire collection to a single dataframe
#'
#' @inheritParams common_parameter_descriptions
#'
#' @return Dataframe with orography data as from \code{create_cell_orography_lookup}
#' @export
#'
#' @examples
#' target_domains_files <- list(
#'   'ABC-10' = "/path/to/file/ABC-10.nc",
#'   'XYZ-05' = "/path/to/file/XYZ-05.nc",
#'   )
#' create_cell_orography_lookup_from_domain_list(target_domains_files)
create_cell_orography_lookup_from_domain_list <- function(target_domains_files) {
  lapply(names(target_domains_files),
         function(target_domain) create_cell_orography_lookup(domain = target_domain,
                                                              file = target_domains_files[[target_domain]])) %>% bind_rows
}


#' Create sf points from lat/lon values
#'
#' Creates a simple feature collection of sf buffers centered around points defined in a dataframe
#'
#' Dataframe must contain latitidues, longitudes, and labels of the points;
#' in columns named 'lat', 'lon', and 'label', respectively.
#'
#' The default buffer distance is 0.05
#'
#' @param dataframe_locations Dataframe specifying locations.
#' @inheritParams sf::st_buffer
#' @inheritParams common_parameter_descriptions
#'
#' @return Simple feature collection
#' @export
#'
#' @examples
#' dataframe_locations <- tribble(~lat, ~lon, ~label,
#' 48.858222, 2.2945, 'Eiffel Tower',
#' 51.5007, -0.1245, 'Big Ben')
#' x <- create_shapes_from_dataframe_of_points(dataframe_locations)
#' ggplot(x) + borders('world') + geom_sf(fill = 'red', colour = 'red') +
#'   coord_sf(xlim = c(-10, 10), ylim = c(45, 55))
create_shapes_from_dataframe_of_points <- function(dataframe_locations,
                                                   dist = 0.05,
                                                   proj4string = common_defaults$proj4string) {
  num_points <- dim(dataframe_locations)[1]

  as_sf <- st_sf(
    Label = dataframe_locations$label,
    geometry = st_sfc(
      lapply(1:num_points, function(i)
        st_point(
          c(dataframe_locations[["lon"]][i],
            dataframe_locations[["lat"]][i]))
      )
    ),
    crs = proj4string
  )

  # need an adequately sized buffer, but not too large that it spreads across multiple cells
  as_polygons <- st_buffer(as_sf, dist = dist)

  as_polygons
}


#' Create a target cells lookup from an sf object
#'
#' For a single shape file, therefore a lot more can be assumed about the properties
#'
#' @param polygons_all_sf Simple feature object with polygons for all regions
#' @inheritParams convert_all_polygons_to_multipolygons
#' @inheritParams find_matching_cells_in_target_domains_for_polygon_sf
#'
#' @return A target cells lookup list
#' @export
create_polygons_target_cells_lookup_from_sf <- function(polygons_all_sf,
                                                        target_domains_files,
                                                        field_names = list(index = 'OBJECTID', label = 'LABEL', code = 'CODE'),
                                                        preferred_domains = list()) {

  logdebug('Convert all polygons to multipolygons')
  polygons_fixed_sf <- convert_all_polygons_to_multipolygons(polygons_all_sf, field_names = field_names)

  loginfo('Get matching cells for polygons...')
  num_polygons <- length(polygons_fixed_sf)
  target_cells_lookup <- lapply(1:num_polygons, function(polygon_sf_i) {
    # polygon_sf_i <- 1
    polygon_sf <- polygons_fixed_sf[[polygon_sf_i]]

    field_name_index <- field_names[['index']]
    field_name_label <- field_names[['label']]
    field_name_code <- field_names[['code']]

    # if index is not defined, create one
    if (is.null(field_name_index)) {
      polygon_index <- polygon_sf_i
    } else {
      polygon_index <- polygon_sf[[field_name_index]]
    }

    loginfo(sprintf('(%s/%s) %s', polygon_index, length(polygons_fixed_sf), polygon_sf[[field_name_label]]))

    matching_cells_with_weights_for_domains <- find_matching_cells_in_target_domains_for_polygon_sf(polygon_sf, target_domains_files)
    matching_cells_for_domains <- sapply(matching_cells_with_weights_for_domains, function(e) {e$cell}, simplify = FALSE, USE.NAMES = TRUE)

    preferred_domain <- cfdtk:::pick_preferred_domain_from_lookup(preferred_domains, target_domains_files, polygon_code)

    cfdtk:::generate_target_cells_lookup_entry(#
      id = polygon_index,
      code = polygon_sf[[field_name_code]],
      label = polygon_sf[[field_name_label]],
      sf = polygon_sf[['geometry']],
      domain = preferred_domain,
      matching_cells = matching_cells_for_domains,
      matching_cells_with_weights = matching_cells_with_weights_for_domains
    )

  })

  target_cells_lookup <- cfdtk:::set_names_for_target_cells_lookup(target_cells_lookup)
  target_cells_lookup
}


#' Create a target cells lookup from a dataframe of points with lat/lon values
#'
#' This lookup will specify each region/point of interest and which cells match each domain
#'
#' From a set of lat/lon points of interest
#'
#' @param dataframe_locations A dataframe of locations representing points to extract.
#' Required columns: label, lat, lon.
#' Optional columns: index, code (these will be generated if they don't exist).
#' @inheritParams common_parameter_descriptions
#'
#' @return A target cells lookup list
#' @export
create_polygons_target_cells_lookup_from_points <- function(dataframe_locations,
                                                            target_domains_files,
                                                            field_names = list(index = 'id', label = 'label', code = 'code'),
                                                            preferred_domains = list(),
                                                            proj4string = common_defaults$proj4string) {

  logdebug('Create cell orography lookup from domain files')
  cell_orog_lookup <- create_cell_orography_lookup_from_domain_list(target_domains_files)

  loginfo('Get matching cells for points...')
  num_points <- dim(dataframe_locations)[1]
  target_cells_lookup <- lapply(1:num_points, function(point_i) {
    # point_i <- 1
    point <- dataframe_locations[point_i,]

    field_name_index <- field_names[['index']]
    field_name_label <- field_names[['label']]
    field_name_code <- field_names[['code']]

    point_label <- point[[field_name_label]]

    # if index is not defined, or points to a non-existent column (i.e. defaults are used) create one
    if (is.null(field_name_index) | is.null(point[[field_name_index]])) {
      point_index <- point_i
    } else {
      point_index <- point[[field_name_index]]
    }

    # if code is not defined, or points to a non-existent column (i.e. defaults are used) create one
    if (is.null(field_name_code) | is.null(point[[field_name_code]])) {
      point_code <- point_label %>% label_to_code
    } else {
      point_code <- point[[field_name_code]]
    }

    loginfo(sprintf('(%s/%s) %s', point_index, num_points, point_label))

    matching_cells_for_domains <-
      sapply(
        names(target_domains_files),
        function(target_domain) get_cell_from_lat_lon_values(cell_orog_lookup, point[['lat']], point[['lon']], target_domain),
        simplify = FALSE,
        USE.NAMES = TRUE)

    preferred_domain <- cfdtk:::pick_preferred_domain_from_lookup(preferred_domains, target_domains_files, point_code)

    sf_of_point <- st_sf(
      index = point_index,
      label = point_label,
      code = point_code,
      geometry = st_sfc(
        st_point(
          c(point[["lon"]],
            point[["lat"]]))
        ),
      crs = proj4string
    )

    cfdtk:::generate_target_cells_lookup_entry(
      id = point_index,
      code = point_code,
      label = point_label,
      sf = sf_of_point,
      domain = preferred_domain,
      matching_cells = matching_cells_for_domains,
      matching_cells_with_weights = NULL
    )
  })

  target_cells_lookup <- set_names_for_target_cells_lookup(target_cells_lookup)
  target_cells_lookup
}


#' Title
#'
#' For a single shape file, therefore a lot more can be assumed about the properties
#'
#' @param all_polygons_shape_file Shape file will polygons for all regions
#' @inheritParams convert_all_polygons_to_multipolygons
#' @inheritParams find_matching_cells_in_target_domains_for_polygon_sf
#'
#' @return A target cells lookup list
#' @export
create_polygons_target_cells_lookup_from_single_shape_file <- function(all_polygons_shape_file,
                                                                       target_domains_files,
                                                                       field_names = list(index = 'OBJECTID', label = 'LABEL', code = 'CODE'),
                                                                       preferred_domains = list()) {
  logdebug('Read all polygons as sf')
  polygons_all_sf <- read_sf(all_polygons_shape_file)

  create_polygons_target_cells_lookup_from_sf(polygons_all_sf,
                                              target_domains_files,
                                              field_names = field_names,
                                              preferred_domains = preferred_domains)
}


#' Title
#'
#' For a shape files from a mixture of sources, therefore a lot more must be specified
#'
#' @param polygons_properties_table A dataframe specifying the shape files for each polygon, key fields for each polygon.
#' Required columns: filename (full path to file), index (), label, (), code ()
#' @inheritParams find_matching_cells_in_target_domains_for_polygon_sf
#'
#' @return A target cells lookup list
#' @export
create_polygons_target_cells_lookup_from_multiple_shape_files <- function(polygons_properties_table,
                                                                          target_domains_files,
                                                                          preferred_domains) {

  num_polygons <- dim(polygons_properties_table)[1]

  target_cells_lookup <- lapply(1:num_polygons, function(i) {
    # i <- 1
    lookup_row <- polygons_properties_table[i,]

    polygon_code <- lookup_row$code
    polygon_label <- lookup_row$label
    loginfo(glue("Processing {polygon_code} ({polygon_label})"))

    polygon_filename <- lookup_row$filename
    polygon_index <- lookup_row$index
    polygon_sf <- read_sf(polygon_filename)


    matching_cells_with_weights_for_domains <- find_matching_cells_in_target_domains_for_polygon_sf(polygon_sf, target_domains_files)
    matching_cells_for_domains <- sapply(matching_cells_with_weights_for_domains, function(e) { if(length(e) == 1 && is.na(e)) { NA } else { e$cell } }, simplify = FALSE, USE.NAMES = TRUE)

    preferred_domain <- cfdtk:::pick_preferred_domain_from_lookup(preferred_domains, target_domains_files, polygon_code)
    logdebug(glue("Preferred domain: {preferred_domain}"))

    cfdtk:::generate_target_cells_lookup_entry(
      id = polygon_index,
      code = polygon_code,
      label = polygon_label,
      sf = polygon_sf[['geometry']],
      domain = preferred_domain,
      matching_cells = matching_cells_for_domains,
      matching_cells_with_weights = matching_cells_with_weights_for_domains
    )
  })

  target_cells_lookup <- set_names_for_target_cells_lookup(target_cells_lookup)
  target_cells_lookup
}


#' Pick the preferred domain for a polygon when building a target cells lookup
#'
#' If there is only one domain, then set the preferred domain as that.
#' Otherwise, a list of which domain is the preferred to list the matching cells must be used.
#'
#' @inheritParams common_parameter_descriptions
#' @param polygon_code
#'
#' @return Domain to be chosen
pick_preferred_domain_from_lookup <- function(preferred_domains, target_domains_files, polygon_code) {
  if (length(target_domains_files) == 1) {
    names(target_domains_files)[1]
  } else {
    preferred_domains[[polygon_code]]
  }
}


#' Return list data structure for a target cells lookup entry
#' @return List
generate_target_cells_lookup_entry <- function(id, code, label, sf, domain, matching_cells, matching_cells_with_weights) {
  list(
    id = id,
    code = code,
    label = label,
    sf = sf,
    domain = domain,
    matching_cells = matching_cells,
    matching_cells_with_weights = matching_cells_with_weights
  )
}


#' Set the names of a target cells lookup list to the code property of each element
#' @inheritParams common_parameter_descriptions
set_names_for_target_cells_lookup <- function(target_cells_lookup) {
  setNames(target_cells_lookup, lapply(target_cells_lookup, function(i) { i[['code']] }))
}



# Use Lookup Functions ----------------------------------------------------

# Helper functions to retrieve data from lookups already created


#' Get cell from lat/lon values
#'
#' Find the cell in a domain that matches the given latitute and longitude.
#'
#' Only one cell is expected to be found for a particular pair of lat/lon values.
#' As a result, a \code{warning} error will be thrown if a cell is not found, or if multiple cells are found
#' (since this would indicate a malformed cell orography lookup, or invalid lat/lon inputs).
#'
#' Take care when providing lat/lon values that lie exactly on the border between multiple cells.
#' If this happens, then the chosen cell will the closest to 0 latitude and 0 longitude.
#'
#' @inheritParams common_parameter_descriptions
#' @param domain Domain (not required if cell orography lookup has only one domain)
#'
#' @return Cell number
#' @export
#'
#' @examples
#' cell_orog_lookup <- readRDS('/path/to/file.rds')
#' cell_number <- get_cell_from_lat_lon_values(cell_orog_lookup, lat = -42.8824, lon = 147.3301, domain = 'name_of_domain')
get_cell_from_lat_lon_values <- function(cell_orog_lookup, lat, lon, domain = NULL) {
  # clarify variable names from the column names of cell_orog_lookup
  lat_ <- lat
  lon_ <- lon
  domain_ <- domain

  # check if lookup has no rows
  if (dim(cell_orog_lookup)[1] == 0) {
    stop('Cell orography lookup has no rows')
  }

  # if domain was not provided, check to see if lookup has only one
  if (is.null(domain_)) {
    unique_domains <- cell_orog_lookup %>% pull(domain) %>% unique
    if (length(unique_domains) == 1) {
      domain_ <- unique_domains
    } else {
      stop('No domain specified and cell orography lookup has multiple domains')
    }
  }

  # rename some columns in cell_orog_lookup if they have the old column names, temporarily
  if ('long' %in% names(cell_orog_lookup)) {
    cell_orog_lookup <- cell_orog_lookup %>% rename(lon = long)
  }
  if ('resolution_long_km' %in% names(cell_orog_lookup)) {
    cell_orog_lookup <- cell_orog_lookup %>% rename(resolution_lon_km = resolution_long_km)
  }


  # old approach:
  # get the cell number
  cell_number <- cell_orog_lookup %>%

    # filter to the desired domain
    filter(domain == domain_) %>%

    # add columns detailing the edges of each cell
    mutate(lat_min = lat - 0.5 * resolution_lat_km,
           lat_max = lat + 0.5 * resolution_lat_km,
           lon_min = lon - 0.5 * resolution_lon_km,
           lon_max = lon + 0.5 * resolution_lon_km) %>%

    # filter by lat & lon
    #   the handling of edge cases is determined by the
    #   exact configuration of the operators: > >= < <=
    filter(lat_ >= lat_min & lat_ < lat_max) %>%
    filter(lon_ >= lon_min & lon_ < lon_max) %>%

    pull(cell_number)


  # return the cell (only one cell number is expected)
  # throw a warning if not
  if (length(cell_number) == 0) {
    warning(glue("No cell found matching lat={lat_} lon={lon_} domain={domain_}"))
    NA
  } else if (length(cell_number) > 1) {
    warning(glue("Multiple cells matching lat={lat_} lon={lon_} domain={domain_}\nValues: {paste(cell_number, collapse=', ')}"))
    NA
  } else {
    cell_number
  }
}


#' Get the closest cell to given lat/lon values
#'
#' Find the cell in a domain is closest to the given latitute and longitude, where the lat/lon does not have to be in the given domain.
#'
#' The geosphere::distHaversine() function is used to detemine the distance between the lat/lon values and the centre of each cell.
#'
#' @inheritParams common_parameter_descriptions
#' @inheritParams get_cell_from_lat_lon_values
#'
#' @return Cell number
#' @export
get_closest_cell_to_lat_lon_values <- function(cell_orog_lookup, lat, lon, domain = NULL) {
  # clarify variable names from the column names of cell_orog_lookup
  lat_ <- lat
  lon_ <- lon
  domain_ <- domain

  # check if lookup has no rows
  if (dim(cell_orog_lookup)[1] == 0) {
    stop('Cell orography lookup has no rows')
  }

  # if domain was not provided, check to see if lookup has only one
  if (is.null(domain_)) {
    unique_domains <- cell_orog_lookup %>% pull(domain) %>% unique
    if (length(unique_domains) == 1) {
      domain_ <- unique_domains
    } else {
      stop('No domain specified and cell orography lookup has multiple domains')
    }
  }

  # rename some columns in cell_orog_lookup if they have the old column names, temporarily
  if ('long' %in% names(cell_orog_lookup)) {
    cell_orog_lookup <- cell_orog_lookup %>% rename(lon = long)
  }
  if ('resolution_long_km' %in% names(cell_orog_lookup)) {
    cell_orog_lookup <- cell_orog_lookup %>% rename(resolution_lon_km = resolution_long_km)
  }


  # get cells closest to this one
  nearby_cells <- cell_orog_lookup %>%

    # filter to the desired domain
    filter(domain == domain_) %>%

    # wrap lon values greater than 180 to -180 to 0
    mutate(lon_wrap = ifelse(lon > 180, lon - 360, lon)) %>%

    mutate(distance_to_cell = geosphere::distHaversine(cbind(lon_, lat_), cbind(lon_wrap, lat))) %>%

    filter(distance_to_cell == min(distance_to_cell))


  # if there are multiple cells, choose the one closest to 0, 0
  if (nrow(nearby_cells) > 1) {
    closest_cell <- nearby_cells %>% arrange(abs(lon), abs(lat)) %>% .[1,]
  } else {
    closest_cell <- nearby_cells
  }

  cell_number <- closest_cell %>% pull(cell_number)

  # return the cell (only one cell number is expected)
  # throw a warning if not
  if (length(cell_number) == 0) {
    warning(glue("No cell found matching lat={lat_} lon={lon_} domain={domain_}"))
    NA
  } else if (length(cell_number) > 1) {
    warning(glue("Multiple cells matching lat={lat_} lon={lon_} domain={domain_}\nValues: {paste(cell_number, collapse=', ')}"))
    NA
  } else {
    cell_number
  }
}



#' Get cell orography data for specific cells
#'
#' Get the cell orography data for a specific set of cell numbers from the orography lookup for the chosen domain
#'
#' @inheritParams common_parameter_descriptions
#' @param target_cells Vector of cells to retrieve data for
#' @param domain Domain to retrieve data for
#'
#' @return Data frame with cell orography data
#' @export
#'
#' @examples
#' cell_orog_lookup <- readRDS('/path/to/file.rds')
#' target_cells <- c(987, 1597, 2584, 4181, 6765)
#' domain <- 'AUS-50'
#' get_cells_from_orog_lookup(cell_orog_lookup, target_cells, domain)
get_cells_from_orog_lookup <- function(cell_orog_lookup, target_cells, domain) {
  # rename function
  if (as.character(match.call()[[1]]) == "get_cells_from_orog_lookup") {
    warning("Please use get_cells() instead of get_cells_from_orog_lookup()", call. = FALSE)
  }

  # clarify parameters
  domain_ <- domain

  cell_orog_lookup %>%
    filter(cell_number %in% target_cells) %>%
    filter(domain == domain_)
}


#' Get cell orography data for region
#'
#' Get the cell orography data for the matching cells of the region from the orography lookup for the chosen domain
#'
#' @inheritParams common_parameter_descriptions
#' @param region Region. Can be the numerical index or the name of the list item in \code{target_cells_lookup} code
#' @param domain Domain
#'
#' @return Vector of cells
#' @export
#'
#' @examples
#' target_cells_lookup <- readRDS('/path/to/target_cells_lookup_file.rds')
#' cell_orog_lookup <- readRDS('/path/to/cell_orog_lookup_file.rds')
#' region <- 'region_name'
#' domain <- 'AUS-50'
#' get_cells_from_orog_lookup_for_region_domain(target_cells_lookup, cell_orog_lookup, region, domain)
get_cells_from_orog_lookup_for_region_domain <- function(target_cells_lookup, cell_orog_lookup, region, domain) {
  cells_in_region <- lookup_cells_for_region_with_domain(target_cells_lookup, region, domain)
  get_cells(cell_orog_lookup, cells_in_region, domain)
}


#' Get target cell indices for a domain from lookup object
#'
#' Return the cells in the specified domain that cover all of the regions.
#' Duplicate cells (i.e. cells that exist in multiple regions) will be listed only once.
#'
#' @inheritParams common_parameter_descriptions
#' @param target_domain The domain in the lookup object to get the matching cells for
#'
#' @return An integer vector of unique cells, not necessarily in sequencial order
#' @export
lookup_cells_for_domain <- function(target_cells_lookup, target_domain) {
  # all cells from the lookup
  cells_with_na <- unique(
    unlist(
      lapply(target_cells_lookup, function(region) {
        region[["matching_cells"]][[target_domain]]
      })
    )
  )

  # remove NA values
  # these are from regions/cells that do not exist within the target domain
  # (there will actually only be one NA value to remove due to unique call above)
  cells <- cells_with_na[!is.na(cells_with_na)]

  sort(cells)
}


#' Lookup preferred domain for a region
#'
#' Retrieve the domain specified to have the highest resolution data for a region
#'
#' @inheritParams common_parameter_descriptions
#' @param region Can be the numerical index or the name of the list element
#'
#' @return Domain
#' @export
lookup_domain_for_region <- function(target_cells_lookup, region) {
  target_cells_lookup[[region]][['domain']]
}


#' Lookup cells for a region
#'
#' Retrieve the cells for a region, in the domain specified to have the highest resolution data
#'
#' @inheritParams lookup_domain_for_region
#'
#' @return Vector of cells
#' @export
lookup_cells_for_region <- function(target_cells_lookup, region) {
  # if a region object (i.e. a list) is passed then use the 'code' property, otherwise expect the code itself to be passed
  region_code <- ifelse(class(region) == "list", yes = region[['code']], no = region)

  domain <- lookup_domain_for_region(target_cells_lookup, region_code)
  lookup_cells_for_region_with_domain(target_cells_lookup, region_code, domain)
}


#' Lookup cells in a specific domain for a region
#'
#' Retrieve the cells in a specific domain for a region
#'
#' @inheritParams lookup_domain_for_region
#' @param domain Domain to lookup, e.g. 'VIC-5'
#'
#' @return Vector of cells
#' @export
lookup_cells_for_region_with_domain <- function(target_cells_lookup, region, domain) {
  target_cells_lookup[[region]][['matching_cells']][[domain]]
}


#' Lookup region label from the code
#'
#' @inheritParams common_parameter_descriptions
#' @param region
#'
#' @return Label
#' @export
lookup_polygon_label_from_code <- function(target_cells_lookup, region) {
  target_cells_lookup[[region]][['label']]
}


#' Get all of the embedded sf objects from a target cells lookup
#'
#' @inheritParams common_parameter_descriptions
#'
#' @return An sfc object
#' @export
get_all_sf_objects_from_target_cells_lookup <- function(target_cells_lookup, proj4string = common_defaults$proj4string) {
  all_polygons_c <- c()
  for (target_region_code in names(target_cells_lookup_from_shapes)) {
    all_polygons_c <- c(all_polygons_c, target_cells_lookup_from_shapes[[target_region_code]]$sf)
  }

  st_sfc(all_polygons_c,
         crs = proj4string)
}



# the get_cells family will return the full cell orog data
# the lookup family will just get raw data from the target_cells_lookup

#' @export
#' @rdname get_cells_from_orog_lookup
get_cells <- get_cells_from_orog_lookup

#' @export
#' @rdname lookup_cells_for_domain
get_cells_for_domain <- lookup_cells_for_domain

#' @export
#' @rdname get_cells_from_orog_lookup_for_region_domain
get_cells_for_region_in_domain <- get_cells_from_orog_lookup_for_region_domain

#' @export
#' @rdname lookup_cells_for_region
get_cells_for_region <- lookup_cells_for_region





# Lookup Summary Functions ------------------------------------------------

# Functions to provide quick summary information on lookups

#' Display a simple summary of the number of cells for each region in the target cells lookup
#'
#' @rdname target_cells_lookup_summary_by_number_of_cells
#' @inheritParams common_parameter_descriptions
#'
#' @return Dataframe with columns: region, domain, number of cells
#' @export
target_cells_lookup_summary_by_number_of_cells <- function(target_cells_lookup) {
  region_cells <- lapply(target_cells_lookup, function(x) length(lookup_cells_for_region(target_cells_lookup, x))) %>% unlist

  regions <- names(region_cells)

  domains <- lapply(regions, function(x) lookup_domain_for_region(target_cells_lookup, x)) %>% unlist

  df <- data.frame(region = regions,
                   domain = domains,
                   num_cells = setNames(region_cells, nm = NULL)) %>% arrange(num_cells)

  df
}


#' Sort target cells lookup by number of cells
#'
#' Returns the target cells lookup itself, sorted in the order of least cells to most cells.
#'
#' This is useful when performing long batch processes, as smaller regions will be processed first,
#' meaning the results for a greater number of regions will be ready to to inspect sooner
#'
#' @rdname target_cells_lookup_summary_by_number_of_cells
#' @inheritParams common_parameter_descriptions
#'
#' @return A sorted lookup
#' @export
target_cells_lookup_sort_by_number_of_cells <- function(target_cells_lookup) {
  df <- target_cells_lookup_summary_by_number_of_cells(target_cells_lookup)

  target_cells_lookup[df$region]
}


#' Plot a simple visualisation of the
#'
#' @rdname target_cells_lookup_summary_by_number_of_cells
#' @inheritParams common_parameter_descriptions
#'
#' @return ggplot object of the plot
#' @export
target_cells_lookup_plot_by_number_of_cells <- function(target_cells_lookup) {
  ggplot(data = target_cells_lookup_summary_by_number_of_cells(target_cells_lookup)) +
    geom_bar(data = df,
             mapping = aes(x = reorder(region, num_cells), y = num_cells),
             stat = 'identity') +
    # note: these labels also get flipped with coord flip
    xlab("Region") +
    ylab("Number of cells") +
    scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
    labs(title = 'Regions by number of cells in preferred domain') +
    geom_text(mapping = aes(x=region, y=num_cells, label=num_cells, hjust=0), size = 2.5, nudge_y = 8) +
    theme(axis.ticks.y = element_blank()) +
    coord_flip()
}



# Data Archive File Functions ---------------------------------------------

# Functions to find and identify files in the data archives

#' Identify input files from data archive for model/domain/variable combinations
#'
#' This follows the THREDDS conventions of directory structure and file naming
#'
#' Create a data frame where each row represents a file to extract, and the corresponding model/domain/variable.
#' Uses expand.grid on a list of models, domains and variables; then searches the source_directory for files matching
#' those in the directory structure convention used by the THREDDS data server.
#'
#' @param target_models Vector of models (corresponding to directory structure)
#' @param target_domains Vector of domains (corresponding to directory structure)
#' @param target_variables Vector of variables of interest (corresponding to directory structure)
#' @param source_directory Source data files for climate data
#' @param org Thredds: organisation
#' @param data_group Thredds: data group
#' @param model_initial_conditions Thredds: model initial conditions
#' @param model_version Thredds: model version
#' @param data_version Thredds: data version
#' @param tr Thredds: temporal resolution
#'
#' @return A dataframe with columns with the model, domain, variable and filename of each file found
#' @export
identify_files_for_target_model_input_thredds <- function(target_models, target_domains, target_variables,
                                                          source_directory = "/rdsi/private/climatefutures/WINE/output",
                                                          org = 'CSIRO',
                                                          data_group = 'rcp85',
                                                          model_initial_conditions = 'r1i1p1',
                                                          model_version = 'CSIRO-CCAM-r3355',
                                                          data_version = 'v1',
                                                          tr = 'day') {

  logdebug(glue("source_directory         = {source_directory}"))
  logdebug(glue("org                      = {org}"))
  logdebug(glue("data_group               = {data_group}"))
  logdebug(glue("model_initial_conditions = {model_initial_conditions}"))
  logdebug(glue("model_version            = {model_version}"))
  logdebug(glue("data_version             = {data_version}"))
  logdebug(glue("tr                       = {tr}"))

  # to begin, produce a table/grid with each row corresponding
  # to a model / domain / variable combination to process

  # model / domain / variable grid
  mdv_grid <- expand.grid(model = target_models,
                          domain = target_domains,
                          variable = target_variables,
                          stringsAsFactors = FALSE)

  # slow to build a dataframe row-by-row
  # but we don't know for certain how many rows there will be
  input_list <- data.frame(model = character(),
                           domain = character(),
                           variable = character(),
                           file = character(),
                           stringsAsFactors = FALSE)

  # process each row of the target inputs table
  # (can't get one of the apply family to work so this will have to do)
  for(i in seq(1, dim(mdv_grid)[1])) {
    row <- mdv_grid[i,]
    target_model <- row$model
    target_domain <- row$domain
    target_variable <- row$variable
    target_pattern <- sprintf("%s.*%s", target_variable, target_model)

    dir_hist  <- glue('{source_directory}/{target_domain}/{org}/{target_model}/historical/{model_initial_conditions}/{model_version}/{data_version}/{tr}/{target_variable}')
    dir_group <- glue('{source_directory}/{target_domain}/{org}/{target_model}/{data_group}/{model_initial_conditions}/{model_version}/{data_version}/{tr}/{target_variable}')

    target_files_hist  <- dir(path = dir_hist, pattern = target_pattern, full.names = TRUE, ignore.case = TRUE)
    target_files_group <- dir(path = dir_group, pattern = target_pattern, full.names = TRUE, ignore.case = TRUE)
    target_files <- c(target_files_hist, target_files_group)

    loginfo(sprintf("%s files found in dir scan for: %s %s %s", length(target_files), target_domain, target_variable, target_model))

    grid <- expand.grid(model = target_model,
                        domain = target_domain,
                        variable = target_variable,
                        files = target_files,
                        stringsAsFactors = FALSE)

    input_list <- rbind(input_list, grid)
  }

  input_list
}


#' Select files to process from a pre-calculated list for target variables in a target domain
#'
#' @param target_inputs_table A dataframe with list of input files to extract data from. Must contains the columns: model, domain, variable, file.
#' @param target_domain Domain
#' @param target_variables A vector of variables
#'
#' @return A named list with variables as the keys, and the values being a vector of filenames
#' @export
select_files_for_target_domain_and_variables <- function(target_inputs_table, target_domain, target_variables) {
  sapply(target_variables, function(var) {
    target_files_var <- target_inputs_table %>%
      dplyr::filter(domain == target_domain & variable == var) %>%
      dplyr::select(files)

      unlist(target_files_var, use.names = FALSE)
    },
    simplify = FALSE, USE.NAMES = TRUE)
}



# Extract Functions -------------------------------------------------------

# Functions to assist with the extraction of data from NetCDF files


#' Extract data from NetCDF files for all regions in a target cells lookup for given model/domain/variables (and optionally perform post-calculations)
#'
#' @param target_inputs_table A dataframe with list of input files to extract data from. Must contains the columns: model, domain, variable, file.
#' The model, domain and variable columns must correspond with the filename, and the variable column must correspond with name of the variable in the NetCDF file.
#' @param target_cells_lookup Pre-calculated lookup listing the cells that exist in each region for each domain
#' @param extract_directory Directory to output the extract
#' @param batch_size Number of cells in each batch. Each batch is saved to disk for later use. If this number is too high then performance will suffer. Set to 100 by default.
#' @param batch_limit Number of batches to process. Useful for testing script behaviour prior to running on a complete data set.
#' @param batch_start_number Begin the extract from a particular batch number. This allows the process to resume after a system crash, etc. Note: does not work in conjunction with batch_limit (leave batch_limit set to NA to avoid strange behaviour).
#' @param post_process_function Name of a function to process the data through after it is extracted from the source files
#' @param post_process_params Parameters to pass to \code{post_process_function}
#'
#' @return Data frame with the extracted (and calculated) data
#' @export
extract_and_calc <- function(target_inputs_table,
                             target_cells_lookup,
                             extract_directory,
                             batch_size = 100,
                             batch_limit = NA,
                             batch_start_number = 1,
                             post_process_function = NULL,
                             post_process_params = NULL) {

  loginfo(glue("Extract and calc"))

  target_models <- target_inputs_table$model %>% unique
  target_domains <- target_inputs_table$domain %>% unique
  target_variables <- target_inputs_table$variable %>% unique

  loginfo(glue("target_domains:      {logarray(target_domains)}"))
  loginfo(glue("target_models:       {logarray(target_models)}"))
  loginfo(glue("target_variables:    {logarray(target_variables)}"))

  extract_directory <- ensure_dir_exists(extract_directory)
  loginfo(glue("extract_directory:   {extract_directory}"))

  # throw a warning if unsupported variables are used
  check_all_variables_are_supported(target_variables)

  # end if no files in the target inputs table
  verify_target_inputs_table(target_inputs_table)


  ## Domain / Cell Statistics
  #
  loginfo(glue("Number of cells to process in each domain:"))

  for (target_domain in target_domains) {
    cells <- lookup_cells_for_domain(target_cells_lookup, target_domain)
    loginfo(glue("{target_domain} -> {length(cells)}"))
  }



  ## Extract and calc each Domain
  #
  time_log <- logtime_init()
  time_log <- logtime(time_log, 'all_domains', 'start')

  for (target_domain in target_domains) {
    # target_domain <- target_domains[1]
    loginfo(glue("Running extract for {target_domain} domain"))
    loginfo(glue("target_models:    {logarray(target_models)}"))
    loginfo(glue("target_variables: {logarray(target_variables)}"))

    domain_log_name <- "domain_{target_domain}" %>% glue
    time_log <- logtime(time_log, domain_log_name, 'start')


    ## Search for Input Files and Validate ##
    #  create a names list of the files for each variable
    loginfo("Search for files to process")

    target_files <- select_files_for_target_domain_and_variables(target_inputs_table,
                                                                 target_domain,
                                                                 target_variables)

    # check that there are actually files matching this model/domain
    if (length(unlist(target_files)) == 0) {
      loginfo(glue("No files found -- target_domain: {target_domain}; target_models: {logarray(target_models))}; target_variables: {logarray(target_variables)}"))
      loginfo(glue("Moving to next input target"))
      return()
    }



    ## Setup Cell Batches
    #
    loginfo("Prepare cell batches")

    cells_list <- lookup_cells_for_domain(target_cells_lookup, target_domain) %>% sort
    total_cells <- length(cells_list)

    loginfo(glue("Total cells: {total_cells}"))

    if (total_cells == 0) {
      loginfo(glue("No cells found for this domain"))
      loginfo(glue("Moving to next input target"))
      return()
    }

    loginfo(glue("Batch size: {batch_size}"))

    batch_params <- calculate_batch_params(total_cells = total_cells,
                                           batch_size = batch_size,
                                           batch_start_number = batch_start_number,
                                           batch_limit = batch_limit)

    total_cells <- batch_params$total_cells_in_batches
    batch_start <- batch_params$batch_start
    batch_end   <- batch_params$batch_end
    batch_limit <- batch_params$batch_limit
    num_batches <- batch_params$num_batches

    loginfo(glue("Num cells: {total_cells}"))    # can be different to above value if adjusted in calculate_batch_params
    loginfo(glue("Num batches: {num_batches}"))


    ## Process each model
    #
    for (target_model in target_models) {
      # target_model <- target_models[1]

      ## Process all batches
      #
      time_log <- logtime(time_log, 'all_batches', 'start')
      loginfo(glue("Start all batches for {target_domain} domain with model {target_model}"))

      for (i in 1:num_batches) {
        # i <- 1
        time_log <- logtime(time_log, 'batch', 'start')

        cells_batch <- cells_list[batch_start[i]:batch_end[i]]
        cell_start <- cells_batch[1]
        cell_end <- tail(cells_batch, n = 1)
        loginfo(glue("Batch {i}/{num_batches} started. Cells: {cell_start}-{cell_end}"))


        ## Extract Variables from Files ##
        #
        loginfo("Start extract variables from files")
        time_log <- logtime(time_log, 'extract', 'start')

        # the extracted data for all variables is stored in the same list
        batch_data <- list()

        for (var in target_variables) {
          # var <- target_variables[1]
          loginfo(var)
          var_log_name <- glue("var_{var}")
          time_log <- logtime(time_log, var_log_name, 'start')

          # extract data for this target_model
          target_files_model_indices <- which(!is.na(str_match(string = target_files[[var]], pattern = target_model)))
          target_files_model <- target_files[[var]][target_files_model_indices]

          batch_data[[var]] <- extract_variable_from_files_by_cells(target_files_model, cells_batch)

          time_log <- logtime(time_log, var_log_name, 'end')
        }

        loginfo("Finish extract variables from files")
        time_log <- logtime(time_log, 'extract', 'end')


        ## Display time taken for each variable ##
        #
        for (var in target_variables) {
          var_log_name <- glue("var_{var}")
          loginfo(glue("Extract time for '{var}': {logtime_print_diff(time_log, var_log_name)}"))
        }
        loginfo(glue("Total extract time: {logtime_print_diff(time_log, 'extract')}"))


        ## Identify temporal range for variables ##
        #
        # # if there were no target cells then this domain does not contain any cells for the target files
        # # if tasmax_by_cells is valid it will be a data frame, if not then it will just be NA
        # # worth checking all 3 objects?
        #
        # if (!is.data.frame(tasmax_batch)) {
        #   loginfo(glue("No matching cells for this domain -- target_model: {target_model}; target_domain: {target_domain};"))
        #   loginfo(glue("Moving to next input target"))
        #   return()
        # }


        ## Generate Time Series ##
        #  We can just create the df from the first variable, since the timeseries will be the same for all
        loginfo("Generate time series dataframe from nc files")

        time_df <- make_time_dataframe_from_nc_files(target_files[[1]])


        ## Build Batch Dataframe ##
        #  Has raw data values for each variable from source files combined with time series
        loginfo(glue("Build dataframe from extracted variables for batch"))

        data_for_cell_batch <- build_data_table_by_cells(time_df, batch_data, target_variables,
                                                         convert_temps_from_K_to_C = TRUE,
                                                         convert_pr_from_rate_to_mm = TRUE,
                                                         convert_epanave_from_energy_to_mm = TRUE)


        ## Post Extract Calculations ##
        #  optional step that will run a function on the extracted data before saving it to disk
        if (is.null(post_process_function) || is.na(post_process_function)) {
          data_for_cell_batch_processed <- data_for_cell_batch
        } else {
          loginfo(glue("Start post processing: domain={target_domain}, model={target_model}"))
          time_log <- logtime(time_log, 'post_process', 'start')

          data_for_cell_batch_processed <- do.call(what = post_process_function, args = list(data_for_cell_batch, post_process_params))

          time_log <- logtime(time_log, 'post_process', 'end')
          loginfo(glue("Finish post processing: {logtime_print_diff(time_log, 'post_process')}"))
        }

        ## Write results to file ##
        #  filenames include the model, domain and cell range (cell range figures are padded to be the same number of digits for all files)
        output_file_prefix <- generate_output_filename_prefix(target_model, target_domain, cell_start, cell_end, last_cell = max(cells_list))
        write_to_RDS_file(data_for_cell_batch_processed, output_directory = extract_directory, filename = output_file_prefix)

        logdebug("Removing large calculated variables")
        rm(batch_data)
        rm(time_df)
        rm(data_for_cell_batch)
        rm(data_for_cell_batch_processed)
        logdebug("Running gc()")
        gc()

        time_log <- logtime(time_log, 'batch', 'end')
        loginfo(glue("Batch {i}/{num_batches} finished. Batch time: {logtime_print_diff(time_log, 'batch')}"))
      }

      time_log <- logtime(time_log, 'all_batches', 'end')
      loginfo(glue("Finished all batches for {target_domain} domain with model {target_model}"))
      loginfo(glue("Finished all batches time: {logtime_print_diff(time_log, 'all_batches')}"))
    }

    time_log <- logtime(time_log, domain_log_name, 'end')
    loginfo(glue("Finished domain {target_domain}"))
    loginfo(glue("Finished domain time: {logtime_print_diff(time_log, domain_log_name)}"))
  }

  time_log <- logtime(time_log, 'all_domains', 'end')
  loginfo(glue("Finished all domains"))
  loginfo(glue("Finished all domains time: {logtime_print_diff(time_log, 'all_domains')}"))
}


extract_variables_in_batches <- function() {

}


#' Make time dataframe from time dimension of a list of NetCDF files
#'
#' Fields produced are: year, month, day, day_of_year, Austral_season_year, Austral_season_month, Austral_day_of_season
#'
#' @param nc_files List of NetCDF files
#'
#' @return A data frame with the described fields
#' @export
#'
#' @examples
#' nc_files <- dir(path = "/path/to/nc/files/", pattern = "*.nc", full.names = TRUE)
#' make_time_dataframe_from_nc_files(nc_files)
make_time_dataframe_from_nc_files <- function(nc_files) {

  nc_files_time_string <- extract_and_combine_time_from_netcdf_files(nc_files)
  nc_files_time_lt <- as.POSIXlt(nc_files_time_string)

  time_df <- create_time_series_dataframe(nc_files_time_lt)

  return(time_df)
}


#' Extract values from cells from a list of NetCDF files
#'
#' @param target_files Vector of NetCDF files to extract values from
#' @param target_cell_indices Vector of cell indices to extract from the given files
#' @param target_variable If the NetCDF file has multiple variable, specify here otherwise only the first will be extract and a warning message will be produced
#' @param crop If TRUE, crop the the brick before performing the data extract --- which can be faster for large datasets, but cause issues for others (default: TRUE)
#' @param silent If TRUE, will not loginfo for each file as it is processed (default: FALSE)
#'
#' @return Data frame with 3 columns, layer (point in time series), cell, and the value
#' @export
#'
#' @examples
#' target_files = c('/path/to/data/a.nc', '/path/to/data/b.nc', '/path/to/data/c.nc', '/path/to/data/d.nc')
#' target_cell_indices = c(1, 2, 3, 4, 5)
#' tmax_for_all_region_cells <- extract_variable_from_files_by_cells(target_files, target_cell_indices)
extract_variable_from_files_by_cells <- function(target_files, target_cell_indices, target_variable = NULL, crop = TRUE, silent = FALSE) {
  # return NA if no cells are passed
  if(!is.numeric(target_cell_indices)){
    return(NA)
  } else {
    if (!silent) { loginfo("Extract variable from files") }

    data_series_for_all_files <- lapply(target_files, target_files = target_files, FUN = function(target_file, target_files) {
      # target_file <- target_files[1]

      ## old log style
      # only log the dirname for files once to avoid log clutter
      # log dirname with basename if this is first file in this dir
      # (assuming alphabetical order)
      dirnames <- dirname(target_files)
      dirname_target_file <- dirname(target_file)
      first_in_dir <- which(dirnames == dirname_target_file)[1] == which(target_file == target_files)
      if (first_in_dir) {
        if (!silent) { loginfo(glue("{dirname_target_file}")) }
      }
      if (!silent) { loginfo(basename(target_file)) }


      # per some benchmarks, it appears to be faster to crop the the brick before performing the data extract
      # we map the cell indices from the entire brick to XYs, then from XYs to the indices of the crop
      # after the data is extracted from the brick, the indices for the whole brick are imposed back on the data
      #
      # without the crop, the code would simply be this:
      # brick_of_nc <- raster::brick(target_file)
      # timeseries_per_cell <- t(raster::extract(brick_of_nc, target_cell_indices)

      # # commented out with market insights performance issues
      # #
      # # without the crop, the code would simply be this:
      # # 08/06/2018: cropping causing some weird issues with ERA data, so back to this for now
      # logdebug("...producing brick...")
      # brick_of_nc <- raster::brick(target_file)
      # logdebug("...producing timeseries")
      # timeseries_per_cell <- t(raster::extract(brick_of_nc, target_cell_indices))


      if (is.null(target_variable)) {
        brick_of_nc <- raster::brick(target_file)
      } else {
        brick_of_nc <- raster::brick(target_file, varname = target_variable)
      }


      if (crop) {
        if (!silent) { logdebug("...producing and cropping brick...") }
        cropped_brick_of_nc <- crop(brick_of_nc, extentFromCells(brick_of_nc, target_cell_indices))

        if (!silent) { logdebug("...map cell indices to cropped brick...") }
        target_XYs_parent <- xyFromCell(brick_of_nc, target_cell_indices)
        target_cells_in_child <- cellFromXY(cropped_brick_of_nc, target_XYs_parent)

        if (!silent) { logdebug("...producing timeseries...") }
        timeseries_per_cell <- t(raster::extract(cropped_brick_of_nc, target_cells_in_child))
      } else {
        timeseries_per_cell <- t(raster::extract(brick_of_nc, target_cell_indices))
      }

      colnames(timeseries_per_cell) <- sprintf("X%i",target_cell_indices)
      rownames(timeseries_per_cell) <- NULL

      return(as.data.frame(timeseries_per_cell))
    })

    # this "gathered <-" might be failing if data_series_for_all_files has no rows due to row_number?
    # perhaps add a conditional for empty data_series_for_all_files?

    if (!silent) { logdebug("...gathering data...") }

    # above outputs data with a column per cell, and a row per layer in the brick (unit time)
    # need to unpivot/gather the data to have a cell column (with leading 'X' stripped off)
    gathered <- dplyr::bind_rows(data_series_for_all_files) %>%
      mutate(layer = seq_len(n())) %>%
      gather(key = cell, value = value, -layer) %>%
      mutate(cell = as.integer(replace(cell, TRUE, substring(cell, 2)))) %>%
      dplyr::select(layer, cell, value)

    return(gathered)
  }
}


#' Extract values from cells matching a particular domain from a list of NetCDF files
#'
#' A wrapper for `extract_variable_from_files_by_cells` where the target cells are extracted from a lookup for a particular domain
#'
#' @inheritParams common_parameter_descriptions
#' @inheritParams extract_variable_from_files_by_cells
#' @inheritParams lookup_cells_for_domain
#'
#' @return Data frame with 3 columns, layer (point in time series), cell, and the value
#' @export
extract_variable_from_files_by_domain <- function(target_files, target_domain, target_cells_lookup, silent = FALSE) {
  # the unique list of cells in this domain that cover all of the regions
  target_cell_indices <- lookup_cells_for_domain(target_cells_lookup, target_domain)

  cells <- extract_variable_from_files_by_cells(target_files, target_cell_indices, silent)

  return(cells)
}


#' Build...
#'
#' The data can be from an arbitrary group of cells (e.g. a batch)
#'
#' @param time_df Data frame with time series data, e.g. produced from \code{make_time_dataframe_from_nc_files}
#' @param batch_data Spatial cell extract data for a group of cells as a list of values for each variable
#' @param target_variables Vector of variables to extract from the \code{batch_data} list
#' @param method How to extract and aggregate the data; either 'by_cell' or 'by_region'
#' @param region_info Item from the target cells lookup list corresponding to the region being processed. Required for a method of 'by_region'
#' @param convert_temps_from_K_to_C If TRUE, will assume all temperature values are in K and will convert them to C (default: FALSE)
#' @param convert_pr_from_rate_to_mm If TRUE, will assume pr values are rate and will convert them to mm (default: TRUE)
#' @param convert_epanave_from_energy_to_mm If TRUE, will assume epanave values are in energy and will convert them to mm (default: TRUE)
#'
#' @return A dataframe with the contents of \code{time_df} as the data for the with the contents of \code{batch_data} added
#' @export
build_data_table_from_batch_data <- function(time_df, batch_data, target_variables, method, region_info = NULL,
                                             convert_temps_from_K_to_C = TRUE, convert_pr_from_rate_to_mm = TRUE, convert_epanave_from_energy_to_mm = TRUE) {
  # correction offsets/factors
  # K = C - 273.15
  # 86400 = 24hr * 60min * 60sec
  # 28.4 is the amount of energy (W/m^2) required to evaporate 1mm of water (for CCAM models)
  temp_offset <- ifelse(convert_temps_from_K_to_C, 273.15, 0)
  pr_factor <- ifelse(convert_pr_from_rate_to_mm, 86400, 1)
  epanave_factor <- ifelse(convert_epanave_from_energy_to_mm, (1 / 28.4), 1)

  # base columns
  # use the first data set for these, since they are all the same

  # check
  if (class(time_df) == 'list') {

  }

  # TODO: we need to ge thte total range

  # add region code and region label columns if method is by_region
  switch(method,
         by_cell = {
           tbl <- time_df[as.numeric(pull(batch_data[[1]], layer)),]
           tbl$cell <- pull(batch_data[[1]], cell)
         },
         by_region = {
           tbl <- time_df[as.numeric(unique(pull(batch_data[[1]], layer))),]
           tbl$region_code <- region_info[['code']]
           tbl$region_label <- region_info[['label']]
         },
         # default:
         {
           stop("Invalid method")
         })


  # variables
  logdebug(glue("Variables in batch: {logarray(target_variables)}"))

  for (var in target_variables) {

    # for by_cell:
    #   data is extracted as-is per cell
    # for by_regions
    #    data is summarised across the entire region
    #    this assumes that batch_data has all and only all of the data for a region
    #    i.e. all cells for that region and no other cells
    switch(method,
           by_cell = {
             batch_data_for_var <- batch_data[[var]]
             pull_var <- 'value'
           },
           by_region = {
             batch_data_for_var <- batch_data[[var]] %>%
               group_by(layer) %>%
               summarise(spatial_mean = mean(value))
             pull_var <- 'spatial_mean'
           },
           # default:
           {
             stop("Invalid method")
           })

    logdebug(sprintf("var: %s", var))
    logdebug(sprintf('%s rows, %s cols (%s)', dim(batch_data_for_var)[1], dim(batch_data_for_var)[2], paste(names(batch_data_for_var), collapse = ', ')))

    # allow a prefix for variable names if they have been bias adjusted
    prefix <- 'ba_qq_si_'

    if (startsWith(var, prefix)) {
      process_var <- str_replace_all(string = var, pattern = prefix, replacement = "")
      logdebug(glue("Processing {var} as {process_var}"))
    } else {
      process_var <- var
    }

    switch(process_var,
           tasmin = {
             tbl[[var]] <- pull(batch_data_for_var, pull_var) - temp_offset
           },
           tasmax = {
             tbl[[var]] <- pull(batch_data_for_var, pull_var) - temp_offset
           },
           tas = {
             tbl[[var]] <- pull(batch_data_for_var, pull_var) - temp_offset
           },
           pr = {
             tbl[[var]] <- pull(batch_data_for_var, pull_var) * pr_factor
           },
           # bias-adjusted pr
           bapr = {
             tbl[[var]] <- pull(batch_data_for_var, pull_var) * pr_factor
           },
           # bias-adjusted pr
           ba_qq_pr = {
             tbl[[var]] <- pull(batch_data_for_var, pull_var) * pr_factor
           },
           hurs = {
             tbl[[var]] <- pull(batch_data_for_var, pull_var)
           },
           rhminscr = {
             tbl[[var]] <- pull(batch_data_for_var, pull_var)
           },
           epanave = {
             tbl[[var]] <- pull(batch_data_for_var, pull_var) * epanave_factor
           },
           epan_ave = {
             tbl[[var]] <- pull(batch_data_for_var, pull_var) * epanave_factor
           },
           # default:
           {
             loginfo(sprintf("Unknown variable '%s', extracting values with no conversion", var))
             tbl[[var]] <- pull(batch_data_for_var, pull_var)
           }
    )
  }

  tbl
}


#' Prepare a cell based data table for processing
#'
#' @inheritParams build_data_table_from_batch_data
#'
#' @return A dataframe with the contents of \code{time_df} as the data for the with the contents of \code{batch_data} added
#' @export
#'
#' @examples
build_data_table_by_cells <- function(time_df, batch_data, target_variables,
                                      convert_temps_from_K_to_C = TRUE, convert_pr_from_rate_to_mm = TRUE, convert_epanave_from_energy_to_mm = TRUE) {

  build_data_table_from_batch_data(time_df, batch_data, target_variables, method = 'by_cell', region_info = NULL,
                                   convert_temps_from_K_to_C = convert_temps_from_K_to_C,
                                   convert_pr_from_rate_to_mm = convert_pr_from_rate_to_mm,
                                   convert_epanave_from_energy_to_mm = convert_epanave_from_energy_to_mm)
}


# TODO:
# extract the processing functions from the cells functions
# call them/it from these two
# the region one must first: group the data for each variable by layer, then summarise(x = mean(value))
# then when building the final dataframe, add the region_code and region_label columns
# also: provido for this function, batch_data is assumed to be all and only all of the data for a region
# i.e. all cells for that region and no other cells


#' Prepare a region based data table for processing
#'
#' @inheritParams build_data_table_from_batch_data
#'
#' @return A dataframe with the contents of \code{time_df} as the data for the with the contents of \code{batch_data} grouped into a single figure, with a column added designating the region
#' @export
#'
#' @examples
build_data_table_by_region <- function(time_df, batch_data, target_variables, region_info,
                                        convert_temps_from_K_to_C = TRUE, convert_pr_from_rate_to_mm = TRUE, convert_epanave_from_energy_to_mm = TRUE) {

  build_data_table_from_batch_data(time_df, batch_data, target_variables, method = 'by_region', region_info = region_info,
                                   convert_temps_from_K_to_C = convert_temps_from_K_to_C,
                                   convert_pr_from_rate_to_mm = convert_pr_from_rate_to_mm,
                                   convert_epanave_from_energy_to_mm = convert_epanave_from_energy_to_mm)
}


#' Check all variables are supported for extraction
#'
#' The "\code{build_data_table}" family of functions contain code to handle extraction of different types of variables.
#' For example, sometimes temperature will be stored in the source as Kelvin and sometimes it will be stored as Celcius.
#' Depending on the requirements of the project, the source data may need to be converted.
#' How each variable is converted must be handled manually.
#' This function notifies the user of the package if they are attempting to extract an unknown variable.
#'
#' Unknown variables are extracted as-is from the source files with no conversions.
#'
#' To handle a new variable with special requirements, code must be added to \code{build_data_table_for_cells} to perform any conversions on the source data, and the SUPPORTED_VARIABLES list in this function must be altered.
#'
#' @param target_variables
#'
#' @export
#'
#' @examples
#' target_variables <- c('tasmax', 'tasmin', 'pr')
#' check_all_variables_are_supported(target_variables)
check_all_variables_are_supported <- function(target_variables) {
  # check only supported variables are used
  SUPPORTED_VARIABLES <- c('tasmax', 'tasmin', 'tas', 'pr', 'hurs', 'rhminscr', 'epanave', 'epan_ave', 'bapr', 'ba_qq_pr')

  SUPPORTED_VARIABLES <- c(SUPPORTED_VARIABLES, str_c('ba_qq_si_', SUPPORTED_VARIABLES))

  if (!all(target_variables %in% SUPPORTED_VARIABLES)) {
    unknown_variables <- target_variables[!(target_variables %in% SUPPORTED_VARIABLES)]
    warning_message <- "
These variables are unknown: {paste(unknown_variables, collapse = ', ')}.

They will be extracted as-is from the source files with no conversions.
Fix this by adding code to handle these variables in `build_data_table_for_cells`
and by changing the list of supported variables in `check_all_variables_are_supported`."

    warning(glue(warning_message))
  }
}



# Extract Input File Helper Functions -------------------------------------

#' Verify that there are the name number of files ofr each combination of model / domain / variable
#'
#' @param target_inputs_table A dataframe with list of input files to extract data from. Must contains the columns: model, domain, variable, file.
#'
#' @return No value is returned, but `stop` error may be thrown or warnings may be logged
#' @export
#'
#' @examples
#' target_inputs_table <- identify_files_for_target_model_input_thredds(...)
#' verify_target_inputs_table(target_inputs_table)
verify_target_inputs_table <- function(target_inputs_table) {
  target_inputs_table_count <- dim(target_inputs_table)[1]
  if (target_inputs_table_count == 0) {
    stop("No files at all in target_inputs_table. Check function arguments.")
  } else {
    loginfo(sprintf("%s total files found", target_inputs_table_count))

    table_tally <- target_inputs_table %>% group_by(model, domain, variable) %>% summarise(num_files = n())
    unique_counts <- unique(table_tally$num_files)

    if (length(unique_counts) > 1) {
      table_combo_tally <- table_tally %>%
        group_by(num_files) %>%
        summarise(num_combinations = n()) %>%
        mutate(msg = ifelse(num_combinations == 1,
                            yes = sprintf('%s (%s combination)', num_files, num_combinations),
                            no  = sprintf('%s (%s combinations)', num_files, num_combinations)))

      logwarn('There are a different number of files for different combinations of model, domain and variable in target_inputs_table.')
      logwarn(sprintf("Unique counts: %s", paste(table_combo_tally$msg, collapse = ', ')))
    }
  }
}


# Extract Batch Parameters Helper Functions -------------------------------

# Functions to help with the calculation of batch parameters for the extract functions

#' Calculate batch indexing parameters
#'
#' @param total_cells Total number of cells in extract
#' @param batch_size Size of each batch
#' @param batch_start_number The batch number to start from (if not supplied, then all batches will be processed, i.e. the first batch will be 1)
#' @param batch_limit Maximum number of batches to process (if not supplied, then all batches will be processed)
#'
#' @return A named list with calculated batch paramaters for the given inputs. Named elements are:
#' \code{batch_start}: a vector with the starting cell of each batch;
#' \code{batch_end}: a vector with the ending cell of each batch;
#' \code{total_cells_in_batches}: total number of cells that will processed;
#' \code{batch_size}: size of each batch
#' \code{batch_start_number}: batch number to start from
#' \code{batch_limit}: the defined batch limit (if any);
#' \code{num_batches}: number of batches that will be processed.
#' @export
#'
#' @examples
#' target_cells_lookup <- readRD('/path/to/lookup_file.rds')
#' cells_to_process <- lookup_cell_for_domain(target_cells_lookup, 'MYDOMAIN')
#' calculate_batch_params(total_cells = length(cells_to_process), batch_size = 100)
#' calculate_batch_params(total_cells = length(cells_to_process), batch_size = 100, batch_limit = 3)
calculate_batch_params <- function(total_cells, batch_size, batch_start_number = 1, batch_limit = NA) {
  batch_start <- cfdtk:::generate_batch_start_vector(batch_size, total_cells)
  batch_end <- cfdtk:::generate_batch_end_vector(batch_size, total_cells)

  # original values before any alterations
  num_batches_possible <- length(batch_start)
  total_cells_in_batches <- total_cells

  # start from a particular batch number
  if (batch_start_number > 1) {
    if (batch_start_number > num_batches_possible) {
      stop(glue("Specified batch start number ({batch_start_number}) greater than",
                " number of possible batches ({num_batches_possible}) for given parameters",
                " (total_cells = {total_cells}, batch_size = {batch_size})"))
    }

    loginfo(glue("Starting from batch {batch_start_number}"))
    batch_start <- batch_start[batch_start_number:num_batches_possible]
    batch_end <- batch_end[batch_start_number:num_batches_possible]
  }

  # limit cells to a particular number of batches
  if (!is.na(batch_limit)) {
    # don't limit if there are already less batches than batch limit
    if (length(batch_start) > batch_limit) {
      loginfo(glue("Limit number of batches to {batch_limit}"))
      batch_start <- batch_start[1:batch_limit]
      batch_end <- batch_end[1:batch_limit]
      total_cells_in_batches <- batch_end[batch_limit]
    } else {
      loginfo(glue("Already less batches ({length(batch_start)}) than batch limit ({batch_limit})"))
    }
  }

  # actual number of batches after any alterations
  num_batches_actual <- length(batch_start)

  list(batch_start = batch_start,
       batch_end = batch_end,
       total_cells = total_cells,
       total_cells_in_batches = total_cells_in_batches,
       batch_size = batch_size,
       batch_start_number = batch_start_number,
       batch_limit = batch_limit,
       num_batches = num_batches_actual)
}


#' Generate vector of starting cells for batch processing
#'
#' @inheritParams calculate_batch_params
#'
#' @return Vector of starting cells
#'
#' @examples
#' generate_batch_start_vector(10, 55)  # 1, 11, 21, 31, 41, 51
generate_batch_start_vector <- function(batch_size, total_cells) {
  seq(1, total_cells, by = batch_size)
}


#' Generate vector of ending cells for batch processing
#'
#' @inheritParams calculate_batch_params
#'
#' @return Vector of ending cells
#'
#' @examples
#' generate_batch_end_vector(10, 55)  # 10, 20, 30, 40, 55
generate_batch_end_vector <- function(batch_size, total_cells) {
  # handle case where there are less items than the batch size
  batch_end <- if (batch_size > total_cells) {
    c(total_cells)
  } else {
    seq(batch_size, total_cells, by = batch_size)
  }

  if (tail(batch_end, 1) != total_cells) {
    batch_end <- c(batch_end, total_cells)
  }

  batch_end
}


#' Verify all batch files are present
#'
#' Examine an extract directory and verify that each file is named as expected given a set of batch parameters.
#'
#' This is useful if an extract was performed in multiple stages for resourcing reasons, to check that each batch has been processed correctly.
#'
#' @inheritParams extract_and_calc
#' @inheritParams common_parameter_descriptions
#' @inheritParams generate_output_filename_prefix
#'
#' @return A dataframe with a row for each batch specifying the batch number,
#' the first and last cell in the batch, the expected filename pattern,
#' and the name of the actual file (if it exists)
#' @export
verify_batch_files <- function(extract_directory, target_cells_lookup, target_domain, target_model, batch_size) {

  cells_list <- lookup_cells_for_domain(target_cells_lookup, target_domain) %>% sort
  total_cells <- length(cells_list)

  batch_params <- calculate_batch_params(total_cells = total_cells,
                                         batch_size = batch_size,
                                         batch_start_number = 1,
                                         batch_limit = NA)

  batch_start <- batch_params$batch_start
  batch_end   <- batch_params$batch_end
  num_batches <- batch_params$num_batches
  last_cell <- cells_list %>% last

  df_expected <- lapply(1:num_batches, function(i) {
    cells_batch <- cells_list[batch_start[i]:batch_end[i]]
    cell_start <- cells_batch[1]
    cell_end <- tail(cells_batch, n = 1)

    tibble(batch_number = i,
           cell_start = cell_start,
           cell_end = cell_end,
           filename_pattern = generate_output_filename_prefix(target_model = target_model,
                                                              target_domain = target_domain,
                                                              cell_start = cell_start,
                                                              cell_end = cell_end,
                                                              last_cell = last_cell))
  }) %>% bind_rows

  df_actual <- lapply(df_expected$batch_number, function(i) {
    tibble(batch_number = i,
           actual_file = list.files(path = extract_directory, pattern = df_expected$filename_pattern[i]))
  }) %>% bind_rows


  df_report <- left_join(df_expected, df_actual, by = c('batch_number'))
}


# Extract Output File Helper Functions ------------------------------------

#' Identify files with data for specified cells
#'
#' Determines which files in a directory of batches contain the data for a list of cells
#'
#' @param extract_data_dir Directory with saved data files with the containing cells in the filename, e.g. "0365-1389"
#' @param target_model Model
#' @param target_domain Domain
#' @param target_cells Vector of cells
#'
#' @return Vector of files
#' @export
#'
#' @examples
identify_files_with_data_for_target_cells <- function(extract_data_dir, target_model, target_domain, target_cells) {
  # get all files for model/domain and extract cell range for each file
  target_pattern <- sprintf("%s_%s.*", target_model, target_domain)
  all_files <- list.files(extract_data_dir, full.names = TRUE, recursive = TRUE, pattern = target_pattern)

  if (length(all_files) == 0) {
    msg <- glue("No files found matching pattern '{target_pattern}' in directory: {extract_data_dir}")
    logerror(msg)
    stop(msg)
  }

  all_files_split_cell_range_field <- grep("[0-9]{2,}-[0-9]{2,}", str_split(all_files, "_")[[1]])
  cell_range_per_file <- do.call(rbind, str_split(unlist(lapply(str_split(all_files, "_"), function(x){x[all_files_split_cell_range_field]})), "-"))

  target_files <- lapply(target_cells, function (target_cell) {
    target_files_index <- which( c(target_cell >= as.numeric(cell_range_per_file[,1]) & target_cell <= as.numeric(cell_range_per_file[,2])) == TRUE)
    target_file <- all_files[target_files_index]
  })

  unique(unlist(target_files))
}


# Access Extracted Data Functions -----------------------------------------

#' Build multi-model ensemble dataframe
#'
#' @param source_data_directory Directory with saved model
#' @param target_region_code Region to build dataframe for
#' @param target_models A vector of models to include. If NULL, then all models for data in the directory will be included.
#' @param required_filename_part Permits further filtering of which model files to include. Only files that contain this text will be used.
#'
#' @return
#' @export
build_multi_model_ensemble_for_polygon <- function(source_data_directory, target_region_code, target_models = NULL, required_filename_part = NULL) {
  # all files
  files <- list.files(path = source_data_directory, pattern = target_region_code, full.names = TRUE)

  # filter
  if (!is.null(required_filename_part)) {
    files <- files[str_detect(files, required_filename_part)]
  }

  # only models of interest
  if (!is.null(target_models)) {
    files <- lapply(target_models, function(x) files[str_detect(files, x)]) %>% unlist
  }

  model_from_file_index <- 4
  lapply(files, function(file) {
    # second last underscore separated section
    model_from_file <- basename(file) %>% str_split('_') %>% .[[1]] %>% tail(2) %>% .[1]
    readRDS(file) %>%
      mutate(model = model_from_file) %>%
      relocate(model, .before = domain)
  }) %>% bind_rows
}


#' Build multi-model mean dataframe
#'
#' @inheritParams build_multi_model_ensemble_for_polygon
#'
#' @return
#' @export
build_multi_model_mean_for_polygon <- function(source_data_directory, target_region_code, target_models = NULL, required_filename_part = NULL) {
  df_mme <- build_multi_model_ensemble_for_polygon(source_data_directory, target_region_code, target_models, required_filename_part)
  df_mmm <- build_multi_model_mean_from_mme(df_mme)
}


#' Build multi-model mean dataframe from pre-existing multi-model ensemble
#'
#' @param df_mme Dataframe of a multi-model ensemble
#'
#' @return
#' @export
build_multi_model_mean_from_mme <- function(df_mme, group_by_vars = NULL) {

  if (is.null(group_by_vars)) {
    columns_to_group_by_if_present <- c('date', 'year', 'month', 'day', 'period', 'domain', 'region', 'season', 'Austral_season_group_period', 'decade')
    group_by_vars <- intersect(names(df_mme), columns_to_group_by_if_present)
  }

  df_mmm <- df_mme %>%
    # group by specified columns, or defaults if not specified
    group_by_at(group_by_vars) %>%
    # grouping variables will not summarised,
    # so exclude model to summarise all data columns
    summarise_at(vars(-model), mean, na.rm = TRUE) %>%
    ungroup()

  df_mmm
}


#' Collate data from an extract directory for a given polygon for a given model
#'
#' @param extract_directory Directory with source data files named like "{1}_{2}_{3}" where: {1} is the GCM name, {2} is the domain code, {3} is the range of cell numbers in that file
#' @param target_model Model to use
#' @param target_region_code Region code to be added as a column to this dataframe
#' @inheritParams common_parameter_descriptions
#'
#' @return
#' @export
collate_polygon_data_for_model <- function(extract_directory, target_cells_lookup, target_region_code, target_model) {
  target_region_domain <- lookup_domain_for_region(target_cells_lookup, target_region_code)
  target_region_cells  <- lookup_cells_for_region(target_cells_lookup, target_region_code)
  target_region_files  <- identify_files_for_target_cells(extract_directory, target_model, target_region_domain, target_region_cells)

  loginfo(glue("Collate data for: {target_region_code} ({target_region_domain}); {length(target_region_files)} files, {length(target_region_cells)} cells ({target_model})"))

  target_region_data <- extract_cells_from_files(target_files = target_region_files,
                                                 target_cells = target_region_cells)

  target_region_data$domain <- target_region_domain
  target_region_data$region <- target_region_code
  target_region_data$model <- target_model

  column_names <- names(target_region_data)
  column_index_cell <- which('cell' == column_names)
  column_index_domain <- which('domain' == column_names)  # domain must be added first above
  column_indices_vars <- (column_index_cell+1):(column_index_domain-1)

  sort_order <- c(1:column_index_cell,    # 1 to 'cell' column
                  column_index_domain,    # domain
                  column_index_domain+1,  # region
                  column_index_domain+2,  # model
                  column_indices_vars)    # column indices for the variables

  # sort the variables to be at the end
  target_region_data %>% dplyr::select(sort_order)
}


#' Collate data from an extract directory for a given polygon for multiple (typically all) models
#'
#' @inheritParams common_parameter_descriptions
#' @inheritParams collate_polygon_data_for_model
#' @param target_models Vector of models to build dataframe from. If NULL (the default), then all available models in the extract directory will be used.
#'
#' @return
#' @export
collate_polygon_data <- function(extract_directory, target_cells_lookup, target_region_code, target_models = NULL) {
  # if target_models is not passed, then asssume all models and identify them from the filenames
  if (is.null(target_models)) {
    all_files <- list.files(path = extract_directory, full.names = FALSE)
    target_models <- read_part_of_filename_model(all_files) %>% unique %>% sort
  }

  lapply(target_models, function(target_model) {
    collate_polygon_data_for_model(extract_directory, target_cells_lookup, target_region_code, target_model)
  }) %>% bind_rows
}


#' @rdname collate_polygon_data
collate_polygon_data_for_models <- collate_polygon_data


#' Identify Files with Data for Specified Cells
#'
#' @param extract_directory Directory with saved data files with the containing cells in the filename, e.g. "0365-1389"
#' @param target_cells Vector of cells
#'
#' @return Vector of files
#' @export
identify_files_for_target_cells <- function(extract_directory, target_model, target_domain, target_cells) {
  # get all files for model/domain and extract cell range for each file
  target_pattern <- sprintf("%s_%s.*", target_model, target_domain)
  all_files <- dir(extract_directory, full.names = TRUE, recursive = TRUE, pattern = target_pattern)

  if (length(all_files) == 0) {
    logerror(glue("No files found matching pattern '{target_pattern}' in source directory: {extract_directory}"))
    stop()
  }

  all_files_split_cell_range_field <- grep("[0-9]{2,}-[0-9]{2,}", str_split(all_files, "_")[[1]])

  # easier to follow way
  cell_range_per_file <- all_files %>%
    str_replace('.rds', '') %>%
    str_split("_") %>%
    lapply(function(x) x[all_files_split_cell_range_field]) %>%
    unlist %>%
    str_split('-') %>%
    do.call(rbind, .)

  ## old way messy
  # cell_range_per_file <- do.call(rbind, str_split(unlist(lapply(str_split(str_replace(all_files, '.rds', ''), "_"), function(x){x[all_files_split_cell_range_field]})), "-"))

  target_files <- lapply(target_cells, function(target_cell) {
    target_files_index <- which( c(target_cell >= as.numeric(cell_range_per_file[,1]) & target_cell <= as.numeric(cell_range_per_file[,2])) == TRUE)
    target_file <- all_files[target_files_index]
  })

  unique(unlist(target_files))
}


#' Extract cell data from files
#'
#' Data for only given cells will be extracted from the list of files
#'
#' This is designed to be used in conjunction with \code{identify_files_for_target_cells},
#' and simplifies the process for extracting data from files where the cells of interest are spread across multiple files.
#' If a file does not have any of the cells in \code{target_cells} then no cells from the file will be added
#' (however, there will of course be an unnecessary increase in computation time as the file is loaded for no reason).
#'
#' A warning message is shown if some \code{target_cells} are missing from the final dataframe.
#'
#' @param target_files Vector of files
#' @param target_cells Vector of cells to extract data for (if NULL, then all cells will be extracted)
#'
#' @return Dataframe
#' @export
#'
#' @examples
extract_cells_from_files <- function(target_files, target_cells = NULL) {
  df_all <- lapply(target_files, function(file) {
    loginfo(file)

    df <- readRDS(file)

    if (!is.null(target_cells)) {
      # filter cells that aren't required
      df <- df %>% filter(cell %in% target_cells)
    }

    # sort by cell and then date, for more convenient viewing
    df %>% arrange(cell, date)
  }) %>% bind_rows

  cells_extracted <- unique(df_all$cell)

  if (!identical(as.integer(sort(cells_extracted)), as.integer(sort(target_cells)))) {
    missing_cells <- setdiff(target_cells, cells_extracted)
    warning(glue("Not all cells in target_cells have been extracted (missing cells: {cfdtk::logarray(missing_cells)})"))
  }

  df_all
}

#' @rdname extract_cells_from_files
load_files_for_cells <- extract_cells_from_files


# Write Output Functions --------------------------------------------------

# Helper functions for saving dataframes to disk and generating standardised filenames for data


#' Generate filename for CCAM extract
#'
#' Format is \code{model-name_domain_cell-start_cell-end} where each component has no underscores
#'
#' @param target_model Model that data is for
#' @param target_domain Domain that data is for
#' @param cell_start The first cell in this data file
#' @param cell_end The last cell in this data file
#' @param last_cell The last cell for the entire variable, to determine number padding for every file
#'
#' @return String to be used at the filename
#' @export
#'
#' @examples
generate_output_filename_prefix <- function(target_model, target_domain, cell_start, cell_end, last_cell) {
  max_cell_str_length <- str_length(last_cell)
  cell_start_text <- formatC(cell_start, width=max_cell_str_length, format='d', flag='0')
  cell_start_end <-  formatC(cell_end,   width=max_cell_str_length, format='d', flag='0')

  glue("{target_model}_{target_domain}_{cell_start_text}-{cell_start_end}")
}


#' Writes calculated stats object to file on disk
#'
#' This collection of wrapper functions handle a number of general tasks, such as ensuring the target directory exists and logging the start/end process and the size of the file.
#'
#' @param x Object to save
#' @param output_directory Directory to save file to
#' @param filename Name of the file (without the extension)
#' @param format File format to save; one of: 'csv', 'RData', 'rds'
#'
#' @export
#'
#' @examples
#' write_to_file(datasets::mtcars, '/path/to/dir', 'project_name_', 'csv')
write_to_file <- function(x, output_directory, filename, format) {
  loginfo(glue("Saving results to file on disk..."))
  logdebug(glue("R object size: {object.size(x)} bytes"))

  # ensure dir exists
  ensure_dir_exists(output_directory)

  # if filename already has the desired extention then don't add it
  full_filename <- if (str_split(filename, pattern = '\\.') %>% .[[1]] %>% tail(1) == format) {
    file.path(output_directory, glue("{filename}"))
  } else {
    file.path(output_directory, glue("{filename}.{format}"))
  }

  switch(format,
         csv = { readr::write_csv(x, full_filename) },
         rds = { saveRDS(x, file = full_filename) },
         RData = { save(x, file = full_filename) },
         # default:
         { stop(glue("Unknown format '{format}'")) })

  loginfo(glue("Saved: {full_filename}"))
  loginfo(glue("Size: {gdata::humanReadable(file.info(full_filename)$size)}"))
}


#' @rdname write_to_file
#' @export
write_to_csv_file <- function(x, output_directory, filename) {
  write_to_file(x, output_directory, filename, format = 'csv')
}


#' @rdname write_to_file
#' @export
write_to_RData_file <- function(x, output_directory, filename) {
  write_to_file(x, output_directory, filename, format = 'RData')
}


#' @rdname write_to_file
#' @export
#'
write_to_RDS_file <- function(x, output_directory, filename) {
  write_to_file(x, output_directory, filename, format = 'rds')
}


#' Title
#'
#' @param x
#' @param file
#'
#' @return
#' @export
#'
#' @examples
save_csv <- function(x, file) {
  write_to_file(x = x, output_directory = dirname(file), filename = basename(tools::file_path_sans_ext(file)), format = 'csv')
}


#' Title
#'
#' @param x
#' @param file
#'
#' @return
#' @export
#'
#' @examples
save_rds <- function(x, file) {
  write_to_file(x = x, output_directory = dirname(file), filename = basename(tools::file_path_sans_ext(file)), format = 'rds')
}


# Image Output Functions --------------------------------------------------

# Common image output functions

#' Save a grob to a PNG file
#'
#' Files are saved using some sensible defaults for general analysis usage.
#'
#' Default dimensions are 1920x1080 (to correspond with those of a standard large screen monitor). In addition, the png device will still be closed even if there is an error with the plot. This means that the file will still be created (with an empty canvas), and no lingering device will be left open.
#'
#' @param grob grob of plot to save
#' @param file the name of the output file
#' @param width the width of the image
#' @param height the height of the image
#' @param units the units in which \code{width} and \code{height} are given
#' @param res resolution of the image in ppi
#' @param silent Default: FALSE. If TRUE, will not display a loginfo message.
#'
#' @export
#'
#' @examples
#' grob <- ggplot() + geom_point(data = datasets::mtcars, mapping = aes(x = cyl, y = hp))
#' file <- '/path/to/output/figures/img.png'
#' save_png(grob, file)
save_png <- function(grob, file, width = 1920, height = 1080, units = "px", res = "144", silent = FALSE) {

  # if user omits extension then add it
  if (str_to_lower(str_sub(file, -4)) != '.png') {
    file <- paste0(file, '.png')
  }

  # log time taken, and if longer than 30 seconds, log it
  logtime_save_png <- logtime_init()
  logtime_save_png <- logtime(logtime_save_png, 'save_png', log_type = 'start')

  if (!silent) { loginfo(paste0("Plotting: ", file)) }

  ensure_dir_exists(dirname(file))

  png(filename = file, width = width, height = height, units = units, res = res)

  # save the grob to the file, if there is an error, still close the device
  tryCatch(
    expr = print(grob),
    finally = {
      dev.off()
      logtime_save_png <- logtime(logtime_save_png, 'save_png', log_type = 'end')

      if (!silent &&
          logtime_secs_diff(time_log = logtime_save_png, log_name = 'save_png') > 30) {
        loginfo(paste0("Time taken: ", logtime_print_diff(logtime_save_png, 'save_png')))
      }
    }
  )
}


#' @rdname save_png
#' @param preserve_filename Preserve the original filename (i.e. do not prefix with "A4_")
#' @details \code{save_png_a4} sets the dimension parameters to be such that the width takes an A4 page minus 25.4mm margins and the width:height ratio of the image is 4:3
#' (meaning the default width is 159.2mm and the default height is 119.4mm).
#' The text theme \code{grob} will be automatically be scaled to a text size of 9.5, in order to make the text size slightly smaller than standard 12pt text in a document.
#' In addition, the filename will be prefixed with "A4_" (if not already) to make it easier to differentiate between the A4 image and the screen image.
#' This automatic prefix can be prevented by setting the \code{preserve} parameter to \code{TRUE} (it is \code{FALSE} by default).
#' @export
save_png_a4 <- function(grob, file, width = NULL, height = NULL, units = "mm", res = 200, silent = FALSE, preserve_filename = FALSE) {

  if (is.null(width)) {
    width <- 210 - 2*(25.4)

    if (is.null(height)) {
      height <- width * (3/4)
    }
  }

  # prefix name with A4_ if it isn't already?
  if(!preserve_filename & str_starts(basename(file), 'A4_', negate = TRUE)) {
    file <- file.path(dirname(file), paste0('A4_', basename(file)))
  }

  save_png(grob + theme(text = element_text(size=9.5)),
           file, width = width, height = height, units = units, res = res, silent = silent)
}


#' Append two A4 generated images vertically
#'
#' Designed for join two images generated by \code{save_png_a4} that are the full page width
#'
#' @param files Vector of file paths, without the 'A4_' prefix
#' @param output Output file path
#'
#' @export
#'
#' @examples
append_a4_vertical <- function(files, output) {
  # files = c(file_gg, file_gg_legends)

  a4_files <- lapply(files, function(file) file.path(dirname(file), paste0('A4_', basename(file)))) %>% unlist

  img <- magick::image_append(magick::image_read(a4_files), stack = TRUE)

  magick::image_write(img, output)
}



# File / Directory Management Functions -----------------------------------

#' Ensure a directory exists
#'
#' Create a directory and any required parent directories if it does not already exist.
#'
#' Useful to do at the start of a script to ensure the directory exists later to save outputs to.
#' This is a convenience wrapper for dir.create(), with a small addition: the directory is returned by the function.
#' This allows the directory to be assigned to a variable on the same line as calling the function (see examples).
#'
#' @param path The directory
#'
#' @return The directory
#' @export
#'
#' @examples
#' output_directory <- ensure_dir_exists('/mnt/projects/my_project')
#' output_directory <- '/mnt/projects/my_project' %>% ensure_dir_exists
#' output_directory <- file.path('/mnt/projects/my_project', 'v1') %>% ensure_dir_exists
#'
#' file_to_save <- '/path/to/file_to_save.rds'
#' ensure_dir_exists(dirname(file_to_save))
#' saveRDS(mtcars, file_to_save)
ensure_dir_exists <- function(path) {
  dir.create(path, showWarnings = FALSE, recursive = TRUE)
  path
}


# Data Series Helper Functions --------------------------------------------

# Functions to help identify states and transitions in data series

#' Identify date in a vector that an associated vector of values first exceeds a given threshold
#'
#' For a vector of dates and a corresponding vector of values, finds the first date which is greater than a particular threshold value
#'
#' @param dates Vector of dates
#' @param values Vector of values
#' @param threshold Threshold value to test against
#'
#' @return The item in the dates vector which corresponds to the item in the values vector.
#' If the values vector never exceeds the threshold, then NA is returned.
#' @export
#'
#' @examples
#' df <- data.frame(date = seq(as.Date("1961-07-01"), as.Date("1961-07-31"), "days"),
#'                  vals = c(1.1, 2.2, 3.3, 4.4, 5.5, 6.6, 7.7, 8.8, 9.9) )
#' date_vector_exceeds_threshold(df$date, df$vals, 5)
date_vector_exceeds_threshold <- function(dates, values, threshold) {
  dates[which(values > threshold)[1]]
}


#' Identify last date in a vector that an associated vector of values is under a given threshold
#'
#' For a vector of dates and a corresponding vector of values, finds the last date which is less than a particular threshold value
#'
#' @inheritParams date_vector_exceeds_threshold
#'
#' @return The item in the dates vector which corresponds to the item in the values vector.
#' If the values vector never drops below the threshold, then NA is returned.
#' @export
#'
#' @examples
#' df <- data.frame(date = seq(as.Date("1961-07-01"), as.Date("1961-07-31"), "days"),
#'                  vals = seq(from = 31, to = 1, by = 1) )
#' last_date_vector_under_threshold(df$date, df$vals, 4)
last_date_vector_under_threshold <- function(dates, values, threshold) {
  last_idx <- tail(which(values < threshold), 1)

  if (length(last_idx) > 0) {
    dates[last_idx]
  } else {
    NA
  }
}


#' Convert a year and month to a POSIX date corresponding to the first of that month
#'
#' @param year Year
#' @param month Month
#' @param tz Timezone, as is passed to as.POSIXct (default: 'AEDT')
#'
#' @return
#' @export
#'
#' @examples
#' date_from_year_month(2016, 7)
#' date_from_year_month(2016, 7, tz = 'UTC')
date_from_year_month <- function(year, month, tz = 'AEDT') {
  as.POSIXct(glue("{year}-{month}-01", "%y-%m-%d"), tz = tz)
}


#' Title
#'
#' @param dates Vector of dates (POSIXct or date)
#'
#' @return
#' @export
#'
#' @examples
scale_x_datetime_decadal <- function(dates, ...) {
  scale_x_datetime(breaks = create_date_breaks_decadal(dates),
                   minor_breaks = create_date_breaks_yearly(dates),
                   date_labels = "%Y",
                   ...)
}


#' Title
#'
#' for scale_x_datetime
#'
#' @inheritParams scale_x_datetime_decadal
#'
#' @return
#' @export
#'
#' @examples
create_date_breaks_yearly <- function(dates) {
  date_range <- tibble::enframe(dates) %>%

    # ## this is not needed, because it will be sorted out by range?
    # # first day of year
    # filter(month(value) == 1 & day(value) == 1) %>%

    # raw dates
    pull(value) %>%
    # take the range rather than unique in case of sections of data missing years
    range


  # seq every year from start to end
  switch(class(date_range)[1],
         Date = {
           out <- seq.Date(date_range[1], date_range[2], by = 'year')
         },
         POSIXct = {
           out <- seq.POSIXt(date_range[1], date_range[2], by = 'year')
         })

  out
}


#' Title
#'
#' for scale_x_datetime
#'
#' @inheritParams scale_x_datetime_decadal
#'
#' @return
#' @export
#'
#' @examples
create_date_breaks_decadal <- function(dates) {
  tibble::enframe(create_date_breaks_yearly(dates)) %>%
    filter(year(value) %% 10 == 0) %>%
    pull(value)

}



# Plot Preparation Helper Functions ---------------------------------------

# Function to help manipulate data for plotting


#' Title
#'
#' tests to write:
#'
#' @param values
#' @param interval
#' @param snap_lower_to_zero
#' @param snap_upper_to_zero
#' @param symmetrical Make scale symmetrical over zero
#'
#' Behaviour will be strange with symmetrical and snap set
#'
#' @return
#' @export
#'
#' @examples
#'
get_scale_limits_from_values <- function(values, interval = 1, snap_lower_to_zero = TRUE, snap_upper_to_zero = TRUE, symmetrical = FALSE) {
  if (any(is.na(values))) {
    warning("Parameter 'values' has NA values. These will be excluded from the calculations.")
  }

  range_of_values <- range(values, na.rm = TRUE)
  lower_val <- ifelse(snap_lower_to_zero, yes = min(0, range_of_values[1]), no = range_of_values[1])
  upper_val <- ifelse(snap_upper_to_zero, yes = max(0, range_of_values[2]), no = range_of_values[2])

  if (symmetrical) {
    mag <- max(abs(lower_val), abs(upper_val))
    lower_val <- -mag
    upper_val <- mag
  }

  c(floor(lower_val * (1/interval)) / (1/interval), ceiling(upper_val * (1/interval)) / (1/interval))
}



# Utility Functions -------------------------------------------------------

# Functions used to perform utility takes to facilitate higher level functions elsewhere

#' Load pre-calculated results from file if it exists, if not calculate results as save it to the file
#'
#' @param file Name of the file
#' @param FUN Function (or code block) to create the object that is to be stored and subsequently retrieved from the \code{file}
#' @param ARGS List of arguments to be passed to the function
#' @param force logical. If TRUE, then force recreation and saving of the object, even if file already exists. Set to FALSE by default.
#'
#' @return
#' @export
#'
#' @examples
create_or_read <- function(file, FUN, ARGS, force = FALSE) {
  if(force | !file.exists(file)) {
    logdebug("File does not exist. Running function:")
    result <- do.call(what = FUN, args = ARGS)
    loginfo(glue("Saving to {file}"))

    # save object, creating directory if it doesn't exist
    dir.create(dirname(file), showWarnings = FALSE, recursive = TRUE)
    saveRDS(result, file)
  } else {
    loginfo(glue("Reading from {file}"))
    result <- readRDS(file)
  }

  result
}


#' Create time series dataframe from a time series vector
#'
#' From a vector of POSIX dates, create a dataframe with additional columns derived from the supplied dates.
#'
#' @param time_series Vector of POSIX dates
#' @param add_Austral_columns Add columns for southern hemisphere year, month, and day of season (aka day of Austral year)
#'
#' @return Dataframe with supplied dates and additional date related columns
#' @export
#'
#' @examples
create_time_series_dataframe <- function(time_series, add_Austral_columns = TRUE) {

  if (add_Austral_columns) {
    time_df <- data.frame(
      date = time_series,
      year = NA, month = NA, day = NA, day_of_year = NA,
      Austral_season_year = NA,
      Austral_season_month = NA,
      Austral_day_of_season = NA,
      stringsAsFactors = FALSE
    )
  } else {
    time_df <- data.frame(
      date = time_series,
      year = NA, month = NA, day = NA, day_of_year = NA,
      stringsAsFactors = FALSE
    )
  }

  # account for POSIXlt offsets
  time_df$year  = as.integer(format.Date(time_series, "%Y"))
  time_df$month = as.integer(format.Date(time_series, "%m"))
  time_df$day   = as.integer(format.Date(time_series, "%d"))
  time_df$day_of_year = as.integer(format.Date(time_series, "%j"))


  if (add_Austral_columns) {
    time_df$Austral_season_year <- as.integer(ifelse(time_df$month <= 6, time_df$year - 1, time_df$year))
    time_df$Austral_season_month <- as.integer(ifelse(time_df$month <= 6, time_df$month + 6, time_df$month - 6))

    time_df$Austral_day_of_season <- as.integer(case_when(
      time_df$month > 6 & is_leap_year(time_df$year)==FALSE ~ c(time_df$day_of_year - 181),
      time_df$month > 6 & is_leap_year(time_df$year)==TRUE  ~ c(time_df$day_of_year - 182),
      time_df$month < 7 ~ c(time_df$day_of_year + 184)  # (184 is sum of Jul=31 + Aug=31 + Sep=30 + Oct=31 + Nov=30 + Dec=31)
    ))

    # correct data types of some columns to integer instead of double
    integer_cols <- c("year", "month", "day", "day_of_year",
                      "Austral_season_year", "Austral_season_month", "Austral_day_of_season")
  } else {
    # correct data types of some columns to integer instead of double
    integer_cols <- c("year", "month", "day", "day_of_year")
  }

  time_df[integer_cols] <- lapply(time_df[integer_cols], function (x) as.integer(x) )

  as.data.frame(time_df, stringsAsFactors = FALSE)
}


#' Extract and combine time values from multiple netcdf files.
#'
#' Time handling in any file format is constantly a pain. It can be described
#' in a range of different ways, relative to a range of different starting points.
#' For example, "Minutes since 1800-01-01 00:00:00", or "Seconds since 1960-01-01 00:00:00".
#' This tool is a wrapper around RNetCDF tools that extract and transform the time
#' representation in netcdf files into a useable, standard reference, human readable format.
#'
#' @param nc_files_list A list of files you wish to combine together, but have independently defined "time" variables.
#'
#' @return Returns a vector of character strings in the form of "YYYY-MM-DD hh:mm:ss".
#' This can then be used by POSIXct or POSIXlt to get date or time class objects.
#' @export
#'
#' @examples
#' files <- dir(path = "/path/to/nc/files/", pattern = "*.nc", full.names = TRUE)
#' ts <- extract_and_combine_time_from_netcdf_files(files)
extract_and_combine_time_from_netcdf_files <- function(nc_files_list){
  if (!requireNamespace("RNetCDF")) { stop("RNetCDF package required.") }

  out_time_POSIXct <- lapply(nc_files_list, function(target_file){
    tmp_nc <- RNetCDF::open.nc(target_file)
    tmp_time_unitstring <- RNetCDF::att.get.nc(ncfile = tmp_nc, variable = "time", attribute = "units")
    tmp_time_raw <- RNetCDF::var.get.nc(ncfile = tmp_nc, variable = "time")
    RNetCDF::close.nc(tmp_nc)
    tmp_time_string <- RNetCDF::utcal.nc(unitstring = tmp_time_unitstring, value = tmp_time_raw, type = "s")
  }) %>% unlist

  return(out_time_POSIXct)
}


#' Add cell orography information to a dataframe
#'
#' @param df Dataframe
#' @inheritParams common_parameter_descriptions
#'
#' @return
#' @export
#'
#' @examples
df_add_cell_orog_data <- function(df, cell_orog_lookup) {
  df %>%
    inner_join(y = cell_orog_lookup, by = c("cell" = "cell_number", "domain" = "domain")) %>%
    dplyr::select(-resolution_lon_km, -resolution_lat_km, -surface_height)
}



#' Calculate number of seconds per month
#'
#' This will take into account leap years
#'
#' @param year Year
#' @param month Month
#'
#' @return Integer
#' @export
#'
#' @examples
#' seconds_per_month(1979, 1)
#' seconds_per_month(1979, 2)
#' seconds_per_month(1979, 3)
#' seconds_per_month(1979, 4)
#' seconds_per_month(1980, 2)
seconds_per_month <- function(year, month) {

  if (range(month)[1] < 1 | range(month)[2] > 12) { stop(paste0("Invalid month: ", month)) }

  seconds_per_day <- 86400L  # 24 * 60 * 60

  n <- case_when(
    month == 1 ~ seconds_per_day * 31,
    month == 2 ~ seconds_per_day * 28 + ifelse(is_leap_year(year), seconds_per_day*1, 0),
    month == 3 ~ seconds_per_day * 31,
    month == 4 ~ seconds_per_day * 30,
    month == 5 ~ seconds_per_day * 31,
    month == 6 ~ seconds_per_day * 30,
    month == 7 ~ seconds_per_day * 31,
    month == 8 ~ seconds_per_day * 31,
    month == 9 ~ seconds_per_day * 30,
    month == 10 ~ seconds_per_day * 31,
    month == 11 ~ seconds_per_day * 30,
    month == 12 ~ seconds_per_day * 31,
    TRUE ~ 0
  )

  as.integer(n)
}


#' Day of year ignoring leap year
#'
#' Find the day of year for a date, where the leap day of Feb 29 is considered to have the same value as Feb 28.
#'
#' This is useful when analysing data across the day of year dimension, as it avoids a day offset of 1 for leap years.
#' Calculating aggregates based on the output of this function will group Feb 28 and Feb 29 together.
#' Any days after this will always have the same day of year value.
#'
#' i.e., a value of 365 is always Dec 31, rather than Dec 31 on non leap years, and Dec 30 on leap years.
#'
#' @param x
#'
#' @return
#' @export
#'
#' @examples
yday_ly <- function(x) {

  # feb 29 is 60th day of year
  # if leap year then
  #   map this to the same doy as feb 28,
  #   and subtract 1 from remaining days of year

  ifelse(
    test = leap_year(x),
    # if leap year
    yes = ifelse(test = yday(x) < 60,
                 # if before feb29 then yday normal
                 yes = yday(x),
                 no = ifelse(test = yday(x) == 60,
                             # if feb29 then yday same as feb28
                             # otherwise sub 1 from yday to shift to normal year result
                             yes = 59,
                             no = yday(x) - 1)
                 ),
    # not leap year
    no = yday(x)
  )
}


#' Check if year is a leap year
#'
#' The calculation is derived from the article here: http://en.wikipedia.org/wiki/Leap_year
#'
#' @param year Year or vector of to test for being a leap year
#'
#' @return TRUE or FALSE
#' @export
#'
#' @examples
#' is_leap_year(1999)  # FALSE
#' is_leap_year(2000)  # TRUE
#' is_leap_year(2100)  # FALSE
is_leap_year <- function(year) { return(((year %% 4 == 0) & (year %% 100 != 0)) | (year %% 400 == 0)) }


#' Check if vector has range of zero (i.e. all elements the same)
#'
#' Checks if range of vector is zero, , or length of vector is one
#'
#' As implemented here: https://stackoverflow.com/a/4752580
#'
#' @param x Vector
#' @param tol Tolerance, defaults to .Machine$double.eps (for high precision with doubles)
#'
#' @return TRUE or FALSE
#' @export
#'
#' @examples
#' is_zero_range_vector(c(1, 2, 3))
#' is_zero_range_vector(c(3.14, 3.14, 3.14))
is_zero_range_vector <- function(x, tol = .Machine$double.eps ^ 0.5) {
  if (length(x) == 1) return(TRUE)
  x <- range(x) / mean(x)
  isTRUE(all.equal(x[1], x[2], tolerance = tol))
}



#' Title
#'
#' ggnewscale uses the built in function isFALSE (for R >= 3.5)
#' so make a replacement
#' @param x
#'
#' @return
#' @export
#'
#' @examples
isFALSE <- function(x) {
  identical(x, FALSE)
}



#' Convert a "label" string to a "code" string
#'
#' String will be converted to lowercase with underscores in place of spaces, and punctuation removed
#'
#' @param label Label string to convert
#'
#' @return String
#' @export
#'
#' @examples
#' label_to_code('Tasmania East Coast')  # tasmania_east_coast
#' label_to_code("This/That won't be the Other")  # this_that_wont_be_the_other
#' label_to_code('Description (Extra Info)')  # description_extra_info
#' label_to_code('Description - Extra Info')  # description_extra_info
label_to_code <- function(label) {
  str_to_lower(label) %>%
    # remove brackets first, so no double underscores
    str_remove_all(pattern = '\\(|\\)') %>%
    str_remove_all(pattern = '\\[|\\]') %>%
    str_remove_all(pattern = '\\{|\\}') %>%
    # remove some punctuation, so no double underscores
    str_remove_all(pattern = '"') %>%
    str_remove_all(pattern = "'") %>%
    # replace all non-word characters with underscores
    str_replace_all(pattern = '\\W', replacement = '_') %>%
    # replace multiple underscores with one
    str_replace_all(pattern = '_+', replacement = '_') %>%
    # dont end with an underscore
    sub('_$', '', .)
}


#' Title
#'
#' @param file
#'
#' @return
#' @export
#'
#' @examples
read_part_of_filename_model <- function(file) {
  read_part_of_filename(file, 1)
}


#' Read part from output of extract and calc
#'
#' @param file
#' @param n
#'
#' @return
#' @export
#'
#' @examples
read_part_of_filename <- function(file, n, pattern = '_') {
  str <- basename(file)
  split <- str_split(string = str, pattern = pattern)

  lapply(split, function(entry) entry[n]) %>% unlist
}



#' Title
#'
#' @param col
#'
#' @return
#' @export
#'
#' @examples
col2hex <- function(col) {
  col_rgb <- col2rgb(col)

  rgb(red = col_rgb[1,] / 255L,
      green = col_rgb[2,] / 255L,
      blue = col_rgb[3,] / 255L)
}


#' Lighten or darken colours
#'
#' desc
#'
#' details
#'
#' @rdname colour_lightener
#' @param cols List or vector of colours
#' @param x Factor to adjust colour by. A value of 1 will return the original colour;
#' for \code{colour_lightener} a value of 0 will return white,
#' and for \code{colour_darkener} a value of 0 will return black,
#'
#' @return
#' @export
#'
#' @examples
colour_lightener <- function(cols, x = 0.5) {

  if (!between(x, 0, 1)) { stop("Factor must be between 0 and 1") }

  crp_steps <- 200

  crp_idx <- if (x == 0) {
    1  # account for no zero element
  } else {
    x * (crp_steps/2)
  }

  lapply(cols, function(c) colorRampPalette(c("white", c, "black"))(crp_steps)[crp_idx]) %>% unlist
}


#' @export
#' @rdname colour_lightener
colour_darkener <- function(cols, x = 0.5) {

  if (!between(x, 0, 1)) { stop("Factor must be between 0 and 1") }

  crp_steps <- 200

  crp_idx <- if (x == 0) {
    crp_steps  # account for no zero element
  } else {
    crp_steps - x * (crp_steps/2)
  }

  lapply(cols, function(c) colorRampPalette(c("white", c, "black"))(crp_steps)[crp_idx]) %>% unlist
}



#' Title
#'
#' @param database
#'
#' @return
#' @export
#'
#' @examples
borders_cfdtk <- function(database) {

  if (database == 'australia') {
    australia_file <- file.path(getwd(), 'data', 'borders', 'australia.shp')
    sf_object <- read_sf(australia_file)
  } else {
    state_boundaries_file <- file.path(getwd(), 'data', 'borders', 'state_boundaries.shp')
    sf_object <- read_sf(state_boundaries_file) %>% filter(STATE_NAME == database)
  }

  sf_object
}


# Viewing Functions -------------------------------------------------------

#' Quickly view a large object in the data viewer
#'
#' By default the first 50 rows of an object will be shown.
#'
#' This is intended as a quick way to type this in the console,
#' by typing the name of a dataframe and piping it to \code{QV} using the RStudio keyboard shortcut Ctrl+Shift+m.
#'
#' @inheritParams utils::View
#' @param n Number of rows to show in data viewer
#'
#' @export
#'
#' @examples QuickView(iris)
QuickView <- function(x, n = 50, title = NULL) {
  if (is.null(title)) { title = deparse(substitute(x)) }
  View(head(x, n), title)
}


#' @rdname QuickView
#' @examples QV(iris)
#' @export
QV <- QuickView


#' Return the First and Last Parts of an Object
#'
#' @inheritParams utils::head
#'
#' @return Combined output of \code{head} and \code{tail}
#' @export
#'
#' @examples
#' headtail(datasets::iris)
headtail <- function(x, n = 6L, ...) {
  h <- head(x, n, ...)
  t <- tail(x, n, ...)
  bind_rows(h, t)
}


# Logging Functions -------------------------------------------------------

# Functions that augment and extend the capabilities of the logging package

#' Quickly convert an array to a comma separated string for logging
#'
#' This function is a convenience wrapper for the \code{paste} function with a default \code{collapse} parameter set.
#'
#' @param array Array to log
#' @param collapse Character string to separate the results. Default is ' ,' (comma with a space)
#'
#' @return Values of the array concatenated into a single string, with the elements being separated by the value of \code{collapse}.
#' @export
#'
#' @examples
#' array_to_log <- c('this', 'that', 'other')
#' loginfo(glue("The values are: {logarray(array_to_log)}"))
#' loginfo(paste0("The values are: ", logarray(array_to_log)))
logarray <- function(array, collapse = ', ') {
  paste(array, collapse = collapse)
}


#' Log and display the time taken for tasks to complete
#'
#' Provides a set of tools to allow easier reporting on the time taken to perform intermediate tasks within a larger body of code.
#'
#' First, initialise new logtime object with the \code{logtime_init} method.
#' Then, before starting a task to measure the time taken,
#' use \code{logtime} to append a 'start' timestamp for a given name (the \code{log_name}) to refer uniquely to this task.
#' After the task has completed,
#' use \code{logtime} again to append an 'end' timestamp for the same \code{log_name}.
#'
#' To print the time difference between the start and end times of a log entry, use the \code{logtime_print_diff} method.
#' This will use the most suitable measurement of time to represent the elapsed time to make the entry readable.
#'
#' To retrieve the time difference between the start and end times of a log entry in seconds, use the \code{logtime_secs_diff} method.
#' This is sdesigned to be used for tests on the time difference.
#'
#' @param time_log An existing logtime object
#' @param log_name Name of the log entry
#' @param log_type State of the log entry, either 'start' or 'end'
#'
#' @export
#'
#' @examples
#' time_log <- logtime_init()
#' time_log <- logtime(time_log, 'my_log_name', 'start')
#' # do some things
#' # that take time
#' time_log <- logtime(time_log, 'my_log_name', 'end')
#' logtime_print_diff(time_log, 'my_log_name')
logtime <- function(time_log, log_name, log_type) {

  VALID_LOG_TYPE_VALUES <- c('start', 'end')
  if (!(log_type %in% VALID_LOG_TYPE_VALUES)) {
    stop(glue("Error: log_type must be one of the following: {paste(VALID_LOG_TYPE_VALUES, collapse=', ')}"))
  }

  time_log_key <- glue("{log_name}_{log_type}")
  time_log[[time_log_key]] <- Sys.time()
  time_log
}


#' @param digits How many significant digits are to be used for printed value
#' @rdname logtime
#' @export
logtime_print_diff <- function(time_log, log_name, digits = 4) {
  end_time <- time_log[[glue("{log_name}_end")]]
  start_time <- time_log[[glue("{log_name}_start")]]

  if (is.null(start_time)) {
    logwarn(glue("start logtime for {log_name} is NULL"))
  } else if (is.null(end_time)) {
    logwarn(glue("end logtime for {log_name} is NULL"))
  } else {
    format(difftime(end_time, start_time), digits = digits)
  }
}


#' @rdname logtime
#' @export
logtime_secs_diff <- function(time_log, log_name) {
  end_time <- time_log[[glue("{log_name}_end")]]
  start_time <- time_log[[glue("{log_name}_start")]]

  if (is.null(start_time)) {
    logwarn(glue("start logtime for {log_name} is NULL"))
  } else if (is.null(end_time)) {
    logwarn(glue("end logtime for {log_name} is NULL"))
  } else {
    difftime(end_time, start_time, units = 'secs')
  }
}


#' @rdname logtime
#' @export
logtime_init <- function() {
  time_log <- list()
  time_log[['init']] <- Sys.time()
  time_log
}


#' Build and/or log string describing current progress of a loop
#'
#' Helper function to build a string to be logged that describes the progress within a loop (e.g. for, lapply, ...).
#'
#' \code{progress_of_loop} will return a string matching the format "(x/y) element" will be returned,
#' where 'x' is the index of the current element in the list,
#' 'y' is the total number of elements,
#' and 'element' is the element itself.
#'
#' \code{loginfo_progress} will log a string as produced by \code{progress_of_loop} to the info logger.
#'
#' @param element Current element of the loop
#' @param all_elements All elements that will be iterated over
#' @param FUN Function to pass element to before logging. For example, \code{basename} will shorten the log line for a file with a long path that is assumed or logged previously.
#'
#' @return String
#' @export
#'
#' @examples
#' for (letter in letters) {
#'   loginfo(progress_of_loop(letter, letters))
#'   # do something
#' }
#'
#' lapply(letters, function(letter)  {
#'   loginfo(progress_of_loop(letter, letters))
#'   stringr::str_to_upper(letter)
#' }) %>% unlist
progress_of_loop <- function(element, all_elements, FUN = as.character) {
  i <- match(element, all_elements)
  total <- length(all_elements)
  element_text <- do.call(FUN, list(element))

  glue("({i}/{total}) {element_text}") %>% toString
}


#' @rdname progress_of_loop
#' @param n Every \code{n}th element will be logged. The first and last element are always logged. Useful for loops with a large number of iterations.
#'
#' @export
#'
#' @examples
loginfo_progress <- function(element, all_elements, n = 1, FUN = as.character) {
  i <- match(element, all_elements)

  # if FUN is basename, then log the dirname on the first iteration
  if (i == 1) {
    FUN_name <- substitute(FUN)

    if (length(FUN_name) == 1) {
      # FUN = basename
      if (FUN_name == 'basename') {
        loginfo(dirname(element))
      }
    } else {
      # FUN = base::basename
      if (FUN_name[3] == 'basename') {
        loginfo(dirname(element))
      }
    }
  }

  # always log if n == 1, log on the first iteration, every nth iteration, and every last iteration
  if (n == 1 || i == 1 || i %% n == 0 || i == length(all_elements)) {
    loginfo(progress_of_loop(element, all_elements, FUN = FUN))
  }
}


# Common Parameter Descriptions -------------------------------------------

#' @name common_parameter_descriptions
#' @title Common parameter descriptions
#' @description A placeholder to contain descriptions for parameters that are used in multiple functions
#' @keywords internal
#'
#' @param target_cells_lookup Pre-calculated lookup listing the cells that exist in each region for each domain
#' @param cell_orog_lookup Cell orography lookup, as created by \code{create_cell_orography_lookup}
#' @param target_domains_files Named list defining each domain to be assessed with an example NetCDF file with the desired projection for each domain
#' @param lat Latitude, in decimal degrees
#' @param lon Longitude, in decimal degrees
#' @param proj4string A proj4string describing coordinate reference system to be used (set to "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0" by default
#' @param field_names A named list with the following names: 'index', 'label', and 'code'
#' @param preferred_domains Optional. A named list defining the preferred domain for each polygon.
#' If the polygons are spread across multiple domains, then the preferred domain for each polygon must be specified explictly to distinguish between those that overlap.
#' Use the code for the polygon (as will be created in \code{convert_all_polygons_to_multipolygons}) as the key, and the doman as the value.
#' If only one domain is present in the \code{target_domains_files} parameter, then this list is not required.
NULL


#' @name common_defaults
#' @title Common default values
#' @description A placeholder to contain default values for parameters that are used in multiple functions
#' @keywords internal
#'
common_defaults <- list(
  # NOTE: if this proj4string ever changes,
  #       be sure to update the documented value in common_parameter_descriptions
  proj4string = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0"
)
