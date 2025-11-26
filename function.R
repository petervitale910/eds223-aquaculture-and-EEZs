#' Map EEZ
#' Make a map of a suitable area of a species in west coast EEZ's
#' 
#' @param species 
#' The name of your species, MUST be wrapped eg: ('oysters')
#' @param t_lwr 
#' Lower temperature, integer.
#' @param t_upr 
#' Upper temperature, integer
#' @param d_lower 
#' Lower depth, positive integer of your deepest depth
#' @param d_upr 
#' Upper depth, positive integer of your shallowest depth
#' @returns
#' A Map of west coast eez's and a kable table?
#' @export
#'
#' @examples
#' map_eez('Bull Kelp', 10, 18, 40, 0)
map_eez <- function(species, t_lwr, t_upr, d_lower, d_upr){

rcl_depth <- matrix(c(-Inf, -d_lower, 0,
                      -d_lower, -d_upr, 1,
                      -d_upr, Inf, 0), 
                    nrow = 3, 
                    byrow = TRUE)

rcl_temp <- matrix(c(-Inf, t_lwr, 0,
                     t_lwr, t_upr, 1,
                     t_upr, Inf, 0), 
                   nrow =3,
                   byrow = TRUE)

depth_rcl <- classify(depth_resampled, rcl_depth)

sst_rcl <- classify(mean_sst, rcl_temp)

suitable_cells <- sst_rcl * depth_rcl

suitable_cells[suitable_cells == 0] <- NA

names(suitable_cells) <- 'Suitable'

extracted <- terra::extract(suitable_cells, eez, touches = TRUE)

extracted_counts <- extracted %>% 
  group_by(ID) %>% 
  summarise(count = sum(Suitable, na.rm = TRUE))

area_raster <- suitable_cells * cellSize(suitable_cells, unit = 'km')

# Extract suitable areas area
area_extracted <- terra::extract(area_raster, 
                                 eez, 
                                 touches = TRUE) %>%  
  group_by(ID) %>% 
  summarise(suitable_area = sum(Suitable, na.rm = TRUE)) %>% 
  mutate(rgn = eez$rgn)


region_suitable_area <- left_join(x = eez,
                                  y = area_extracted, 
                                  by = "rgn") %>% 
  select(-ID)


title <- paste0('Ideal locations for farming', species, 'in west coast EEZ\'s')

map <- tm_basemap("Esri.WorldTopoMap")+
  tm_shape(region_suitable_area)+
  tm_polygons(fill = 'suitable_area',
              fill.scale = tm_scale(values = c('#E1DABD',
                                               '#ABC798',
                                               '#FFC4EB',
                                               '#E6983F',
                                               '#A0185A'),
                                    breaks = c(0, 800, 1600,
                                               2400, 3200,4000)),
              fill.legend = tm_legend( 'Suitable area (km)'))+
  tm_title(text = paste('Suitable area for farming', species, 'in West Coast EEZ'), position = tm_pos_out("center", "top", pos.h = "center"))+
  tm_layout(component.autoscale = FALSE)+
  tm_graticules(alpha = .5)+
  tm_text('rgn', size = .7)


return(map)
}
