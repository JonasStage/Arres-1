source("Library.R")

wms_url <- "https://api.dataforsyningen.dk/orto_foraar_DAF?token=3d6237caab0287976f475f68c890a359"
#### Get Arresø catchment ####
read_sf("/Users/jonas/Library/CloudStorage/OneDrive-SyddanskUniversitet/danmark_søer_co2_flux/catchments_simple.sqlite") -> catchments

read_sf("/Users/jonas/Library/CloudStorage/OneDrive-SyddanskUniversitet/danmark_søer_co2_flux/data/lakes.sqlite") -> lakes

tibble(x = 693898, y = 6209574) %>% 
  st_as_sf(coords = c("x","y"), crs = st_crs(25832)) %>% 
  st_filter(lakes,.) -> arre

eng_lakes <- tibble(lat = c(55.97912278575208,55.99368260230246,55.95968629353999), 
                    lng = c(12.21625658864829,12.252469517131345,12.263313800133842),
                    lake = c("Alsønderup","Solbjerg","Strødam")) %>%
  {. ->> eng_coordinates} %>% 
  st_as_sf(coords = c("lng","lat"), crs = st_crs(4326)) %>% 
  st_transform(st_crs(25832)) %>% 
  st_filter(lakes,.) %>% 
  st_transform(st_crs(4326)) %>% 
  add_column(lake = c("Solbjerg","Alsønderup","Strødam"))

catchments %>% 
  filter(gml_id == arre$gml_id) %>% 
  {. ->> arre_catchment_utm} %>% 
  st_transform(st_crs(4326))-> arre_catchment

read_sf("Data/vandloebsflade.gpkg") %>% 
  filter(objectid %in% c(102450,152846,81896,150133,
                         111114,108743,135410,133027,
                         141249,118217,80045,
                         145857,153344,
                         113362)) %>% 
  st_transform(st_crs(4326)) -> arre_streams

eng_coordinates %>% 
  filter(!lake %in% c("Strødam","Solbjerg")) %>% 
  add_row(lat = c(55.928066079776144,56.020837415439885,55.971066341532286,55.97165936758006,55.935346484786685,55.95637213382005,55.972148087969,55.995686970108686,55.96114609091519), 
          lng = c(12.298768247880266,12.196701441896485,12.12565324512546, 12.019057220908195, 12.08466828422201, 12.174956143254057, 12.190851545120015,12.265278925208303, 12.270568433638076), 
          lake = c("Hillerød","Helsinge","Arresø","Frederiksværk", "Lyngby Å", "Æbelholt Å","Pøle Å","Solbjerg","Strødam")) %>% 
  mutate(lat = case_when(lake == "Strødam" ~ lat-0.01,
                         lake == "Lyngby Å" ~ lat-0.02,
                         lake == "Helsinge" ~ lat-0.015,
                         lake == "Æbelholt Å" ~ lat-0.03,
                         lake == "Solbjerg" ~ lat-0.01,
                         lake == "Alsønderup" ~ lat-0.01,
                         lake == "Pøle Å" ~ lat-0.015,
                         T ~ lat ),
         lng = case_when(lake == "Lyngby Å" ~ lng+0.04,
                         lake == "Æbelholt Å" ~ lng-0.01,
                         lake == "Solbjerg" ~ lng +0.03,
                         lake == "Strødam" ~ lng +0.03,
                         lake == "Alsønderup" ~ lng+0.06,
                         lake == "Pøle Å" ~ lng+0.02,
                         T ~ lng)) -> places

#### Get WMS and laeflet ####

leaflet() %>%
  addWMSTiles(
    wms_url,
    layers = "geodanmark_2023_12_5cm", 
    options = c(WMSTileOptions(format = "image/png", transparent = T),
                providerTileOptions(minZoom = 0.1, maxZoom = 100))) %>% 
  addPolygons(data = arre_catchment,stroke = F, fillOpacity = 0.6, col = "white") %>% 
  addPolylines(data = arre_streams, opacity = 1) %>% 
  addLabelOnlyMarkers(data = places, lat = ~lat, lng = ~lng, label = ~lake,
                          labelOptions = labelOptions(noHide = T, direction = 'top', textOnly = T,
                                                      style=list('fontSize'="25px"))) %>% 
  addPolygons(data = eng_lakes, stroke = T,fillOpacity = 1, color = "black", fillColor = "blue", opacity = 1,smoothFactor = 0.5) %>% 
  setView(12.1903,55.98221,zoom = 11.)-> catchment_map;catchment_map 

mapshot(catchment_map, file = "Manuscript/Vand og Jord/Figures/figure 1.png",remove_url = TRUE)
