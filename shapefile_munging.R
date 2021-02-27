get_water <- function(state_abbr,
                      county,
                      year,
                      refresh,
                      class="sf"){
  the_water <- area_water(state_abbr, 
                          county = county, 
                          year = year,
                          refresh = refresh,
                          class = "sf")  %>% 
    select("geometry") %>% 
    add_column(., COUNTYFP=county,.before=1) %>% 
    st_transform(., crs=4326)
  return(the_water)
}


get_tracts <- function(state_abbr, 
                       year=NULL, 
                       refresh=FALSE, 
                       remove_water=TRUE){
  # if we have saved them, then load them if refresh is FALSE
  print(paste0("getting shapefile for tracts in ",state_abbr))
  state_tracts <- tracts(cb = TRUE, 
                         year = year, 
                         class="sf", 
                         state=state_abbr, 
                         refresh=refresh) %>% 
    st_transform(., crs=4326) %>% st_make_valid()
  Sys.sleep(1)
  if(!remove_water){
    return(state_tracts)
  }
  print(paste0("fetching water boundaries for ",state_abbr))
  
  county_codes <- unique(state_tracts$COUNTYFP)
  
  print(paste0("joining water areas into one shapefile for ",state_abbr))
  
  # https://community.rstudio.com/t/better-ways-to-remove-areas-of-water-from-us-map/60771
  # https://gis.stackexchange.com/questions/308119/function-in-r-to-get-difference-between-two-sets-of-polygons-comparable-in-speed
  water_area <- 
    map_dfr(
      county_codes,
      ~get_water(
        state_abbr=state_abbr,
        county=.,
        year=year,
        refresh=refresh,
        class="sf"
      )
    )
  print(paste0("subtracting water from census tracts for ",state_abbr))
  pb <- txtProgressBar(min=0, 
                       max=nrow(state_tracts),
                       style=3)
  current_county_num <- -1
  for(i in 1:nrow(state_tracts)){
    next_county_num <- as.numeric(st_drop_geometry(state_tracts)[i,c("COUNTYFP")])
    if(next_county_num!=current_county_num){
      county_water_area <- water_area %>%
        filter(`COUNTYFP`==next_county_num)
      current_county_num <- next_county_num
    }
    if(nrow(county_water_area)>0){
      state_tracts[i,] <- ms_erase(state_tracts[i,], 
                                   county_water_area, 
                                   remove_slivers=TRUE)
    }
    setTxtProgressBar(pb, i)
  }
  return(state_tracts)
}

get_county_water <- function(df_row, 
                             year=NULL, refresh=FALSE){
  state_code <- as.numeric(df_row[1])
  # print(state_code)
  county_code <- as.numeric(df_row[2])
  # print(county_code)
  
  county_water <- area_water(state=state_code,
                             county=county_code,
                             year=year,
                             refresh=refresh)
  Sys.sleep(0.25)
  return(county_water)
}

get_state_water <- function(state_abbr, 
                            year=NULL, refresh=FALSE){
  print(paste0("getting water for ",state_abbr))
  
  state_codes <- fips_codes[fips_codes$state==state_abbr,
                            c("state_code","county_code")]
  
  county_waters <- apply(state_codes, 1, get_county_water, 
                       year=year, 
                       refresh=refresh)
  
  state_water <- county_waters %>% st_union()
  
  Sys.sleep(0.5)
  return(state_water)
}

library(ggmap)
# https://stackoverflow.com/questions/47749078/how-to-put-a-geom-sf-produced-map-on-top-of-a-ggmap-produced-raster/50844502#50844502
ggmap_bbox <- function(map) {
  if (!inherits(map, "ggmap")) stop("map must be a ggmap object")
  # Extract the bounding box (in lat/lon) from the ggmap to a numeric vector, 
  # and set the names to what sf::st_bbox expects:
  map_bbox <- setNames(unlist(attr(map, "bb")), 
                       c("ymin", "xmin", "ymax", "xmax"))
  
  # Coonvert the bbox to an sf polygon, transform it to 3857, 
  # and convert back to a bbox (convoluted, but it works)
  bbox_3857 <- st_bbox(st_transform(st_as_sfc(st_bbox(map_bbox, crs = 4326)), 3857))
  
  # Overwrite the bbox of the ggmap object with the transformed coordinates 
  attr(map, "bb")$ll.lat <- bbox_3857["ymin"]
  attr(map, "bb")$ll.lon <- bbox_3857["xmin"]
  attr(map, "bb")$ur.lat <- bbox_3857["ymax"]
  attr(map, "bb")$ur.lon <- bbox_3857["xmax"]
  map
}

geom_sf_rast <- function(layer, 
                              dpi = NULL,
                              dev = NULL){
  layer <- lapply(layer, function(x) {
    if (inherits(x, "LayerInstance")) {
      x <- ggrastr::rasterise(x, dpi=dpi,dev=dev)
    }
    return(x)
  })
  return(layer)
}