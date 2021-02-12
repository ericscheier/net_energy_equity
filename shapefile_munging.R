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
  # state_tracts_combined <- rbind_tigris(state_tracts)
  county_codes <- unique(state_tracts$COUNTYFP)
  
  # https://rdrr.io/cran/tigris/man/rbind_tigris.html
  # aw <- lapply(county_codes, function(x) {
  #   area_water(state=state_abbr,
  #              county=x,
  #              year=year,
  #              refresh=refresh)
  # })
  print("joining water areas into one shapefile")
  # water_area <- rbind_tigris(aw) %>% st_union()
  
  # https://community.rstudio.com/t/better-ways-to-remove-areas-of-water-from-us-map/60771
  # https://gis.stackexchange.com/questions/308119/function-in-r-to-get-difference-between-two-sets-of-polygons-comparable-in-speed
  water_area <- 
    map_dfr(
      county_codes,
      ~ area_water(state_abbr, 
                   county = ., 
                   year = year,
                   refresh = refresh,
                   class = "sf")  %>% 
        st_transform(., crs=4326) %>% 
        st_make_valid() #%>% 
        # filter(AWATER > 10^8)
    ) #%>%
    #st_union()
  print("subtracting water areas from census tract areas")
  # state_tracts_sans_water <- st_difference(state_tracts, st_union(water_area))
  state_tracts_sans_water <- ms_erase(state_tracts, water_area)
  Sys.sleep(1)
  return(state_tracts_sans_water)
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