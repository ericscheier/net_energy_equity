source("sources.R")

metric_name <- "energy_burden"

energy_burden_poverty_line <- 0.06
ner_poverty_line <- ner_func(g = 1,s = energy_burden_poverty_line)
metric_cutoff_level <- energy_burden_poverty_line #(ner_poverty_line)^-1

metric_long_name <- bquote(E[b]) #bquote(E[b]^n)
metric_label <- "%"

# as.expression(bquote(~italic(nE[b])~": Energy Burden"))


clean_data_ami_all <- read_csv("CohortData_AreaMedianIncome.csv")
clean_data_fpl_all <- read_csv("CohortData_FederalPovertyLine.csv")
replica_sup <- read_csv("CensusTractData.csv") #get_replica_supplemental_dataset()

census_tracts_shp <- get_tract_shapefiles(
  states="all",
  acs_version=2018,
  refresh=TRUE,
  return_files=TRUE,
  all_from_list=unique(replica_sup$state_abbr)
)


clean_data_ami_all$neb <- clean_data_ami_all$ner^-1
clean_data_fpl_all$neb <- clean_data_fpl_all$ner^-1

# clean_data_ami_all$neb <- ifelse(is.finite(clean_data_ami_all$neb),clean_data_ami_all$neb,max(clean_data_ami_all$neb, na.rm = T))
# clean_data_fpl_all$neb <- clean_data_fpl_all$ner^-1

clean_data_ami_all_sup <- left_join(clean_data_ami_all, replica_sup, by=c("geoid","state_abbr",
                                                                          "state_fips",
                                                                          "company_ty",
                                                                          "locale"))

clean_data_fpl_all_sup <- left_join(clean_data_fpl_all, replica_sup, by=c("geo_id"="geoid",
                                                                          "state_abbr",
                                                                          "state_fips"
))

clean_data_ami_all_sup_shp <- st_sf(left_join(census_tracts_shp, clean_data_ami_all_sup, by=c("gisjoin")))
clean_data_fpl_all_sup_shp <- st_sf(left_join(census_tracts_shp, clean_data_fpl_all_sup, by=c("gisjoin")))

# Filter by energysheds

clean_data_ami_all_sup_shp_scp <- clean_data_ami_all_sup_shp %>% filter(state_abbr=="CA", 
                                                                        county_name %in% 
                                                                          c("Sonoma County",
                                                                            "Mendocino County"),
                                                                        company_na %in% c("Pacific Gas & Electric Co",
                                                                                          "Trinity Public Utilities District")
)

clean_data_fpl_all_sup_shp_scp <- clean_data_fpl_all_sup_shp %>% filter(state_abbr=="CA", 
                                                                        county_name %in% 
                                                                          c("Sonoma County",
                                                                            "Mendocino County"),
                                                                        company_na %in% c("Pacific Gas & Electric Co",
                                                                                          "Trinity Public Utilities District")
)


# C:\Users\Eric Scheier\Documents\apps\net_energy_equity\f8612018\Utility_Data_2018.xlsx
nerc_regions_states <- read_excel("f8612018/Utility_Data_2018.xlsx", sheet = "States", skip = 1)
nerc_regions_territories <- read_excel("f8612018/Utility_Data_2018.xlsx", sheet = "Territories", skip = 1)
# merge states and territories tabs
nerc_regions <- bind_rows(nerc_regions_states, nerc_regions_territories)
# join with clean data on state_abbr and eia utility number
nerc_regions$`Utility Number` <- as.character(nerc_regions$`Utility Number`)
clean_data_ami_all_sup_shp <- clean_data_ami_all_sup_shp %>% 
  left_join(nerc_regions, by=c("state_abbr"="State","eia_id"="Utility Number"))
# limit to census tracts in SERC
clean_data_ami_all_sup_shp_serc <- clean_data_ami_all_sup_shp %>% 
  filter(
    `NERC Region`=="WECC"
  )

# nerc_regions <- sf::st_read("nerc_regions.gdb")
# 
# st_intersects(clean_data_ami_all_sup_shp, nerc_regions %>% filter(NAME=="SERC RELIABILITY CORPORATION (SERC)") %>% st_transform(., 4326))

# clean_data_ami_all_sup_shp_serc <- clean_data_ami_all_sup_shp %>% 
#   filter(state_abbr %in% 
#            c("AL", #Alabama, 
#              "GA", #Georgia, 
#              "MS", #Mississippi, 
#              "MO", #Missouri, 
#              "NC", #North Carolina, 
#              "SC", #South Carolina, 
#              "TN", #Tennessee, 
#              #and portions of 
#              "AR", #Arkansas, 
#              "IL", #Illinois, 
#              "KY", #Kentucky, 
#              "LA", #Louisiana, 
#              "OK", #Oklahoma, 
#              "TX", #Texas, 
#              "VA", #Virginia, 
#              "FL" #Florida, 
#            )
#   )


clean_data_ami_all_sup_shp_continent <- 
  clean_data_ami_all_sup_shp %>% 
  filter(!(state_abbr %in% c("HI","AK", NA)))


code <- qr_code("https://www.emergi.eco/poverty-poster/")
# print(code)
# plot(code)
generate_svg(code, 
             filename = "qr.svg", 
             foreground="black",
             background="white",
             show=FALSE)

# SCP subset map
poster_charts_scp <- make_all_charts(
  clean_data=clean_data_ami_all_sup_shp_scp, #clean_data_ami_all_sup_shp, 
  group_columns=c(NULL),
  metric_name=metric_name, 
  metric_long_name=metric_long_name,
  metric_label=metric_label,
  metric_cutoff_level = metric_cutoff_level,
  metric_cutoff_label= NULL,
  weighted_metrics_data = clean_data_ami_all_sup_shp,
  upper_quantile_view=0.95,
  lower_quantile_view=0.05,
  border_field = "tract_fips",
  include_basemap = "toner-lite",
  keep_coordinates=TRUE,
  infer_missing="max"
)

ggsave("scp_poster_map.svg", 
       plot=(poster_charts_scp[["choropleth"]]  + theme(text = element_text(size = 20))),
       device = NULL,
       # path = NULL,
       scale = 1,
       width = 9,
       height = 6,
       units = "in", #c("in", "cm", "mm", "px"),
       dpi = 300,
       limitsize = TRUE,
       bg = NULL
)
# print(poster_charts_scp[["choropleth"]])


# SERC Subset Map
poster_charts_serc <- make_all_charts(
  clean_data=clean_data_ami_all_sup_shp_serc, #clean_data_ami_all_sup_shp, 
  group_columns=c(NULL),
  metric_name=metric_name, 
  metric_long_name=metric_long_name,
  metric_label=metric_label,
  metric_cutoff_level = metric_cutoff_level,
  metric_cutoff_label= NULL,
  weighted_metrics_data = clean_data_ami_all_sup_shp,
  upper_quantile_view=0.95,
  lower_quantile_view=0.05,
  border_field = "state_fips",
  include_basemap = "toner-lite",
  keep_coordinates=TRUE,
  infer_missing="max"
)

ggsave("serc_poster_map.svg", 
       plot=(poster_charts_serc[["choropleth"]] + 
               theme(
                 text = element_text(size = 20),
                 legend.position = "none"
                 )
             ),
       device = NULL,
       # path = NULL,
       scale = 1,
       width = 9,
       height = 6,
       units = "in", #c("in", "cm", "mm", "px"),
       dpi = 300,
       limitsize = TRUE,
       bg = NULL
)

# print(poster_charts_serc[["choropleth"]])
# print(poster_charts[["choropleth"]])

poster_charts <- make_all_charts(
  clean_data=clean_data_ami_all_sup_shp_continent, #clean_data_ami_all_sup_shp_scp, 
  group_columns=c(NULL),
  metric_name=metric_name, 
  metric_label=metric_label,
  metric_long_name=metric_long_name,
  metric_cutoff_level = metric_cutoff_level,
  metric_cutoff_label= NULL,
  weighted_metrics_data = clean_data_ami_all_sup_shp,
  
  upper_quantile_view=0.95,
  lower_quantile_view=0.05,
  
  chart_title="Communities Highlighted in Orange+Red Experience Above Average Energy Burdens",
  chart_subtitle=NULL,
  chart_caption=NULL,
  x_label="Proportion of Households",
  
  border_field = "state_fips",
  include_basemap = FALSE, #"toner-hybrid"
  keep_coordinates = FALSE,
  infer_missing="max"
)


ggsave("poster_map.svg", 
       plot=(poster_charts[["choropleth"]] + 
               theme(
                 text = element_text(size = 25),
                 title = element_text(color="white"),
                 legend.title = element_text(color="black")
                 )
             ),
       device = NULL,
       # path = NULL,
       scale = 1,
       width = 18,
       height = 12,
       units = "in", #c("in", "cm", "mm", "px"),
       dpi = 300,
       limitsize = TRUE,
       bg = NULL
)

energy_poverty_prevalence <- as.numeric((poster_charts$metrics$household_count/(poster_charts$metrics$household_count-poster_charts$metrics$households_below_cutoff)))

neb_fpl_wm <- calculate_weighted_metrics(st_drop_geometry(clean_data_fpl_all_sup_shp), 
                                         group_columns=c("income_bracket"), 
                                         metric_name="neb", 
                                         metric_cutoff_level=metric_cutoff_level, 
                                         upper_quantile_view=1.0, 
                                         lower_quantile_view=0.0)


fpl_energy_poverty_prevalence <- as.numeric(((poster_charts$metrics$household_count-poster_charts$metrics$households_below_cutoff))/(neb_fpl_wm[neb_fpl_wm$income_bracket=="Above Federal Poverty Line", "household_count"] - neb_fpl_wm[neb_fpl_wm$income_bracket=="Above Federal Poverty Line", "households_below_cutoff"]))

neb_fuel_wm <- calculate_weighted_metrics(st_drop_geometry(clean_data_fpl_all_sup_shp), 
                                          group_columns=c("super_simplified_primary_heating_fuel"), 
                                          metric_name="neb", 
                                          metric_cutoff_level=metric_cutoff_level, 
                                          upper_quantile_view=1.0, 
                                          lower_quantile_view=0.0)

solar_energy_poverty_denominator <- as.numeric((1-(neb_fuel_wm[neb_fuel_wm$super_simplified_primary_heating_fuel=="Other","households_below_cutoff"]/neb_fuel_wm[neb_fuel_wm$super_simplified_primary_heating_fuel=="Other","household_count"]))/(1-(neb_fuel_wm[neb_fuel_wm$super_simplified_primary_heating_fuel=="Solar","households_below_cutoff"]/neb_fuel_wm[neb_fuel_wm$super_simplified_primary_heating_fuel=="Solar","household_count"])))


poster_results <- list(
  "energy_poverty_prevalence"=energy_poverty_prevalence,
  "fpl_energy_poverty_prevalence"=fpl_energy_poverty_prevalence,
  "solar_energy_poverty_denominator"=solar_energy_poverty_denominator
)

saveRDS(poster_results, file="poster_results.RData")