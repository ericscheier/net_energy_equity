library(tidyverse)
source("sources.R")


metric_name <- "neb"
metric_cutoff_level <- ner_func(g = 1,s = 0.06)^-1
metric_label <- bquote(E[b]^n)

# as.expression(bquote(~italic(nE[b])~": Energy Burden"))


clean_data_ami_all <- read_csv("CohortData_AreaMedianIncome.csv")
clean_data_fpl_all <- read_csv("CohortData_FederalPovertyLine.csv")
replica_sup <- read_csv("CensusTractData.csv") #get_replica_supplemental_dataset()

census_tracts_shp <- get_tract_shapefiles(
  states="all",
  acs_version=acs_version,
  refresh=refresh,
  return_files=TRUE,
  all_from_list=unique(replica_sup$state_abbr)
)


clean_data_ami_all$neb <- clean_data_ami_all$ner^-1
clean_data_fpl_all$neb <- clean_data_fpl_all$ner^-1

clean_data_ami_all_sup <- left_join(clean_data_ami_all, replica_sup, by=c("geoid"))
clean_data_fpl_all_sup <- left_join(clean_data_fpl_all, replica_sup, by=c("geo_id"="geoid"))

clean_data_ami_all_sup_shp <- st_sf(left_join(census_tracts_shp, clean_data_ami_all_sup, by=c("gisjoin")))
clean_data_fpl_all_sup_shp <- st_sf(left_join(census_tracts_shp, clean_data_fpl_all_sup, by=c("gisjoin")))

clean_data=clean_data_ami_all_sup
group_columns=c(NULL)
metric_name=metric_name
metric_cutoff_level = metric_cutoff_level
# metric_cutoff_label= NULL

#make_all_charts
#clean_data,
# group_columns,
# metric_name,
metric_label=metric_label
# metric_cutoff_level=0,
metric_cutoff_label=NULL
upper_quantile_view=0.99
lower_quantile_view=0.01
chart_title=NULL
chart_subtitle=NULL
chart_caption=NULL
x_label="Proportion of Households"
