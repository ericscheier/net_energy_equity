options(timeout=120)

paper_methods <- function(states="all",
                          acs_version=2018,
                          refresh=FALSE){

  income_metric <- "AMI" #"fpl15" #"FPL" #
  geographic_scope <- "Census Tracts" #statecitycounty
  load_format <- "raw" #poverty_line #lead #raw
  save_ext <- "csv"#"fst",#
  save_format <- "replica" #"replica"
  
  version_text <- as.character(acs_version)
  if(acs_version==2016){
    version_text <- "sh"
  }
  
  # add data folder if it does not exist
  if (!dir.exists("data")) {dir.create("data")}
  
  for(i in 1:2){
    print(i)
    
    base_file_name <- tolower(paste(income_metric,
                                    geographic_scope,
                                    version_text,
                                    paste(states,collapse="_",sep=""), sep = "_"))
    if(!file.exists(paste0("data/very_clean_data_",base_file_name,".csv")) | refresh){
      # paste0("data/clean_lead_",base_file_name,".",save_ext)
      
      # data <- read_csv(paste0("data/clean_",load_format,"_",base_file_name,".csv"), guess_max = Inf)
      data <- get_multiple_states(states=states,
                                  income_metric=income_metric,
                                  geographic_scope=geographic_scope,
                                  acs_version=acs_version,
                                  refresh=refresh,
                                  load_format=load_format,
                                  save_format=save_format,
                                  save_ext = save_ext,
                                  parallel=TRUE,
                                  load=TRUE)
      data$merge_geo_id <- str_pad(as.character(data$geo_id), width=11, side="left", pad="0")
      if(save_format=="replica"){
        
        # lead <- get_multiple_states(states=states,
        #                               income_metric=income_metric,
        #                               geographic_scope=geographic_scope,
        #                               acs_version=acs_version,
        #                               refresh=refresh,
        #                               load_format="lead",
        #                               save_format="replica",
        #                           save_ext = save_ext,
        #                           parallel=TRUE)
        
        replica <- replica_to_lead()
        replica$merge_geo_id <- str_pad(as.character(replica$geoid), width=11, side="left", pad="0")
        
        merge_columns <- c("merge_geo_id",
                           "merge_income_bracket",
                           "replica_units",
                           "replica_housing_tenure")
        
        # data <- full_join(lead, replica, by=merge_columns)
        data <- left_join(replica, data, by=merge_columns)
        
        data <- drop_na(data, merge_columns)# %>% nrow()
        
        data <- data %>% rename(income_bracket = merge_income_bracket,
                                number_of_units = replica_units,
                                housing_tenure = replica_housing_tenure,
                                households = lead_households,
                                income = lead_income,
                                electricity_spend = lead_electricity_spend,
                                gas_spend = lead_gas_spend,
                                other_spend = lead_other_spend)
      }
      
      # Calculating Additional Metrics
      
      ## Poverty
      
      if((tolower(income_metric) %in% tolower(c("fpl15","FPL"))) & save_format != "in_poverty"){
        poverty_cutoff <- "0-100%"
        data$in_poverty <- as.factor(ifelse(data$income_bracket==poverty_cutoff,"Below Federal Poverty Line", "Above Federal Poverty Line"))
      } else if (income_metric %in% c("ami68","AMI")){
        poverty_cutoff <- "very_low" # this is below 80% of AMI?
        data$in_poverty <- as.factor(ifelse(data$income_bracket==poverty_cutoff,"Below AMI Poverty Line", "Above AMI Poverty Line"))
      } else {
        data$in_poverty <- data$income_bracket
      }
      
      ## Annual Energy Spending
      
      # data$energy_cost <- (data$electricity_spend + 
      #                               data$gas_spend + 
      #                               data$other_spend)
      #12*data$mean_energy_cost
      
      num_cols <- c("electricity_spend",
                    "gas_spend",
                    "other_spend",
                    "income")
      
      # data[num_cols] <- lapply(data[num_cols], as.numeric)
      
      data <- data %>% 
        # mutate_at(vars(num_cols), funs(as.numeric)) %>% #ungroup %>% #str()
        mutate(energy_cost = rowSums(dplyr::select(., electricity_spend,
                                                   gas_spend, other_spend
        ), na.rm = TRUE)) #%>% head()
      data$energy_cost <- ifelse(abs(data$energy_cost) < 1,0,data$energy_cost)
      
      ## Net Income
      
      # data$net_income <- data$income - (data$energy_cost)
      
      data <- data %>% mutate(net_income = rowSums(dplyr::select(., income), na.rm = TRUE) - 
                                rowSums(dplyr::select(., energy_cost), na.rm = TRUE))
      
      ## Annual Energy Procurement (kWh)
      
      nat_gas_filename <- paste0(c("t",acs_version,"_24.csv"),collapse = "")
      nat_gas_filepath <- file.path("data",nat_gas_filename)
      if(!file.exists(nat_gas_filepath)){
        nat_gas_url <- paste0(c("https://www.eia.gov/naturalgas/annual/archive/",
                              acs_version,
                              "/csv/",
                              nat_gas_filename), collapse="")
        download.file(nat_gas_url,nat_gas_filepath, method="libcurl")
      }
      
      
      nat_gas <- read_csv(nat_gas_filepath) #change to 2018
      nat_gas <- drop_na(nat_gas) %>% rename("state_abbr"="State Code",
                                             "dlrs_Mcf"="Res. Avg PR")
      
      data$state_fips <- substr(data$merge_geo_id,1,2)
      replica_sup <- get_replica_supplemental_dataset()
      
      nat_gas <- left_join(nat_gas, unique(replica_sup[c("state_abbr","state_fips")]), by=c("state_abbr"))
      data <- left_join(data, nat_gas[,c("state_abbr","state_fips", "dlrs_Mcf")], by=c("state_fips"))
      # convert natural gas consumption to kWh 
      data$gas_Mcf <- data$gas_spend / data$dlrs_Mcf
      data$gas_therms <- data$gas_Mcf / 10.37
      data$gas_kWh <- data$gas_therms * 29.3001
      # (use fixed for now, can make dynamic based on market heating values eventually)
      # calculate electricity implied usage
      data <- left_join(data, replica_sup[c("geoid","dlrs_kwh")],by=c("merge_geo_id"="geoid"))
      data$electricity_kWh <- data$electricity_spend / data$dlrs_kwh
      # add natgas energy value to electricity implied usage
      data$total_kWh <- data$gas_kWh + data$electricity_kWh
      # add other fuel energy values as future research
      
      # ggplot(data=data, aes(x=income, y=energy_cost, color=in_poverty)) + 
      #   geom_point(aes(size=households), alpha=0.05, show.legend = FALSE) + 
      #   geom_smooth(method=lm,
      #                 se=TRUE,
      #                 fullrange=TRUE)# + 
      #   #xlim(0,500000)
      #   
      
      # Create the Energy Burden Indicator `mean_energy_burden`.
      
      # data$energy_burden <- data$mean_energy_cost / (data$annual_income/12.0)
      data$energy_burden <- energy_burden_func(g=data$income,
                                               s=data$energy_cost)
      
      energy_burden_poverty_line <- 0.10
      
      #For further analysis, I will add a designation of whether a cohort is, on average, in energy poverty depending on whether the mean energy burden is above `r label_percent()(energy_burden_poverty_line)`.
      
      data$energy_burden_poverty <- as.logical(data$energy_burden > energy_burden_poverty_line)
      
      ## Energy Return on Investment
      
      # `eroi = g/s`
      
      # Create the Energy Return on Investment Indicator `eroi`
      # data$eroi <- data$annual_income / (12*data$mean_energy_cost)
      data$eroi <- eroi_func(g=data$income,
                             s=data$energy_cost)
      
      eroi_poverty_line <- eroi_func(g=1,
                                     s=energy_burden_poverty_line)
      ## Net Energy Ratio (or Net Energy Return)
      
      # `ner = (g-s)/s`
      
      # data$ner <- (data$annual_income - (12*data$mean_energy_cost)) / (12*data$mean_energy_cost)
      data$ner <- ner_func(g=data$income,
                           s=data$energy_cost)#,
      # se=data$total_kWh)
      
      average_energy_cost <- weighted.mean(data$energy_cost, 
                                           data$total_kWh*data$households, 
                                           na.rm = T)/weighted.mean(data$total_kWh,
                                                                    data$households,
                                                                    na.rm = T)
      
      ner_poverty_line <- ner_func(g=1,
                                   s=energy_burden_poverty_line)#,
      # se=energy_burden_poverty_line/(average_energy_cost))
      
      ## Discretionary Energy Availability Rate
      
      # `dear = (g-s)/g`
      
      # This is equal to `1 - energy_burden`.
      
      # data$dear <- (data$annual_income - (12*data$mean_energy_cost)) / data$annual_income
      data$dear <- dear_func(g=data$income,
                             s=data$energy_cost)
      
      dear_poverty_line <- dear_func(g=1,
                                     s=energy_burden_poverty_line)
      # 
      # energy_burden_poverty_line <- 0.10
      # 
      # eroi_poverty_line <- eroi_func(g=1,
      #                                s=energy_burden_poverty_line)
      # 
      # average_energy_cost <- weighted.mean(data$energy_cost, 
      #                                      data$total_kWh*data$households, 
      #                                      na.rm = T)/weighted.mean(data$total_kWh,
      #                                                               data$households,
      #                                                               na.rm = T)
      # 
      # median_energy_cost <- weighted.median(data$energy_cost, 
      #                                      data$total_kWh*data$households, 
      #                                      na.rm = T)/weighted.median(data$total_kWh,
      #                                                               data$households,
      #                                                               na.rm = T)
      # # 12*(data$electricity_spend + 
      # #       data$gas_spend + 
      # #       data$other_spend)
      # # data$total_kWh <- data$gas_kWh + data$electricity_kWh
      # median_electricity_cost <- weighted.median(data$electricity_spend,
      #                               data$electricity_kWh*data$households, 
      #                                      na.rm = T)/weighted.median(data$electricity_kWh,
      #                                                               data$households,
      #                                                               na.rm = T)
      # 
      # median_gas_cost <- weighted.median(data$gas_spend, 
      #                                      data$gas_kWh*data$households, 
      #                                      na.rm = 
      #                                     T)/weighted.median(data$gas_kWh,
      #                                                               data$households,
      #                                                               na.rm = T)
      # median_gas_cost_Mcf <- weighted.median(data$gas_spend, 
      #                                      data$gas_Mcf*data$households, 
      #                                      na.rm = T)/weighted.median(data$gas_Mcf,
      #                                                               data$households,
      #                                                               na.rm = T)
      # 
      # 
      # ner_poverty_line_dlrs <- ner_func(g=1,
      #                                   s=energy_burden_poverty_line)
      # 
      # ner_poverty_line_mean <- ner_func(g=1,
      #                              s=energy_burden_poverty_line,
      #                              se=energy_burden_poverty_line/(average_energy_cost))
      # 
      # ner_poverty_line_median <- ner_func(g=1,
      #                              s=energy_burden_poverty_line,
      #                              se=median_energy_cost/energy_burden_poverty_line)
      # 
      # ner_poverty_line <- ner_poverty_line_dlrs #ner_poverty_line_median
      # 
      # 
      # dear_poverty_line <- dear_func(g=1,
      #                                s=energy_burden_poverty_line)
      # 
      # ner_dear_poverty_line <- dear_func(g=1+median_energy_cost*ner_poverty_line_median,
      #                                s=1)
      ## Filter Outliers
      
      # metric_name <- "ner"
      # metric_cutoff_level <- ner_poverty_line
      # group_variable <- NULL# "GEOID" #"state_abbr" #merge_geo_id" #
      # group_columns <- c(group_variable) #c("gisjoin") #
      # graph_data <- filter_graph_data(data, group_columns, metric_name)
      # 
      # top_metrics <- grouped_weighted_metrics(graph_data, 
      #                          group_columns, 
      #                          metric_name, 
      #                          metric_cutoff_level, 
      #                          upper_quantile_view=1,#0.9995, 
      #                          lower_quantile_view=0)#0.00005)
      # t(top_metrics)
      
      # data$year_constructed <- fct_reorder(data$year_constructed, data$min_age)
      # data$number_of_units <- fct_reorder(data$number_of_units, data$min_units)
      # data$income_bracket <- factor(data$income_bracket, levels=sort(levels(data$income_bracket)))
      
      # data$state_name <- factor(data$state_name, levels=sort(unique(data$state_name)))
      # data$state_abbr <- factor(data$state_abbr, levels=sort(unique(data$state_abbr)))
      # data$county_name <- factor(data$county_name, levels=sort(unique(data$county_name)))
      # # data$area_name <- factor(data$area_name, levels=sort(unique(data$area_name)))
      # # data$state_code_usps <- factor(data$state_code_usps, levels=sort(unique(data$state_code_usps)))
      # 
      # data$lihtc_qualified <- as_factor(data$lihtc_qualified)
      
      data$households <- ifelse(is.na(data$households),0,data$households)
      
      
      # for (col_to_order in cols_to_order){
      #   data[[col_to_order]] <- factor(data[[col_to_order]], 
      #                                  levels=levels(data[[col_to_order]])[
      #                                    sort(as.numeric(str_extract(levels(data[[col_to_order]]), "[0-9]+")), 
      #                                         index.return=TRUE, 
      #                                         decreasing = TRUE)[["ix"]]
      #                                    ])
      #   print(levels(data[[col_to_order]]))
      # }
      
      # need to update this for being at this stage of the production
      # not sure what is going on with this particular outlier tract, seems to be a park with 500 residents
      # removed manually for now 
      clean_data <- data[#!(data$geo_id %in% c("36005027600")) & 
        #(data$ner<=top_metrics$metric_upper) & 
        #(data$ner>=top_metrics$metric_lower) &
        (data$energy_cost!=0)# & 
        # is.finite(data$ner)
        , ] #drop_na(data)
      
      # clean_data$
      # clean_graph_data <- filter_graph_data(clean_data, group_columns, metric_name)
      
      # clean_top_metrics <- grouped_weighted_metrics(clean_graph_data, 
      #                          group_columns, 
      #                          metric_name, 
      #                          metric_cutoff_level, 
      #                          upper_quantile_view=1, 
      #                          lower_quantile_view=0)
      # t(clean_top_metrics)
      
      # top_metrics$household_count - clean_top_metrics$household_count
      
      # the methods to acquire the geojson files is currently located in tigris_munging.Rmd
      # load the tract or state (or zip) geojson from csv to sf as: data
      # census_tracts_shp <- st_read("census_tracts.geojson")
      # data <- read_csv(paste0("very_clean_data.csv"), guess_max = Inf)
      
      # tract_shp <- st_sf(left_join(census_tracts_shp, clean_data, by=c("gisjoin")))
      # 
      # # tract_shp <- st_sf(left_join(tract_shp, replica_sup, by=c("gisjoin")))
      
      write_csv(clean_data,paste0("data/very_clean_data_",base_file_name,".csv"))
    }
    
    # st_write(obj=tract_shp,
    #          dsn=paste0("very_clean_data_",
    #                     paste0(states,collapse = "_"),
    #                     ".geojson"),
    #          delete_dsn = TRUE)
    
    # geojsonio::geojson_write(tract_shp, 
    #                          file = paste0("very_clean_data_",
    #                                        paste0(states,
    #                                               collapse = "_"),
    #                                        ".geojson"),
    #                          geometry="polygon")
    income_metric <- "FPL"
    load_format <- "raw"
    save_format <- "in_poverty"
  }
  
  tract_file_name <- paste0("data/",paste(states,collapse="_",sep=""),"_census_tracts.geojson")
  if(!file.exists(tract_file_name)){
    
    tract_states <- states
    if(tract_states=="all"){
      tract_states <- list_all_states()
    }
    
    all_census_tracts <- lapply(tract_states[1:2], get_tracts, 
                                year=acs_version, refresh=refresh)
    # census_tracts <- do.call(rbind, all_census_tracts)
    census_tracts <- rbind_tigris(all_census_tracts)
    
    # unique_state_counties <- unique(st_drop_geometry(census_tracts)[c("STATEFP","COUNTYFP")])
    
    # usc_list <- as.list(as.data.frame(t(unique_state_counties)))
    
    
    # county_waters <- lapply(usc_list, 1, get_county_water,
                           # year=acs_version,
                           # refresh=refresh)
    
    # water_area <- do.call(rbind, county_waters)
    # water_area <- st_union(unlist(county_waters))
    # water_area <- do.call(st_union, county_waters)
    
    # all_water_area <- lapply(tract_states, get_state_water,
    #                          year=acs_version, refresh=refresh)
    # 
    # water_area <- do.call(rbind, all_water_area)
    
    # census_tracts <- census_tracts %>% st_difference(st_union(water_area))
    
    census_tracts$gisjoin <- paste0("G",paste(census_tracts$STATEFP,
                                              census_tracts$COUNTYFP,
                                              census_tracts$TRACTCE,
                                              sep="0"))
    # save multipolygon
    st_write(census_tracts, tract_file_name, delete_dsn = TRUE)
  }
  print("methods complete")
}