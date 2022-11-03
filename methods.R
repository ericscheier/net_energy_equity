options(timeout=120)

get_tract_shapefiles <- function(
  states,
  acs_version,
  refresh,
  return_files=FALSE,
  all_from_list=NULL
){
  dir.create("data")
  tract_file_name <- paste0("data/",paste(states,collapse="_",sep=""),"_census_tracts.geojson")
  if(!file.exists(tract_file_name)){
    
    tract_states <- states
    if(tract_states=="all"){
      if(!is.null(all_from_list)){
        tract_states <- all_from_list
      } else{
        tract_states <- list_all_states()
      }
    }
    
    all_census_tracts <- lapply(tract_states, get_tracts, 
                                year=acs_version,
                                refresh=refresh,
                                remove_water=FALSE)
    census_tracts <- do.call(rbind, all_census_tracts)
    
    census_tracts$gisjoin <- paste0("G",paste(census_tracts$STATEFP,
                                              census_tracts$COUNTYFP,
                                              census_tracts$TRACTCE,
                                              sep="0"))
    # save multipolygon
    st_write(census_tracts, tract_file_name, delete_dsn = TRUE)
  }
  if(return_files){
    return(st_read(tract_file_name))
  }
}

paper_methods <- function(states="all",
                          acs_version=2018,
                          energy_burden_poverty_line=0.06,
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
        replica <- replica_to_lead()
        replica$merge_geo_id <- str_pad(as.character(replica$geoid), width=11, side="left", pad="0")
        
        merge_columns <- c("merge_geo_id",
                           "merge_income_bracket",
                           "replica_units",
                           "replica_housing_tenure")
        data <- left_join(replica, data, by=merge_columns)
        
        data <- drop_na(data, merge_columns)
        
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
        poverty_cutoff <- "very_low" # this is below 80% of AMI
        data$in_poverty <- as.factor(ifelse(data$income_bracket==poverty_cutoff,"Below AMI Poverty Line", "Above AMI Poverty Line"))
      } else {
        data$in_poverty <- data$income_bracket
      }
      
      ## Annual Energy Spending
      
      num_cols <- c("electricity_spend",
                    "gas_spend",
                    "other_spend",
                    "income")
      
      
      data <- data %>% 
        mutate(energy_cost = rowSums(dplyr::select(., electricity_spend,
                                                   gas_spend, other_spend
        ), na.rm = TRUE))
      data$energy_cost <- ifelse(abs(data$energy_cost) < 1,0,data$energy_cost)
      
      ## Net Income
      
      
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
      
      # Create the Energy Burden Indicator `mean_energy_burden`.
      
      data$energy_burden <- energy_burden_func(g=data$income,
                                               s=data$energy_cost)
      
      #For further analysis, I will add a designation of whether a cohort is, on average, in energy poverty depending on whether the mean energy burden is above `r label_percent()(energy_burden_poverty_line)`.
      
      data$energy_burden_poverty <- as.logical(data$energy_burden > energy_burden_poverty_line)
      
      ## Energy Return on Investment
      
      # `eroi = g/s`
      
      # Create the Energy Return on Investment Indicator `eroi`
      data$eroi <- eroi_func(g=data$income,
                             s=data$energy_cost)
      
      eroi_poverty_line <- eroi_func(g=1,
                                     s=energy_burden_poverty_line)
      ## Net Energy Ratio (or Net Energy Return)
      
      # `ner = (g-s)/s`
      
      # data$ner <- (data$annual_income - (12*data$mean_energy_cost)) / (12*data$mean_energy_cost)
      data$ner <- ner_func(g=data$income,
                           s=data$energy_cost)
      
      average_energy_cost <- weighted.mean(data$energy_cost, 
                                           data$total_kWh*data$households, 
                                           na.rm = T)/weighted.mean(data$total_kWh,
                                                                    data$households,
                                                                    na.rm = T)
      
      ner_poverty_line <- ner_func(g=1,
                                   s=energy_burden_poverty_line)
      
      ## Discretionary Energy Availability Rate
      
      # `dear = (g-s)/g`
      
      # This is equal to `1 - energy_burden`.
      
      # data$dear <- (data$annual_income - (12*data$mean_energy_cost)) / data$annual_income
      data$dear <- dear_func(g=data$income,
                             s=data$energy_cost)
      
      dear_poverty_line <- dear_func(g=1,
                                     s=energy_burden_poverty_line)

      
      data$households <- ifelse(is.na(data$households),0,data$households)
      
      data$simplified_primary_heating_fuel <- as.factor(ifelse(data$primary_heating_fuel=="ELECTRICITY",
                                                                         "Electricity", 
                                                                         ifelse(data$primary_heating_fuel %in% 
                                                                                  c("UTILITY GAS","BOTTLED GAS"),
                                                                                "Gas",
                                                                                ifelse(data$primary_heating_fuel=="SOLAR",
                                                                                       "Solar",
                                                                                       "Other"))))
      
      data$super_simplified_primary_heating_fuel <- 
        as.factor(ifelse(data$primary_heating_fuel=="SOLAR",
                         "Solar",
                         "Other"))
      
      
      data$all <- as.factor("all")
      
      data <- dplyr::left_join(data, replica_sup[c("geoid","company_ty","locale")], by=c("geoid"))
      
      data$simplified_utility_type <- ifelse(data$company_ty %in% c("Coop","DistCoop"),
                                                       "Cooperatively Owned",
                                                       ifelse(data$company_ty %in% c("Federal",
                                                                                               "Muni",
                                                                                               "State"),
                                                              "Government Owned",
                                                              ifelse(data$company_ty %in% 
                                                                       c("Private","PSubdiv"),
                                                                     "Privately Held",
                                                                     ifelse(data$company_ty=="IOU",
                                                                            "Publicly Held",
                                                                            "Other")
                                                              )
                                                       ))
      
      data$simplified_locale <- sub(" .*", "", data$locale)
      
      
      clean_data <- data[
        (data$energy_cost!=0)
        , ]
      
      write_csv(clean_data,paste0("data/very_clean_data_",base_file_name,".csv"))
    }
    
    income_metric <- "FPL"
    load_format <- "raw"
    save_format <- "in_poverty"
  }
  
  get_tract_shapefiles(
    states=states,
    acs_version=acs_version,
    refresh=refresh,
    return_files=FALSE
  )
  
  print("methods complete")
}