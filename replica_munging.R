get_replica_dataset <- function(){
  file_name <- "seeds_ii_replica"
  
  desired_name <- paste0(file_name,".csv")
  desired_path <- file.path("data",desired_name)
  
  if(!file.exists(desired_path)){
    file_url <- "https://data.nrel.gov/files/81/seeds_ii_replica%20%281%29.csv"
    
    download.file(file_url,desired_path, method="libcurl")
  }
  
  dataset <- fread(desired_path, colClasses = "character")
  #read_csv(desired_path)#, col_types = "character")
  return(dataset)
}

get_replica_supplemental_dataset <- function(col_types=NULL){
  # company_na:                 Utility Company Name
  # company_ty:                 Utility Company Type
  # eia_id:                     EIA ID
  # cust_cnt:                   Utility Customer Count
  # avg_monthl:                 Average Monthly Consumption (kWh)
  # avg_mon_01:                 Average Monthly Bill ($)
  # dlrs_kwh:                   Average Cost of Electricity ($/kWh)
  # avg_pbi_us:                 Average State Residential Solar Production-based Incentive ($/kWh)
  # avg_cbi_us:                 Average State Residential Solar Capacity-based Incentive ($/W)
  # avg_ibi_pc:                 Average State Residential Solar Investment-based Incentive (%)
  # hh_size_1:                  Number of 1 person households
  # hh_size_2:                  Number of 2 person households
  # hh_size_3:                  Number of 3 person households
  # hh_size_4:                  Number of 4 person households
  # fam_med_in:                 Median family income
  # hh_med_inc:                 Median household income
  # hh_gini_in:                 Household GINI Index of Income Inequality
  # pop_total:                  Total population
  # pop_male:                   Total male population
  # pop_female:                 Total female population
  # pop_us_cit:                 Total US citizens
  # pop_nat_us:                 Total naturalized US citizens
  # pop_non_us:                 Total non-US citizens
  # pop_hispan:                 Total hispanics
  # pop_africa:                 Total african american population
  # pop_asian:                  Total asian population
  # pop_native:                 Total american indian/alaska native population
  # pop_caucas:                 Total caucasian population
  # pop25_some:                 Total population with at least some college education (Population 25 years and over)
  # pop25_high:                 Total population with a high school diploma (Population 25 years and over)
  # pop25_no_h:                 Total population with less than a high school diploma (Population 25 years and over)
  # pop_med_ag:                 Median age
  # p16_employ:                 Total employed (Population 16 years and over)
  # p16_unempl:                 Total unemployed (Population 16 years and over)
  # fam_childr:                 Total number of families with children under 6 years
  # fam_chi_01:                 Total number of families with children ages 6-17 years
  # pop_over_6:                 Total population over 65 years
  # pop_under_:                 Total population under 18 years
  # hu_monthly:                 Total number of owner-occupied units with housing costs less than $1000/month
  # hu_mont_01:                 Total number of owner-occupied units with housing costs greater than $1000/month
  # hu_own:                     Total number of owner occupied housing units
  # hu_rent:                    Total number of renter occupied housing units
  # hu_vintage:                 Number of occupied units built after 2010
  # hu_vint_01:                 Number of occupied units built between 2000-2009
  # hu_vint_02:                 Number of occupied units built between 1980-1999
  # hu_vint_03:                 Number of occupied units built between 1960-1979
  # hu_vint_04:                 Number of occupied units built between 1940-1959
  # hu_vint_05:                 Number of occupied units built before 1939
  # hu_med_val:                 Median value of owner-occupied housing units
  # hu_mortgag:                 Number of owner-occupied housing units with a mortgage
  # hu_no_mort:                 Number of owner-occupied housing units without a mortgage
  # aqi_max:                    Max Air Quality Index
  # aqi_max_de:                 Max Air Quality Index Description
  # aqi_90th_p:                 90th Percentile Air Quality Index
  # aqi_90t_01:                 90th Percentile Air Quality Index Description
  # aqi_median:                 Median Air Quality Index
  # aqi_med_01:                 Median Air Quality Index Description
  # hdd:                        Heating Degree Days
  # hdd_std:                    Heating Degree Days Standard Deviation
  # hdd_ci:                     Heating Degree Days Confidence Interval
  # cdd:                        Cooling Degree Days
  # cdd_std:                    Cooling Degree Days Standard Deviation
  # cdd_ci:                     Cooling Degree Days Confidence Interval
  # climate_zo:                 Climate Zone
  # climate_01:                 Climate Zone Description
  # moisture_r:                 Moisture Regime
  # locale:                     Locale
  # total_unit:                 Total Number of Active Public Housing Units
  # active_sub:                 Total Number of Active Subsidies
  # avg_months:                 Average Months of Tenancy
  # fmr_2br   :                 Fair Market Rent - 2 BR
  # occ_rate  :                 Occupancy Rate
  # pct_eli_hh:                 Percent Extremely Low Income
  # lihtc_qual:                 Low Income Tax Credit Qualification (T/F)
  replica <- get_clean_replica(replica=get_replica_dataset(),
                               supplemental_only=TRUE,
                               col_types=col_types)
  
  replica$geoid <- str_pad(as.character(replica$geoid), 
                           width=11, 
                           side="left", 
                           pad="0")
  
  
  return(replica)
}

get_clean_replica <- function(replica=get_replica_dataset(),
                              supplemental_only=FALSE,
                              with_supplemental=TRUE,
                              col_types=NULL){
  # "Fields available include
  # estimates of number of households, number of suitable buildings, number of developable planes, area
  # of developable planes (m2), total capacity potential (MW), and total annual generation potential (MWh)
  # for each of the 20 demographic combinations of AMI Income Group (0-30% AMI, 30-50% AMI, 50-80%
  # AMI, 80-120% AMI, >120% AMI), Housing Type (multi-family or single-family), and Tenure (renter or
  # owner). The result is an array of 120 fields related to LMI solar potential for each Census Tract."
  
  replica_income_bracket <- c("very_low", 
                              "low", 
                              "mod", 
                              "mid", 
                              "high")
  replica_units <- c("sf","mf")
  replica_housing_tenure <- c("rent","own")
  variables <- c("hh", # household count
                 "elep_hh", # Average Household Electricity Expenditures ($/month)
                 "mwh", # annual generation
                 "bldg_cnt", # suitable building count
                 "devp_cnt", # developable plane count
                 "devp_m2", # suitable area
                 "mw") # capacity
  
  spec <-  expand_grid(replica_income_bracket = replica_income_bracket, 
                       replica_units = replica_units, 
                       replica_housing_tenure = replica_housing_tenure, 
                       .value = variables)
  spec$.name <- do.call(paste, c(spec, sep="_"))
  
  spec <- spec[(spec$.name %in% names(replica)),]
  
  supplement_names <- names(replica)[!(names(replica) %in% spec$.name)]
  
  if (supplemental_only){
    replica <- replica[,..supplement_names]
    with_supplemental <- TRUE
  } else {
    replica <- pivot_longer_spec(replica, spec=spec)
  }
  
  if (!with_supplemental){
    non_supplement_names <- names(replica)[!(supplement_names %in% names(replica))]
    replica <- replica[,..non_supplement_names]
  }
  
  replica <- type_convert(replica %>% mutate_all(as.character), col_types=col_types)
  
  return(replica)
}

replica_to_lead <- function(replica=get_clean_replica()){
  
  # "very_low" =  0-30% AMI --> very_low
  # "low" = 30-50% AMI --> low_mod
  # "mod" = 50-80% AMI --> low_mod
  # "mid" = 80-120% AMI --> mid_high
  # "high" = >120% AMI --> mid_high
  
  replica$merge_income_bracket <- recode_factor(replica$replica_income_bracket,
                                                `very_low` = "very_low", 
                                                `low` = "low_mod",
                                                `mod` = "low_mod", 
                                                `mid` = "mid_high", 
                                                `high` = "mid_high")
  
  replica$replica_housing_tenure <- recode_factor(replica$replica_housing_tenure,
                                                  `rent` = "rented",
                                                  `own` = "owned")
  
  replica$replica_units <- recode_factor(replica$replica_units,
                                         `sf` = "single-family",
                                         `mf` = "multi-family")
  
  group_columns <- c("geoid", 
                     "merge_income_bracket", 
                     "replica_units", 
                     "replica_housing_tenure" ) #""
  
  
  replica <- replica %>%
    group_by_at(., .vars=vars(all_of(group_columns))) %>% 
    mutate(group_households = sum(hh)) %>% 
    summarise(replica_households = sum(hh),
              replica_electricity_spend = sum(elep_hh * group_households)/sum(group_households),
              replica_mwh = sum(mwh),
              replica_suitable_buildings = sum(bldg_cnt),
              replica_suitable_planes = sum(devp_cnt),
              replica_suitable_area_m2 = sum(devp_m2),
              replica_mw = sum(mw)) %>% ungroup()
  
  return(replica)
}

lead_to_replica <- function(clean_lead=NULL){
  # #convert and aggregate by then merge_on / group_by
  
  if (is.null(clean_lead)){
    clean_lead <- NULL
  }
  # print(object.size(clean_lead, units="Gb"))
  print("consolidating housing_tenure")
  clean_lead$replica_housing_tenure <- dplyr::recode_factor(clean_lead$housing_tenure, 
                                                     `OWNER` = "owned", 
                                                     `RENTER` = "rented")#,
  # .default = "D", 
  # .missing = "M")
  print("consolidating income_bracket")
  clean_lead$merge_income_bracket <- dplyr::recode_factor(clean_lead$income_bracket,
                                                   `0-30%` = "very_low",
                                                   `30-60%` = "low_mod",
                                                   `60-80%` = "low_mod", 
                                                   `80-100%` = "mid_high", 
                                                   `100%+` = "mid_high")
  print("consolidating min_units")
  clean_lead$replica_units <- as.factor(ifelse(clean_lead$min_units > 1, "multi-family", "single-family"))
  # seem to be getting a lot of NAs in clean_lead$min_units, likely from "other". Like...13% of homes in NC are NA. Should reconcile...
  
  
  # #merge_on / group_by
  # geo_id = readr::col_character(), 
  print("consolidating by all columns")
  group_columns <- c("geo_id", 
                     "merge_income_bracket", 
                     "replica_units", 
                     "replica_housing_tenure" ) #""
  # print(object.size(clean_lead, units="Gb"))
  
  clean_lead <- clean_lead %>%
    group_by_at(., .vars=vars(all_of(group_columns))) %>% 
    mutate(group_households = sum(as.numeric(households), na.rm = T)) %>% 
    summarise(lead_households = sum(as.numeric(households), na.rm = T),
              lead_income = sum(as.numeric(income) * group_households, na.rm = T)/sum(group_households, na.rm = T),
              # lead_mean_energy_cost = sum(mean_energy_cost * group_households)/sum(group_households),
              lead_electricity_spend = sum(as.numeric(electricity_spend) * group_households, na.rm = T)/sum(group_households, na.rm = T),
              lead_gas_spend = sum(as.numeric(gas_spend) * group_households, na.rm = T)/sum(group_households, na.rm = T),
              lead_other_spend = sum(as.numeric(other_spend) * group_households, na.rm = T)/sum(group_households, na.rm = T), 
              # lead_average_min_age = sum(min_age * group_households)/sum(group_households),
              lead_pct_detached = sum(as.numeric(detached) * group_households, na.rm = T)/sum(group_households, na.rm = T)) %>% 
    ungroup()
              # primary fuel type
  # print(object.size(clean_lead, units="Gb"))
  # aggregate
  
  # ## household weighted average
  # lead_min_age,
  # lead_min_units,
  # lead_detached,
  
  # ## sum
  # acs_responses = col_double(),
  # 
  return(clean_lead)
}