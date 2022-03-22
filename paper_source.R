income_metric <- "AMI" #"AMI" #"fpl15" #
geographic_scope <- "Census Tracts" #statecitycounty

version_text <- as.character(acs_version)
if(acs_version==2016){
  version_text <- "sh"
}

base_file_name <- tolower(paste(income_metric,
                                geographic_scope,
                                version_text,
                                paste(states,collapse="_",sep=""), sep = "_"))

clean_data_ami <- read_csv(paste0("data/very_clean_data_",base_file_name,".csv"), guess_max = 10^6)

income_metric <- "FPL"
base_file_name <- tolower(paste(income_metric,
                                geographic_scope,
                                version_text,
                                paste(states,collapse="_",sep=""), sep = "_"))
clean_data_fpl <- read_csv(paste0("data/very_clean_data_",base_file_name,".csv"), guess_max = 10^6)

tract_file_name <- paste0("data/",paste(states,collapse="_",sep=""),"_census_tracts.geojson")

census_tracts_shp <- st_read(tract_file_name)
replica_sup <- get_replica_supplemental_dataset()
tract_shp <- st_sf(left_join(census_tracts_shp, replica_sup, by=c("gisjoin")))

# poverty-lines

eroi_poverty_line <- eroi_func(g=1,
                               s=energy_burden_poverty_line)

average_energy_cost <- weighted.mean(clean_data_ami$energy_cost, 
                                     clean_data_ami$total_kWh*clean_data_ami$households, 
                                     na.rm = T)/weighted.mean(clean_data_ami$total_kWh,
                                                              clean_data_ami$households,
                                                              na.rm = T)

median_energy_cost <- weighted.median(clean_data_ami$energy_cost, 
                                      clean_data_ami$total_kWh*clean_data_ami$households, 
                                      na.rm = T)/weighted.median(clean_data_ami$total_kWh,
                                                                 clean_data_ami$households,
                                                                 na.rm = T)
# 12*(clean_data_ami$electricity_spend + 
#       clean_data_ami$gas_spend + 
#       clean_data_ami$other_spend)
# clean_data_ami$total_kWh <- clean_data_ami$gas_kWh + clean_data_ami$electricity_kWh
median_electricity_cost <- weighted.median(clean_data_ami$electricity_spend,
                                           clean_data_ami$electricity_kWh*clean_data_ami$households, 
                                           na.rm = T)/weighted.median(clean_data_ami$electricity_kWh,
                                                                      clean_data_ami$households,
                                                                      na.rm = T)

median_gas_cost <- weighted.median(clean_data_ami$gas_spend, 
                                   clean_data_ami$gas_kWh*clean_data_ami$households, 
                                   na.rm = 
                                     T)/weighted.median(clean_data_ami$gas_kWh,
                                                        clean_data_ami$households,
                                                        na.rm = T)
median_gas_cost_Mcf <- weighted.median(clean_data_ami$gas_spend, 
                                       clean_data_ami$gas_Mcf*clean_data_ami$households, 
                                       na.rm = T)/weighted.median(clean_data_ami$gas_Mcf,
                                                                  clean_data_ami$households,
                                                                  na.rm = T)


ner_poverty_line_dlrs <- ner_func(g=1,
                                  s=energy_burden_poverty_line)

ner_poverty_line_mean <- ner_func(g=1,
                                  s=energy_burden_poverty_line,
                                  se=energy_burden_poverty_line/(average_energy_cost))

ner_poverty_line_median <- ner_func(g=1,
                                    s=energy_burden_poverty_line,
                                    se=median_energy_cost/energy_burden_poverty_line)

ner_poverty_line <- ner_poverty_line_dlrs #ner_poverty_line_median


dear_poverty_line <- dear_func(g=1,
                               s=energy_burden_poverty_line)

ner_dear_poverty_line <- dear_func(g=1+median_energy_cost*ner_poverty_line_median,
                                   s=1)

ner_nice_fraction <- paste0(floor(ner_poverty_line),ifelse(floor(ner_poverty_line)==ner_poverty_line,"",paste0("\\ \\frac{",numerators(fractional(ner_poverty_line-floor(ner_poverty_line))),"}{",denominators(fractional(ner_poverty_line-floor(ner_poverty_line))),"}")))

ner_nice_text <- paste0(ifelse(ner_poverty_line==round(ner_poverty_line,0),"","\\approx"),round(ner_poverty_line))

# figure-parameters
#chart_title <- "Household Economic Return on Energy Spending"
chart_title <- "Community Net Energy Return"
chart_subtitle <- "Net Earnings per Dollar of Energy Consumed"

group_columns <- NULL#"income_bracket")#in_poverty
#"primary_heating_fuel"

metric_name <- "ner" #"energy_burden" #"ner" #"dear" #"eroi"
metric_label <- "$/$"
metric_cutoff_level <- ner_poverty_line
metric_cutoff_label <- "Energy Poverty Line"

upper_quantile_view <- 1.0
lower_quantile_view <- 0.0

# top_metrics
group_variable <- NULL# "GEOID" #"state_abbr" #merge_geo_id" #
group_columns <- c(group_variable) #c("gisjoin") #
graph_data <- filter_graph_data(clean_data_ami, group_columns, metric_name)

top_metrics <- grouped_weighted_metrics(graph_data, 
                                        group_columns, 
                                        metric_name, 
                                        metric_cutoff_level, 
                                        upper_quantile_view=0.99, 
                                        lower_quantile_view=0.00)
# head(top_metrics)


# grouped_weighted_metrics}

#data$GEOID <- sub('.', '', data$gisjoin)
group_variable <- "geoid"# "GEOID" #"state_abbr" #merge_geo_id" #
group_columns <- c(group_variable) #c("gisjoin") #
graph_data <- filter_graph_data(clean_data_ami, group_columns, metric_name)

gwm <- grouped_weighted_metrics(graph_data, 
                                group_columns, 
                                metric_name, 
                                metric_cutoff_level, 
                                upper_quantile_view=0.75, 
                                lower_quantile_view=0.25)
# head(gwm)


fpl_graph_data <- filter_graph_data(clean_data_fpl, group_columns="in_poverty", metric_name)

fpl_top_metrics <- grouped_weighted_metrics(fpl_graph_data, 
                                            group_columns="in_poverty", 
                                            metric_name, 
                                            metric_cutoff_level, 
                                            upper_quantile_view=1.00, 
                                            lower_quantile_view=0.00)

fpl_ep_data <- filter_graph_data(clean_data_fpl,
                                 group_columns=c("in_poverty",
                                                 "energy_burden_poverty"), 
                                 metric_name)

fpl_ep_top_metrics <- grouped_weighted_metrics(fpl_ep_data, 
                                               group_columns=c("in_poverty",
                                                               "energy_burden_poverty"), 
                                               metric_name, 
                                               metric_cutoff_level, 
                                               upper_quantile_view=1.00, 
                                               lower_quantile_view=0.00)


# should replace this with weighted metrics data
# How many households are below the poverty line in this data?
total_households <- fpl_top_metrics$household_count

number_in_poverty <- fpl_top_metrics$households_below_cutoff

pct_in_poverty <- number_in_poverty / total_households

pct_of_ep_below_fpl <- fpl_top_metrics$households_below_cutoff[fpl_top_metrics$in_poverty=="Below Federal Poverty Line"] / sum(number_in_poverty)

# number_above_poverty <- sum((data$income_bracket!=poverty_cutoff) * (data$households))

pct_above_poverty <- 1 - pct_in_poverty

in_both_poverty <- pct_in_poverty[2]

in_only_energy_poverty <- pct_in_poverty[1]

energy_poverty_rate <- sum(number_in_poverty) / sum(total_households)

ami_graph_data <- filter_graph_data(clean_data_ami,
                                    group_columns=c("in_poverty",
                                                    "income_bracket"),
                                    metric_name)
ami_top_metrics <- grouped_weighted_metrics(ami_graph_data, 
                                            group_columns=c("in_poverty",
                                                            "income_bracket"), 
                                            metric_name, 
                                            metric_cutoff_level,
                                            upper_quantile_view=1.00, 
                                            lower_quantile_view=0.00)


compare_metrics <- function(metric_name="ner"){
  gwm <- calculate_weighted_metrics(filter_graph_data(clean_data_ami,c("income_bracket"),metric_name),
                                    c("income_bracket"),
                                    metric_name,
                                    metric_cutoff_level=0,
                                    upper_quantile_view = 1,
                                    lower_quantile_view=0)
  gwm$metric_name <- metric_name
  return(gwm)
}

compare_table <- rbindlist(lapply(c("income",
                                    "energy_cost",
                                    # "energy_burden",
                                    "net_income"#,
                                    # "ner"
), compare_metrics))

compare_table_totals <- compare_table %>% 
  filter(metric_name=="income") %>% 
  pivot_wider(id_cols=income_bracket,
              names_from=metric_name,
              values_from=household_count,
  ) %>% 
  rename(households=income) %>% pivot_longer(cols="households")

compare_table <- compare_table %>% 
  pivot_wider(id_cols=income_bracket,
              names_from=metric_name,
              values_from=metric_mean) %>% 
  mutate(energy_burden = energy_burden_func(g=income, s=energy_cost),
         ner = ner_func(g=income, s=energy_cost)) %>% 
  pivot_longer(cols=c("income","energy_cost","energy_burden","net_income","ner"))

compare_table <- rbind(compare_table_totals, compare_table)

#format the metrics accordingly
# income = 0 decimal place $
# energy_cost = 0 decimal place $
# energy_burden = 0 decimal place %
# net_income = 0 decimal place $
# ner = 1 decimal place number

which_metric <- "value" #"metric_mean" #"metric_median"

compare_table$print_value <- ifelse(compare_table$name %in% c("income",
                                                              "energy_cost",
                                                              "net_income"),
                                    to_dollar(compare_table[[which_metric]],latex=TRUE),
                                    ifelse(compare_table$name %in% c("energy_burden"),
                                           to_percent(compare_table[[which_metric]],latex=TRUE),
                                           ifelse(compare_table$name %in% c("ner"),
                                                  round(compare_table[[which_metric]],1),
                                                  ifelse(compare_table$name %in% c("households"),
                                                         to_million(compare_table[[which_metric]]),
                                                         to_big(compare_table[[which_metric]])))))

compare_table$display_income_bracket <- dplyr::recode_factor(compare_table$income_bracket, 
                                                             very_low="0-30\\% AMI",
                                                             low_mod="30-80\\% AMI", 
                                                             mid_high="Above 80\\% AMI",
                                                             All="All",
                                                             .ordered=TRUE)

energy_burden_label <- "\\textit{E\\textsubscript{b}} (\\textit{S}/\\textit{G})" #if html etc. <- "*E~b~* (S/G)"
ner_label <- "\\textit{N\\textsubscript{h}} ([\\textit{G}-\\textit{S}]/\\textit{S})" #if html etc. <- "*N~h~* ([G-S]/S)"

compare_table[,"Metric Name"] <- dplyr::recode(compare_table$name,
                                               households="Households in Sample",
                                               income="Annual Income (\\textit{G})",
                                               energy_cost="Annual Energy Expenditures (\\textit{S})",
                                               energy_burden=energy_burden_label,
                                               net_income="Net Income (\\textit{G}-\\textit{S})",
                                               ner=ner_label)

print_table <- compare_table[,c("Metric Name","display_income_bracket","print_value")] %>% 
  arrange(display_income_bracket) %>% 
  pivot_wider(names_from=display_income_bracket,values_from=print_value)

write_csv(print_table, "comparison_table.csv")

highlight_metrics <- rbindlist(lapply(c(
  "energy_burden",
  "ner"
), compare_metrics))

highlight_energy_burden <- as.numeric(highlight_metrics[metric_name=="energy_burden" & income_bracket=="very_low","metric_mean"])

highlight_ner <- as.numeric(highlight_metrics[metric_name=="ner" & income_bracket=="very_low","metric_mean"])

num_one <- paste0(to_big(highlight_energy_burden*100),"\\%")
num_two <- round(highlight_ner,1)