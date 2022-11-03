compare_metrics <- function(
    metric_name,
    groups,
    input_dataset=NULL,
    metric_cutoff_level=0,
    ...
){
  gwm <- calculate_weighted_metrics(filter_graph_data(input_dataset,groups,metric_name),
                                    groups,
                                    metric_name,
                                    metric_cutoff_level=metric_cutoff_level,
                                    upper_quantile_view = 1,
                                    lower_quantile_view=0)
  gwm$metric_name <- metric_name
  return(gwm)
}

create_comparison_table <- function(
    metrics_to_compare=c("income",
                         "energy_cost",
                         # "energy_burden",
                         "net_income"#,
                         # "ner"
    ),
    groups=c("income_bracket"),
    input_dataset=NULL,
    include_totals=TRUE,
    metric_name="ner"
){
  minimum_metrics_to_compare <- c("income","energy_cost","net_income")
  
  metrics_to_compare <- unique(c(metrics_to_compare,minimum_metrics_to_compare))
  
  compare_table_wide <- data.table::rbindlist(lapply(X=metrics_to_compare, 
                                                     FUN=compare_metrics,
                                                     groups=groups,
                                                     input_dataset=input_dataset))
  
  
  compare_table_long <- compare_table_wide %>% 
    pivot_wider(id_cols=income_bracket,
                names_from=metric_name,
                values_from=metric_mean) %>% 
    mutate(energy_burden = energy_burden_func(g=income, s=energy_cost),
           ner = ner_func(g=income, s=energy_cost)) %>% 
    pivot_longer(cols=unique(c("income","energy_cost","energy_burden","net_income","ner"), 
                             minimum_metrics_to_compare))
  
  if(include_totals){
    compare_table_totals <- compare_table_wide %>% 
      filter(metric_name=="income") %>% 
      pivot_wider(id_cols=income_bracket,
                  names_from=metric_name,
                  values_from=household_count,
      ) %>% 
      rename(households=income) %>% pivot_longer(cols="households")
    
    compare_table_long <- rbind(compare_table_totals, compare_table_long)
  }
  
  #format the metrics accordingly
  # income = 0 decimal place $
  # energy_cost = 0 decimal place $
  # energy_burden = 0 decimal place %
  # net_income = 0 decimal place $
  # ner = 1 decimal place number
  
  compare_table <- compare_table_long
  
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
  
  return(compare_table)
}

create_printable_comparison_table <- function(
    compare_table=NULL
){
  print_table <- compare_table[,c("Metric Name","display_income_bracket","print_value")] %>% 
    arrange(display_income_bracket) %>% 
    pivot_wider(names_from=display_income_bracket,values_from=print_value)
  return(print_table)
}