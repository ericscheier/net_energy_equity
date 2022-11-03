filter_graph_data <- function(clean_data, group_columns, metric_name){
  graph_data <- clean_data %>% 
    {if(!is.null(group_columns)) group_by_at(., .vars=vars(all_of(group_columns))) else .} %>% 
    mutate(group_households = sum(households, na.rm = TRUE)) %>% 
    mutate(group_household_weights = ifelse(group_households==0,0,households/group_households)) %>% 
    arrange(!!sym(metric_name)) %>% 
    mutate(group_percentile = cumsum(households * group_household_weights),
           overall_percentile = cumsum(households)/sum(households)#,
    ) %>% 
    # group_map( ~ .x %>% 
    #              mutate(group_name =  paste(.y, collapse="+")), keep=TRUE) %>% 
    {if(!is.null(group_columns)) 
      unite(., "group_name", all_of(group_columns), remove=FALSE, sep="+", na.rm=FALSE) else 
        (mutate(., group_name="all"))} %>% 
    ungroup()
  return(graph_data)
}

grouped_weighted_metrics <- function(graph_data, 
                                     group_columns, 
                                     metric_name, 
                                     metric_cutoff_level, 
                                     upper_quantile_view=1.0, 
                                     lower_quantile_view=0.0){
  # grouped_weighted_medians <- graph_data %>% 
  #    group_by_at(.vars=vars(all_of(group_columns))) %>% 
  #    summarise(metric_median = if( sum(!is.na(households))<3 ){NA} else { weighted.median(!!sym(metric_name), households, na.rm=TRUE)})
  
  weighted_metrics <- graph_data %>% 
    dplyr::filter(is.finite(!!sym(metric_name)), .preserve = TRUE) %>% 
    {if(!is.null(group_columns)) group_by_at(., .vars=vars(all_of(group_columns))) else .} %>% 
    dplyr::summarise(household_count = sum(households),
                     total_na = sum(is.na(!!sym(metric_name)) * households, na.rm = TRUE), 
                     households_below_cutoff = 
                       sum((!!sym(metric_name) < metric_cutoff_level) * households, na.rm = TRUE), 
                     metric_max = max(!!sym(metric_name), na.rm = TRUE),
                     metric_min = min(!!sym(metric_name), na.rm = TRUE),
                     metric_mean = if( sum(!is.na(households*!!sym(metric_name)))<3 || 
                                       all(households==0) ){NA}else{
                                         weighted.mean(x=!!sym(metric_name), w=households, na.rm = TRUE) },
                     metric_median = if( sum(!is.na(households*!!sym(metric_name)))<3 || 
                                         all(households==0) ){NA}else{
                                           weighted.quantile(x=!!sym(metric_name), w=households, probs=c(.5), na.rm=TRUE)},
                     metric_upper = if( sum(!is.na(households*!!sym(metric_name)))<3 || 
                                        all(households==0) ){NA}else{
                                          weighted.quantile(x=!!sym(metric_name), w=households,
                                                            probs=c(upper_quantile_view), na.rm=TRUE)},
                     metric_lower = if( sum(!is.na(households*!!sym(metric_name)))<3 || 
                                        all(households==0) ){NA}else{
                                          weighted.quantile(x=!!sym(metric_name), w=households,
                                                            probs=c(lower_quantile_view), na.rm=TRUE)}) %>% 
    mutate(households_pct = household_count/sum(household_count),
           pct_in_group_below_cutoff = households_below_cutoff/household_count,
           pct_total_below_cutoff = households_below_cutoff/sum(households_below_cutoff))
  
  # overall_weighted_metrics <- graph_data %>% ungroup() %>%
  #   summarise(metric_median = if( sum(!is.na(households))<3 ){NA} else { weighted.quantile(x=!!sym(metric_name), w=households, probs=c(.5), na.rm=TRUE)},
  #             metric_upper = if( sum(!is.na(households))<3 ){NA} else { weighted.quantile(x=!!sym(metric_name), w=households, 
  #                                                                                           probs=c(upper_quantile_view), na.rm=TRUE)},
  #             metric_lower = if( sum(!is.na(households))<3 ){NA} else { weighted.quantile(x=!!sym(metric_name), w=households, 
  #                                                                                           probs=c(lower_quantile_view), na.rm=TRUE)}
  #   )
  
  #  all_groups <- as.data.frame(matrix(rep("All",length(group_columns)),nrow=1))
  #  names(all_groups) <- group_columns
  #  overall_weighted_median <- as_tibble(cbind(all_groups, overall_weighted_median))
  
  #  weighted_quantiles <- bind_rows(grouped_weighted_medians,overall_weighted_median) %>% ungroup() %>% 
  #    mutate_at(.vars=vars(all_of(group_columns)), .funs=as.factor)
  
  #  return(weighted_quantiles)
  return(weighted_metrics)
}

calculate_weighted_metrics <- function(graph_data, 
                                       group_columns, 
                                       metric_name, 
                                       metric_cutoff_level, 
                                       upper_quantile_view=1.0, 
                                       lower_quantile_view=0.0){
  
  weighted_metrics <- grouped_weighted_metrics(graph_data, 
                                               group_columns=NULL, 
                                               metric_name, 
                                               metric_cutoff_level, 
                                               upper_quantile_view, 
                                               lower_quantile_view)
  
  if(!is.null(group_columns)){
    all_groups <- as.data.frame(matrix(rep("All",length(group_columns)),nrow=1))
    names(all_groups) <- group_columns
    weighted_metrics <- as_tibble(cbind(all_groups, weighted_metrics))
    
    grouped_weighted_metrics <- grouped_weighted_metrics(graph_data, 
                                                         group_columns=group_columns, 
                                                         metric_name, 
                                                         metric_cutoff_level, 
                                                         upper_quantile_view, 
                                                         lower_quantile_view) %>% 
      ungroup() %>% 
      mutate_at(.vars=vars(all_of(group_columns)), .funs=as.factor)
    # print(sapply(c(grouped_weighted_metrics, weighted_metrics),function(x){str(x[group_columns])}))
    weighted_metrics <- bind_rows(grouped_weighted_metrics,weighted_metrics) %>% ungroup() %>% 
      mutate_at(.vars=vars(all_of(group_columns)), .funs=as.factor)
    
    
  }else{
    weighted_metrics <- data.frame(group=as.factor(rep("All", nrow(weighted_metrics))), weighted_metrics)
  }
  
  return(weighted_metrics)
}

to_dollar <- function(x,latex=FALSE){
  if(latex){
    y <- scales::label_dollar(largest_with_cents = 10,prefix="\\$")(x)
  } else {
    y <- scales::label_dollar(largest_with_cents = 10)(x)
  }
  y[is.na(x)] <- ""
  return(y)
}

to_percent <- function(x,latex=FALSE){
  if(latex){
    y <- scales::label_percent(accuracy = 1,big.mark=",",suffix="\\%")(x)
  } else {
    y <- scales::label_percent(accuracy = 1,big.mark=",")(x)
  }
  y[is.na(x)] <- ""
  return(y)
}

to_big <- function(x){
  y <- scales::label_comma(accuracy=1,big.mark = ",")(x)
  y[is.na(x)] <- ""
  return(y)
}

to_million <- function(x, suffix=" million", override_to_k=TRUE){
  if(abs(x)<10^6){
    y <- scales::label_number(accuracy = 1, suffix = "k")(x * 10^-3)
  } else {
    y <- scales::label_number(accuracy = 0.1, suffix = suffix)(x * 10^-6)
  }
  y[is.na(x)] <- ""
  return(y)
}

#https://bookdown.org/yihui/rmarkdown-cookbook/font-color.html
colorize <- function(x, color) {
  if (knitr::is_latex_output()) {
    sprintf("\\textcolor{%s}{%s}", color, x)
  } else if (knitr::is_html_output()) {
    sprintf("<span style='color: %s;'>%s</span>", color,
            x)
  } else x
}