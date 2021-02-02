filter_graph_data <- function(clean_data, group_columns, metric_name){
  graph_data <- clean_data %>% 
    {if(!is.null(group_columns)) group_by_at(., .vars=vars(all_of(group_columns))) else .} %>% 
    mutate(group_households = sum(households)) %>% 
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
    summarise(household_count = sum(households),
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
                                       upper_quantile_view, 
                                       lower_quantile_view){
  
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

# weighted_metrics <- calculate_weighted_metrics(graph_data, 
#                                          group_columns, 
#                                          metric_name, 
#                                          metric_cutoff_level, 
#                                          upper_quantile_view, 
#                                          lower_quantile_view)
# weighted_metrics

#' @rdname ggplot2-ggproto
#' @format NULL
#' @usage NULL
#' @export
#' 

StatEcdf <- ggproto("StatEcdf", Stat,
                    compute_group = function(data, scales, weight, n = NULL, pad = TRUE) {
                      # If n is NULL, use raw values; otherwise interpolate
                      if (is.null(n)) {
                        x <- unique(data$x)
                      } else {
                        x <- seq(min(data$x), max(data$x), length.out = n)
                      }
                      
                      if (pad) {
                        x <- c(-Inf, x, Inf)
                      }
                      y <- ewcdf(data$x, weights=data$weight/sum(data$weight))(x)
                      
                      data.frame(x = x, y = y)
                    },
                    
                    default_aes = aes(y = stat(y)),
                    
                    required_aes = c("x")
)

stat_ewcdf <- function(mapping = NULL, data = NULL,
                       geom = "step", position = "identity",
                       weight =  NULL, 
                       ...,
                       n = NULL,
                       pad = TRUE,
                       na.rm = FALSE,
                       show.legend = NA,
                       inherit.aes = TRUE) {
  layer(
    data = data,
    mapping = mapping,
    stat = StatEcdf,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      n = n,
      pad = pad,
      na.rm = na.rm,
      weight = weight,
      ...
    )
  )
}

density_chart <- function(graph_data, 
                          metric_name, 
                          metric_label,
                          group_columns, 
                          metric_cutoff_level, 
                          metric_cutoff_label, 
                          chart_title, 
                          chart_subtitle,
                          x_label="Proportion of Households"){
  
  legend_title <- group_columns
  if(!is.null(legend_title)){
    # remove simplified_
    legend_title <- gsub("simplified_","", legend_title, fixed=TRUE)
    # normalize words
    legend_title <- gsub("_"," ",legend_title,fixed=TRUE)
    legend_title <- str_to_title(legend_title)
    # combine
    legend_title <- paste0(paste(legend_title, sep=" ", collapse=" &\n"))
  }
  print(legend_title)
  
  if(!is.null(group_columns)){
    pal_n <- length(levels(interaction(graph_data[,group_columns])))
  } else {
    pal_n <- 1
  }
  
  graph_data$group_name <- str_to_title(gsub("+"," &\n",
                                             graph_data$group_name,fixed=TRUE))
  
  movie <- "Darjeeling1" #"GrandBudapest1"
  #pal <- wes_palette(name=movie, n=pal_n, type="continuous")
  pal <- sample(x=wes_palette(name=movie, n=pal_n, type="continuous"), 
                size = pal_n, 
                replace = FALSE)
  
  weighted_metrics <- calculate_weighted_metrics(graph_data, 
                                                 group_columns, 
                                                 metric_name, 
                                                 metric_cutoff_level, 
                                                 upper_quantile_view, 
                                                 lower_quantile_view)
  
  
  chart <- graph_data %>% #subset(!is.na(households)) %>% head()
    # ggplot(aes(x=!!sym(metric_name), 
    #            weight=group_household_weights,
    #            color=interaction(!!!sym(group_columns)),
    #            fill=interaction(!!!sym(group_columns))
    # )) + 
    ggplot(aes_string(x=metric_name, 
                      weight="group_household_weights",
                      color=if(is.null(group_columns)){group_columns}else{
                        "group_name"
                        #interaction(!!!sym(group_columns))
                        # paste0("interaction(", paste0(group_columns, collapse =  ", "), ")")
                      },
                      fill=if(is.null(group_columns)){group_columns}else{
                        #interaction(!!!sym(group_columns))
                        "group_name"
                        # paste0("interaction(", paste0(group_columns, collapse =  ", "), ")")
                      },
                      linetype=if(is.null(group_columns)){group_columns}else{
                        #interaction(!!!sym(group_columns))
                        "group_name"
                        # paste0("interaction(", paste0(group_columns, collapse =  ", "), ")")
                      }
    )) + 
    stat_ewcdf(geom='line',  alpha=1, na.rm=T, show.legend = NA, size=0.2) + 
    stat_ewcdf(aes(ymin=..y.., ymax=1), geom='ribbon', alpha=.1, 
               na.rm=T, show.legend = NA) + 
    theme_minimal() + 
    scale_color_manual(name=legend_title, values=pal) + 
    scale_fill_manual(name=legend_title, values=pal) + 
    scale_linetype(name=legend_title) + 
    scale_x_continuous(#labels = scales::dollar_format(accuracy=1),
                       breaks=seq(from=0,to=120,by=20), 
                       # minor_breaks=seq(from=0,to=20,by=.25),
                       name=metric_label) + 
    scale_y_continuous(labels = scales::label_percent(accuracy = 1), 
                       breaks=seq(from=0,to=1,by=.25), 
                       # minor_breaks=seq(from=0,to=1,by=.05),
                       name=x_label) + 
    theme(legend.justification = c(0, 1), 
          legend.position = c(0, 1), 
          legend.title=element_text(size=8),#element_blank(),
          legend.text=element_text(size=6), 
          legend.key.size = unit(10, "points"),
          panel.background = element_blank(),#element_rect(fill="#f1f1f1"),
          panel.grid.major = element_blank(),#element_line(color="#DCDCDC"),
          panel.grid.minor = element_blank(),#element_line(color="#DCDCDC"),
          axis.line = element_line(color = "black",
                                   size = 0.5, 
                                   linetype = "solid"),
          axis.text.x=element_text(angle=45, 
                                   hjust=1,
                                   vjust=NULL,
                                   margin=margin(t = 5, 
                                                 r = 0, 
                                                 b = 0, 
                                                 l = 0, 
                                                 unit = "pt")),
          axis.text.y=element_text(angle=10, 
                                   hjust=1,
                                   vjust=0.5,
                                   margin=margin(t = 0, 
                                                 r = 5, 
                                                 b = 0, 
                                                 l = 0, 
                                                 unit = "pt")),
          axis.ticks=element_line(color = "black"),
          axis.ticks.length = unit(-0.1, "cm")) + 
    # guides(guide_legend(override.aes = list(size = .01))
    guides(guide_legend(override.aes = list(size = 0.05))#,
           # fill = guide_legend(override.aes = list(size = 0)),
           # linetype = guide_legend(override.aes = list(size = 0)),
           # ribbon = guide_legend(override.aes = list(size = 0))
    ) + 
    # geom_segment(y = 0,
    #              x = as.numeric(weighted_medians[weighted_medians$group=="All",c("median_eroi")]),
    #              yend = 0.5,
    #              xend = as.numeric(weighted_medians[weighted_medians$group=="All",c("median_eroi")]),
    #              color="gray25",
    #              linetype="dotted",
    #              size=0.25,
    #              alpha=0.5) +
    # geom_segment(y = 0.5,
    #              x = as.numeric(weighted_medians[weighted_medians$group=="All",c("median_eroi")]),
    #              yend = 0.5,
  #              xend = 0,
  #              color="gray25",
  #              linetype="dotted",
  #              size=0.25,
  #              alpha=0.5) +
  geom_vline(xintercept = metric_cutoff_level,
             linetype="dotted",
             color = "red",
             size=0.2,
             alpha=0.75) +
    annotate("text",
             y = 0.95,#1,
             x = metric_cutoff_level*0.95,
             angle = 0,
             color="red",
             label = metric_cutoff_label,
             vjust = 0.0,#-0.25,
             hjust = 1.0,
             parse = FALSE,
             alpha=0.75,
             size=2.25) +
    # annotate("text", 
    #          y = 0, 
    #          x = max(weighted_medians$median_eroi), 
    #          angle = 0, 
    #          color="gray25", 
    #          label = "Median", 
    #          vjust = -0.25, 
    #          hjust = -0.1, 
    #          parse = FALSE, 
    #          alpha=0.75) + 
    labs(
      title=chart_title,
      subtitle=chart_subtitle,
      caption=NULL
      ) + 
    coord_flip(xlim=c(0,120),
               ylim=c(0,1),
               expand=FALSE)
  
  return(chart)
}


make_violin_chart <- function(graph_data, 
                             group_columns, 
                             metric_name,
                             metric_cutoff_level,
                             upper_quantile_view=1.0,
                             lower_quantile_view=0.0,
                             chart_title=NULL,
                             chart_subtitle=NULL){
  
  gwm <- grouped_weighted_metrics(graph_data=graph_data, 
                                  group_columns=group_columns, 
                                  metric_name=metric_name, 
                                  metric_cutoff_level=metric_cutoff_level, 
                                  upper_quantile_view=upper_quantile_view, 
                                  lower_quantile_view=lower_quantile_view)
  
  ylims <- c(min(max(0,gwm$metric_lower[is.finite(gwm$metric_lower)])),
             max(boxplot.stats(graph_data[["ner"]])$stats))
  
  # if(is.null(group_columns)){
  #   sort_column<-"gisjoin"
  # }else if(length(group_columns)==1){
  #   sort_column <- group_columns
  # }else{paste0(sort_column <- group_columns[1])}
  
  f <- function(column) {
    column <- sym(column)
    ggplot(rates.by.groups, 
           aes(x = name, 
               y = rate, 
               fill  = !!column, 
               group = !!column)) +
      geom_col()
  }
  
  # plot_group <- sym(if(is.null(group_columns)){
  #       group_columns}else{
  #         paste0("interaction(", paste0(group_columns, collapse =  ", "), ")")})
  # print(names(graph_data))
  # print(str(graph_data$group_name))
  
  y <- graph_data %>% #[graph_data$state_abbr=="VT",],
    # mutate(!!sym(group_variable) = fct_reorder(!!sym(group_variable), !!sym(metric_name), .fun='median')) %>%
    # mutate(state_abbr = fct_reorder(state_abbr, ner, .fun='median')) %>%
    # {if(!is.null(group_columns)) group_by_at(., .vars=vars(all_of(group_columns))) else .} %>%
    # ggplot(aes_string(
    ggplot(aes(
      # weight=sqrt(households),
      # varwidth = T,
      # weight="group_household_weights",
      x = fct_reorder(group_name,
        # if(is.null(group_columns)){sort_column}else{
        #   #interaction(!!!sym(group_columns))
        #   paste0("interaction(", paste0(group_columns, collapse =  ", "), ")")
        # },
                     !!sym(metric_name),
                     .fun=median, na.rm=T, .desc=F), #!!sym(group_variable),#
      y= !!sym(metric_name)#,
      # group = if(is.null(group_columns)){group_columns}else{
      #   paste0("interaction(", paste0(group_columns, collapse =  ", "), ")")
      # }
      # group=vars(!!plot_group)
    ))+ 
    # geom_point(stat = "identity") + 
    # geom_boxplot(inherit.aes = TRUE) + 
    geom_boxplot(width=0.5,#position=position_dodge(width=50, preserve="single"),#stat="median",fun.args=c(na.rm=TRUE),
                 notch=TRUE, color="gray", shape = 18, size = 0.5,
                 outlier.shape = NA, na.rm=TRUE) + 
    scale_x_discrete(expand=expansion(mult=c(0.01,0.01))) + 
    # scale_x_discrete(scale_x_discrete(guide = guide_axis(n.dodge=2))) + 
    # geom_violin(trim=FALSE, alpha=0.3, outlier.shape=NA, color="gray",position=position_dodge(.5)) + 
    scale_y_continuous(#labels = scales::dollar_format(accuracy=1),
                       breaks=seq(from=0,to=100,by=10), 
                       # minor_breaks=seq(from=0,to=20,by=.25),
                       name=metric_label) + 
    stat_summary(fun=median,fun.args=c(na.rm=TRUE),
                 geom = "point", shape = 18, size = 1) +
    xlab(paste(group_columns,
               sep="_",
               collapse="+")) +
    theme_minimal() + 
    theme(legend.position="none",
          # legend.justification = c(1, 1), 
          # legend.position = c(0.25, 1), 
          # legend.title=element_blank(),
          panel.background = element_blank(),#element_rect(fill="#f1f1f1"),
          panel.grid.major = element_blank(),#element_line(color="#DCDCDC"),
          panel.grid.minor = element_blank(),#element_line(color="#DCDCDC"),
          axis.line = element_line(color = "black",
                                   size = 0.5, 
                                   linetype = "solid"),
          axis.text.x=element_text(angle=45, 
                                   hjust=1,
                                   vjust=NULL,
                                   margin=margin(t = 5, 
                                                 r = 0, 
                                                 b = 0, 
                                                 l = 0, 
                                                 unit = "pt")),
          axis.text.y=element_text(angle=10, 
                                   hjust=1,
                                   vjust=0.5,
                                   margin=margin(t = 0, 
                                                 r = 5, 
                                                 b = 0, 
                                                 l = 0, 
                                                 unit = "pt")),
          axis.ticks=element_line(color = "black"),
          axis.ticks.length.x = unit(2, "points"),
          axis.ticks.length.y = unit(-2, "points"),
          axis.text=element_text(size=6)
          ) + 
    labs(
      title=chart_title,
      subtitle=chart_subtitle,
      caption=if(is.null(group_columns)){
        group_columns
      } else {
        paste0("By ",paste(group_columns,
                           sep="_",
                           collapse="+"))
      }) + 
    
    # coord_cartesian(ylim=as.numeric(ylims)*1.05)
    coord_flip(ylim = as.numeric(ylims))
  # print("graph_2")
  return(y)
}

choropleth_map <- function(
  clean_data,
  group_columns,
  metric_name,
  metric_label,
  metric_cutoff_level,
  metric_cutoff_label,
  upper_quantile_view,
  lower_quantile_view,
  chart_title,
  chart_subtitle,
  weighted_metrics
){
  # cloropleth map by census tract
  centroids <- st_coordinates(st_centroid(clean_data$geometry)) %>% data.frame()
  
  guide_name <- paste0(c("Median ",toupper(metric_name)," (",metric_label,")"),collapse="")
  # centroids <- centroids[is.finite(rowSums(centroids)),]
  
  # center_centroid <- st_centroid()
  
  o <- clean_data[,c("metric_median", "geometry", "state_fips")] %>% 
    ggplot() + 
    geom_sf(aes(fill=metric_median),
            # size=0.005, 
            color=NA,#"white"
            alpha=0.9) + 
    geom_sf(fill = "transparent", color = "#7B7D7B", #"gray20", 
            size = 0.05, 
            data = . %>% group_by(state_fips) %>% summarise()) +
    coord_sf(crs = 4326, #26945, #
             xlim = c(min(centroids$X), max(centroids$X)), 
             ylim = c(min(centroids$Y), max(centroids$Y)), 
             expand = TRUE)
  
  color_values <- as.numeric(sort(c(
    # weighted_metrics$metric_lower,
    weighted_metrics$metric_min,
    metric_cutoff_level,
    weighted_metrics$metric_median,
    weighted_metrics$metric_max
    # weighted_metrics$metric_upper
  ), decreasing=F))
  
  color_values <- scales::rescale(x=color_values, to=c(0,1))
  
  p <- o +
    # geom_polygon(data = spdf_fortified, 
    #              aes(fill = nb_equip, x = long, y = lat, group = group) , 
    #              size=0, alpha=0.9) +
    theme_void() +
    # scale_colour_identity(
    # scale_fill_identity() +
    scale_fill_gradientn(#) +
      # scale_fill_gradient(low = "#616161", high = "white",
      # scale_fill_gradient2(
      # scale_fill_continuous(
      # low = muted("red"), mid = "white",
      # high = muted("blue"), midpoint = ner_poverty_line,
      # name="Net Income per Energy Spend ($/kWh)",
      # colors = c("#ff5252","#ff5252","#fff176","#228b22","#228b22"),
      colors = c(#"#C83130","#C7C830","#31C831"
                 # "#8B2221","#8B5621","#228B22"
                 # "#B22C2C","#B26F2C","#279F27"
                 "#B22C2C","#B26F2C","#B2B22C","#2CB2B2","#2C6FB2","#2C2CB2"
                 # "#9F2727","#9F6327","#629F27"
                #"#8B2156",
                 # "#8B2221",
                 # "#8B5621",
                 # "#8A8B21",
                 # "#568B21"#,
                 # "#228b22"#,
                 # "#21218B"
                 ),
      limits = c(weighted_metrics$metric_lower,weighted_metrics$metric_upper),
      # na.value = "black",
      # na.value = "#616161",
      values = color_values,
      # guide = guide_colorbar(direction="horizontal",
      # title.position = 'bottom'
      # )
      # ) +
      # scale_fill_viridis(option="plasma",#"cividis",#"inferno",#"magma",#"viridis",#
      # trans="log",
      # breaks=color_values, 
      # colors = c("#ff5252","#ff5252","#fff176","#228b22","#228b22"),
      # na.value = "black",
      na.value = "#B3B3B3", # ", #808080",#"#D3D3D3",#"#616161",#
      # breaks = color_values,
      # minor_breaks = NULL, 
      guide = guide_colorbar(direction="horizontal",
                             title.position = 'bottom'),
      name=guide_name) +
    #keywidth=unit(4, units = "mm"),
    # label.position = "bottom",
    # , nrow=1) ) +
    
    labs(
      title = chart_title,
      subtitle = chart_subtitle#,
      # caption = "Data: NREL" # | Creation: Eric Scheier | emergi.eco"
    ) +
    theme(
      # text = element_text(color = "#22211d"),
      # plot.background = element_rect(fill = "#f5f5f2", color = NA),
      # panel.background = element_rect(fill = "#f5f5f2", color = NA),
      # legend.background = element_rect(fill = "#f5f5f2", color = NA),
      
      # plot.title = element_text(size= 22, 
      #                           hjust=0.01, 
      #                           color = "#4e4d47", 
      #                           margin = margin(b = -0.1, t = 0.4, l = 2, unit = "cm")),
      # plot.subtitle = element_text(size= 17, 
      #                              hjust=0.01, 
      #                              color = "#4e4d47", 
      #                              margin = margin(b = -0.1, t = 0.43, l = 2, unit = "cm")),
      # plot.caption = element_text( size=12, 
      #                              color = "#4e4d47", 
      #                              margin = margin(b = 0.3, r=-99, unit = "cm") ),
      
      legend.position = c(0.20, 0.15)
    ) #+
  #coord_map()
  return(p)
}

# https://www.r-graph-gallery.com/330-bubble-map-with-ggplot2.html
# Left chart
# ggplot() +
#   geom_polygon(data = UK, aes(x=long, y = lat, group = group), fill="grey", alpha=0.3) +
#   geom_point( data=data, aes(x=long, y=lat)) +
#   theme_void() + ylim(50,59) + coord_map() 
# 
# # Second graphic with names of the 10 biggest cities
# library(ggrepel)
# ggplot() +
#   geom_polygon(data = UK, aes(x=long, y = lat, group = group), fill="grey", alpha=0.3) +
#   geom_point( data=data, aes(x=long, y=lat, alpha=pop)) +
#   geom_text_repel( data=data %>% arrange(pop) %>% tail(10), aes(x=long, y=lat, label=name), size=5) +
#   geom_point( data=data %>% arrange(pop) %>% tail(10), aes(x=long, y=lat), color="red", size=3) +
#   theme_void() + ylim(50,59) + coord_map() +
#   theme(legend.position="none")
# 
# # virids package for the color palette
# library(viridis)
# 
# # Left: use size and color
# ggplot() +
#   geom_polygon(data = UK, aes(x=long, y = lat, group = group), fill="grey", alpha=0.3) +
#   geom_point( data=data, aes(x=long, y=lat, size=pop, color=pop)) +
#   scale_size_continuous(range=c(1,12)) +
#   scale_color_viridis(trans="log") +
#   theme_void() + ylim(50,59) + coord_map() 
# 
# # Center: reorder your dataset first! Big cities appear later = on top
# data %>%
#   arrange(pop) %>% 
#   mutate( name=factor(name, unique(name))) %>% 
#   ggplot() +
#   geom_polygon(data = UK, aes(x=long, y = lat, group = group), fill="grey", alpha=0.3) +
#   geom_point( aes(x=long, y=lat, size=pop, color=pop), alpha=0.9) +
#   scale_size_continuous(range=c(1,12)) +
#   scale_color_viridis(trans="log") +
#   theme_void() + ylim(50,59) + coord_map() + theme(legend.position="none")
# 
# # Right: just use arrange(desc(pop)) instead
# data %>%
#   arrange(desc(pop)) %>% 
#   mutate( name=factor(name, unique(name))) %>% 
#   ggplot() +
#   geom_polygon(data = UK, aes(x=long, y = lat, group = group), fill="grey", alpha=0.3) +
#   geom_point( aes(x=long, y=lat, size=pop, color=pop), alpha=0.9) +
#   scale_size_continuous(range=c(1,12)) +
#   scale_color_viridis(trans="log") +
#   theme_void() + ylim(50,59) + coord_map() + theme(legend.position="none")

make_all_charts <- function(clean_data,
                            group_columns,
                            metric_name,
                            metric_label, 
                            metric_cutoff_level,
                            metric_cutoff_label,
                            upper_quantile_view=1.0,
                            lower_quantile_view=0.0,
                            chart_title,
                            chart_subtitle,
                            x_label="Proportion of Households"){
  
  
  graph_data <- filter_graph_data(clean_data, group_columns, metric_name)
  
  weighted_metrics <- calculate_weighted_metrics(graph_data, 
                                                 group_columns, 
                                                 metric_name, 
                                                 metric_cutoff_level, 
                                                 upper_quantile_view=1.0, 
                                                 lower_quantile_view=0.0)
  
  
    
  density_chart <- density_chart(graph_data, 
                                 metric_name, 
                                 metric_label, 
                                 group_columns, 
                                 metric_cutoff_level, 
                                 metric_cutoff_label, 
                                 chart_title, 
                                 chart_subtitle,
                                 x_label=x_label)
  
  # if(length(group_columns)>1){
  #   violin_chart <- "no violin chart yet for groups of factors"
  # }else{
    violin_chart <- make_violin_chart(graph_data, 
                                      group_columns, 
                                      metric_name,
                                      metric_cutoff_level,
                                      upper_quantile_view,
                                      lower_quantile_view,
                                      chart_title,
                                      chart_subtitle)
  # }
  
  return(list("density"=density_chart,
              # "choropleth"=choropleth_chart,
              "violin"=violin_chart))
}