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
                          metric_label=NULL,
                          group_columns, 
                          metric_cutoff_level, 
                          metric_cutoff_label,
                          upper_quantile_view=1,
                          lower_quantile_view=0,
                          chart_title=NULL, 
                          chart_subtitle=NULL,
                          chart_caption=NULL,
                          x_label="Proportion of Households"){
  
  if(is.null(metric_label)){
    metric_label <- toupper(metric_name)
  }
  
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
    ggrastr::rasterise(stat_ewcdf(geom='line',  alpha=1, na.rm=T, show.legend = NA, size=0.1)) + 
    ggrastr::rasterise(stat_ewcdf(aes(ymin=..y.., ymax=1), geom='ribbon', alpha=.1, 
               na.rm=T, show.legend = NA)) + 
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
             size=0.5,
             alpha=0.75) +
    annotate("text",
             y = 0.95,#1,
             x = metric_cutoff_level*1.15,
             angle = 0,
             color="red",
             label = metric_cutoff_label,
             vjust = 0.0,#-0.25,
             hjust = 1.0,
             parse = FALSE,
             alpha=0.75,
             size=2.5) +
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
      caption=chart_caption
      ) + 
    coord_flip(xlim=c(0,120),
               ylim=c(0,1),
               expand=FALSE)
  
  return(chart)
}


make_violin_chart <- function(graph_data, 
                             group_columns,
                             metric_name,
                             metric_label,
                             metric_cutoff_level,
                             fill_metric=NULL,
                             fill_label=NULL,
                             upper_quantile_view=.75,
                             lower_quantile_view=.25,
                             group_name=NULL,
                             chart_title=NULL,
                             chart_subtitle=NULL,
                             chart_caption=NULL,
                             reverse_palette=FALSE,
                             x_label=NULL){
  
  gwm <- grouped_weighted_metrics(graph_data=graph_data, 
                                  group_columns=group_columns, 
                                  metric_name=metric_name, 
                                  metric_cutoff_level=metric_cutoff_level, 
                                  upper_quantile_view=upper_quantile_view, 
                                  lower_quantile_view=lower_quantile_view)
  
  ylims <- c(.95*min(gwm$metric_lower[is.finite(gwm$metric_lower)]),
             1.05*max(gwm$metric_upper[is.finite(gwm$metric_upper)]))
  
  if(is.null(x_label)){
    x_label <- paste0(str_to_upper(metric_name)," (",metric_label,")")
  }
  
  # max(boxplot.stats(graph_data[[metric_name]])$stats))
  
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
  wm.summary <- function(y,w){
    return(as.data.frame(y=weighted.median(y,w,na.rm=T)))
  }
  
  pal <- (wes_palette("Zissou1", 100, type = "continuous"))#STCYPR
  if(reverse_palette){
    pal <- rev(pal)
  }

  
  y <- graph_data %>% #[graph_data$state_abbr=="VT",],
    # mutate(!!sym(group_variable) = fct_reorder(!!sym(group_variable), !!sym(metric_name), .fun='median')) %>%
    # mutate(state_abbr = fct_reorder(state_abbr, ner, .fun='median')) %>%
    # {if(!is.null(group_columns)) group_by_at(., .vars=vars(all_of(group_columns))) else .} %>%
    # ggplot(aes_string(
    ggplot(aes(
      # weight=sqrt(households),
      # varwidth = T,
      # weight="group_household_weights",
      x = fct_reorder2(group_name,
                       .x=!!sym(metric_name),
                       .y=group_household_weights,
                     .fun=weighted.median, .desc=F), #!!sym(group_variable),#
      y= !!sym(metric_name),
      weight = group_household_weights,
      fill=if(is.null(fill_metric)){fill_metric}else{.data[[fill_metric]]}
      # group = if(is.null(group_columns)){group_columns}else{
      #   paste0("interaction(", paste0(group_columns, collapse =  ", "), ")")
      # }
      # group=vars(!!plot_group)
    ))+ 
    # geom_point(stat = "weighted.median") +
    # geom_boxplot(inherit.aes = TRUE) + 
    geom_boxplot(#aes_string(fill=fill_metric),
                 width=0.5,#position=position_dodge(width=50, preserve="single"),#stat="median",fun.args=c(na.rm=TRUE),
                 notch=FALSE,
                 alpha=0.9,
                 # color="gray",
                 shape = 18,
                 size = 0.25,
                 outlier.shape = NA,
                 show.legend = TRUE,
                 coef=0,
                 na.rm=TRUE) +
    scale_x_discrete(expand=expansion(mult=c(0.01,0.01)), name=group_name) + 
    # scale_x_discrete(scale_x_discrete(guide = guide_axis(n.dodge=2))) + 
    # geom_violin(trim=FALSE,
    #             alpha=0.3,
    #             outlier.shape=NA,
    #             color="gray",
    #             position=position_dodge(.5)) +
    scale_y_continuous(#labels = scales::dollar_format(accuracy=1),
                       breaks=seq(from=0,to=120,by=10), 
                       # minor_breaks=seq(from=0,to=20,by=.25),
                       name=x_label)
  
    if(!is.null(fill_metric)){
      fill_limits <- c(
        (floor((100*min(graph_data[[fill_metric]]))/5)*5)/100-0.0000001,
        (ceiling((100*max(graph_data[[fill_metric]]))/5)*5)/100+0.0000001
      )
      
      y <- y+scale_fill_gradientn(colours = pal,
                         labels = scales::label_percent(accuracy = 1),
                         n.breaks=4,
                         limits=fill_limits,
                         guide = guide_colorbar(direction="horizontal",
                                                title.position = 'top',
                                                title.hjust = .5,
                                                frame.colour = "black",
                                                ticks.colour = "black"),
                         name=fill_label)}
    # stat_summary(fun.y="weighted.median",
    #              # fun.args = c(na.rm=TRUE),
    #              # aes(weight=group_household_weights),
    #              geom = "point",
    #              # fill="blue",
    #              shape = 18,
    #              size = 1) +
    y <- y + xlab(paste(group_columns,
               sep="_",
               collapse="+")) +
    theme_minimal() + 
    theme(legend.position=c(0.65, 0.25),#"none",
          legend.title.align=0.5,
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
      caption=chart_caption
      #   if(is.null(group_columns)){
      #   group_columns
      # } else {
      #   paste0("By ",paste(group_columns,
      #                      sep="_",
      #                      collapse="+"))
      # }
) + 
    
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
  weighted_metrics,
  include_basemap=TRUE
){
  # cloropleth map by census tract
  clean_data <- st_transform(clean_data, 4326)
  b <- st_bbox(clean_data,crs=3857)
  names(b) <- c("left","bottom","right","top")
  clean_data <- st_transform(clean_data, 3857)
  # centroids <- st_coordinates(st_centroid(clean_data$geometry)) %>% data.frame()
  
  guide_name <- paste0(c("Median ",toupper(metric_name)," (",metric_label,")"),collapse="")
  # centroids <- centroids[is.finite(rowSums(centroids)),]
  
  # center_centroid <- st_centroid()
  map_data <- clean_data[,c("metric_median", "geometry", "state_fips")]
  
  basemap <- ggplot()
  
  if(include_basemap){
    
    # https://stackoverflow.com/questions/52704695/is-ggmap-broken-basic-qmap-produces-arguments-imply-differing-number-of-rows
    basemap_files <- ggmap::get_stamenmap(bbox=b, 
                                zoom=ggmap::calc_zoom(b, adjust=as.integer(0)),
                                maptype="toner-background")
    
    basemap_files <- ggmap_bbox(basemap_files)
    
    basemap <- ggmap(basemap_files)
  }
  
  o <- basemap + geom_sf_rast(
    geom_sf(data=map_data, aes(fill=metric_median, color=metric_median),
            size=0.1,
            # color=NA,#"white"
            alpha=0.8,
            inherit.aes = FALSE), dpi=NULL, dev=NULL) + 
    geom_sf(fill = "transparent", color = "#7B7D7B", #"gray20",
            size = 0.075,
            data = map_data %>% group_by(state_fips) %>% summarise()) #+
    # coord_sf(crs = 4326, #26945, #
    #          xlim = c(b['left'], b['right']),
    #          ylim = c(b['bottom'], b['top']),
    #          expand = TRUE)
  
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
    scale_color_gradientn(
      colors = c("#B22C2C",
                 "#B26F2C",
                 "#B2B22C",
                 "#2CB2B2",
                 "#2C6FB2",
                 "#2C2CB2"),
      limits = c(weighted_metrics$metric_lower,weighted_metrics$metric_upper),
      values = color_values,
      na.value = "#B3B3B3",
      guide = FALSE,
      name=FALSE
    )+
    scale_fill_gradientn(
      colors = c("#B22C2C",
                 "#B26F2C",
                 "#B2B22C",
                 "#2CB2B2",
                 "#2C6FB2",
                 "#2C2CB2"),
      limits = c(weighted_metrics$metric_lower,weighted_metrics$metric_upper),
      values = color_values,
      na.value = "#B3B3B3",
      guide = guide_colorbar(direction="horizontal",
                             title.position = 'bottom'),
      name=guide_name) +
    labs(
      title = chart_title,
      subtitle = chart_subtitle
    ) +
    theme(
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
                            metric_label=NULL, 
                            metric_cutoff_level,
                            metric_cutoff_label,
                            upper_quantile_view=1.0,
                            lower_quantile_view=0.0,
                            chart_title=NULL,
                            chart_subtitle=NULL,
                            chart_caption=NULL,
                            x_label="Proportion of Households"){
  
  
  graph_data <- filter_graph_data(clean_data, group_columns, metric_name)
  
  weighted_metrics <- calculate_weighted_metrics(graph_data, 
                                                 group_columns, 
                                                 metric_name, 
                                                 metric_cutoff_level, 
                                                 upper_quantile_view, 
                                                 lower_quantile_view)
  
  
    
  density_chart <- density_chart(graph_data, 
                                 metric_name, 
                                 metric_label, 
                                 group_columns, 
                                 metric_cutoff_level, 
                                 metric_cutoff_label, 
                                 upper_quantile_view,
                                 lower_quantile_view,
                                 chart_title, 
                                 chart_subtitle,
                                 chart_caption,
                                 x_label=x_label)
  
  # if(length(group_columns)>1){
  #   violin_chart <- "no violin chart yet for groups of factors"
  # }else{
    violin_chart <- make_violin_chart(graph_data, 
                                      group_columns, 
                                      metric_name,
                                      metric_label,
                                      metric_cutoff_level,
                                      fill_metric=NULL,
                                      fill_label=NULL,
                                      upper_quantile_view,
                                      lower_quantile_view,
                                      group_name=NULL,
                                      chart_title,
                                      chart_subtitle)

  # }
  
  return(list("metrics"=weighted_metrics,
              "density"=density_chart,
              # "choropleth"=choropleth_chart,
              "violin"=violin_chart))
}


scatter_chart <- function(graph_data, 
                          metric_name, 
                          metric_label,
                          group_columns, 
                          metric_cutoff_level, 
                          metric_cutoff_label, 
                          chart_title, 
                          chart_subtitle,
                          independent_variable
  
){
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
  
  
  chart <- graph_data %>% 
    ggplot(aes_string(x=metric_name, 
                      y=independent_variable,
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
    ))# + 
  scatter_chart <- chart + 
    stat_density_2d(aes(alpha = ..piece..), geom="polygon") +
    guides(alpha = FALSE) +
    stat_smooth(method = "lm", fullrange = TRUE) +
    geom_rug() + 
    scale_x_continuous(#name = "Monthly Electricity Cost", 
                       #labels = scales::dollar_format(),
                       #limits = c(0, 300), expand = c(0, 0)
                       ) + 
    scale_y_continuous(#name = "EROI", 
                       #labels = scales::unit_format(unit = "x", scale = 1, accuracy = 1),
                       #limits = c(0, 100), expand = c(0, 0)
                       ) + 
    theme_pubr() + #   theme(plot.margin = margin()) + 
    theme(legend.position = "bottom") + 
    theme_void() + 
    theme(legend.position = "none") + 
    labs(title = NULL, x=NULL, y=NULL) + 
    ggtitle("Monthly Electricity Cost vs. EROI") + 
    plot_spacer() + 
    electricity_v_eroi_contour_plot + 
    theme(legend.justification = c(1, 1), legend.position = c(1, 1), legend.title=element_blank()) + 
    mean_eroi + 
    coord_flip() + 
    theme_void() + 
    theme(legend.position = "none") + 
    labs(title = NULL, x=NULL, y=NULL) + 
    plot_layout(ncol = 2, nrow = 2, widths = c(4, 1), heights = c(1, 4))
}