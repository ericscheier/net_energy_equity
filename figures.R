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
  # print(legend_title)
  
  if(!is.null(group_columns)){
    pal_n <- length(levels(interaction(graph_data[,group_columns])))
  } else {
    pal_n <- 1
  }
  
  graph_data$group_name <- str_to_title(gsub("+"," &\n",
                                             graph_data$group_name,fixed=TRUE))
  
  movie <- "Darjeeling1" #"GrandBudapest1"
  #pal <- wes_palette(name=movie, n=pal_n, type="continuous")
  pal <- rev(sample(x=wes_palette(name=movie, n=pal_n, type="continuous"),
                size = pal_n,
                replace = FALSE))
  
  weighted_metrics <- calculate_weighted_metrics(graph_data, 
                                                 group_columns, 
                                                 metric_name, 
                                                 metric_cutoff_level, 
                                                 upper_quantile_view, 
                                                 lower_quantile_view)
  
  
  chart <- graph_data %>% subset(!is.na(households)) %>% #head()
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
    ggrastr::rasterise(stat_ewcdf(geom='line',  
                                  alpha=1, 
                                  na.rm=T, 
                                  show.legend = NA, 
                                  size=0.2),
                       dpi=300
                       ) + 
    # ggrastr::rasterise(stat_ewcdf(aes(ymin=..y.., ymax=1), geom='ribbon', alpha=.1, 
               # na.rm=T, show.legend = NA)) + 
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
          legend.spacing.y = unit(0.05, 'points'),
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
          axis.title.x = element_text(face="bold"),
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
  
 # x_label <- x_label
  
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

# scale_x_longitude <- function(xmin=-180, xmax=180, step=0.002, ...) {
#   xbreaks <- seq(xmin,xmax,step)
#   xlabels <- unlist(
#     lapply(xbreaks, function(x){
#       ifelse(x < 0, parse(text=paste0(paste0(abs(dms(x)$d), expression("*{degree}*")),
#                                       paste0(abs(dms(x)$m), expression("*{minute}*")),
#                                       paste0(abs(dms(x)$s)), expression("*{second}*W"))), 
#              ifelse(x > 0, parse(text=paste0(paste0(abs(dms(x)$d), expression("*{degree}*")),
#                                              paste0(abs(dms(x)$m), expression("*{minute}*")),
#                                              paste0(abs(dms(x)$s)), expression("*{second}*E"))),
#                     "0"#abs(dms(x))
#                     ))}))
#   return(scale_x_continuous("Longitude", breaks = xbreaks, labels = xlabels, expand = c(0, 0), ...))
# }
# 
# scale_y_latitude <- function(ymin=-90, ymax=90, step=0.002, ...) {
#   ybreaks <- seq(ymin,ymax,step)
#   ylabels <- unlist(
#     lapply(ybreaks, function(x){
#       ifelse(x < 0, parse(text=paste0(paste0(abs(dms(x)$d), expression("*{degree}*")),
#                                       paste0(abs(dms(x)$m), expression("*{minute}*")),
#                                       paste0(abs(dms(x)$s)), expression("*{second}*S"))), 
#              ifelse(x > 0, parse(text=paste0(paste0(abs(dms(x)$d), expression("*{degree}*")),
#                                              paste0(abs(dms(x)$m), expression("*{minute}*")),
#                                              paste0(abs(dms(x)$s)), expression("*{second}*N"))),
#                     "0"#abs(dms(x))
#                     ))}))
#   return(scale_y_continuous("Latitude", breaks = ybreaks, labels = ylabels, expand = c(0, 0), ...))
# }  

FootnoteGridArrange <- function(
  footnoteText = strftime(Sys.time(), format = "%d/%m/%Y"),
  size = 1.2,
  color = "black") {
  grid::textGrob(
    footnoteText,
    gp = grid::gpar(cex = size),
    hjust = 1,
    x = 1
  )
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
  include_basemap="terrain", #maptype = c("terrain", "terrain-background", "terrain-labels", "terrain-lines", "toner", "toner-2010", "toner-2011", "toner-background", "toner-hybrid","toner-labels", "toner-lines", "toner-lite", "watercolor")
  legend_position=c(0.15, 0.125),
  legend_direction="horizontal",
  include_compass=FALSE,
  compass_location="bottomright",
  metric_long_name=NULL,
  include_borders=FALSE,
  border_field=NULL,
  flip_scale=FALSE,
  keep_coordinates=TRUE,
  infer_missing=FALSE
){
  
  clean_data <- st_transform(clean_data, 4326)
  b <- st_bbox(clean_data,crs=3857)
  names(b) <- c("left","bottom","right","top")
  clean_data <- st_transform(clean_data, 3857)
  # centroids <- st_coordinates(st_centroid(clean_data$geometry)) %>% data.frame()
  
  if(is.null(metric_long_name)){
    metric_long_name <- toupper(metric_name)
  }
  
  #guide_name <- paste0(c("Average ",metric_long_name," (",metric_label,")"),collapse="")
  guide_name <- as.expression(bquote(~italic(
                                       .(metric_long_name))~
                                       "("*.(metric_label)*")"))
  # centroids <- centroids[is.finite(rowSums(centroids)),]
  
  # center_centroid <- st_centroid()
  gwm <- calculate_weighted_metrics(filter_graph_data(st_drop_geometry(clean_data),
                                                      c("geoid"),
                                                      metric_name),
                                    c("geoid"),
                                    metric_name,
                                    metric_cutoff_level=0,
                                    upper_quantile_view = 1,
                                    lower_quantile_view=0) %>% 
    mutate(geoid=as.character(geoid))
  
  gwm$metric_name <- metric_name
  
  map_data <- left_join(clean_data, gwm, by=c("geoid"))[,c("metric_mean", "geometry", border_field)]
  
  if(infer_missing!=FALSE){
    if(infer_missing=="max"){
      map_data$metric_mean <- ifelse(is.na(map_data$metric_mean),
                                max(map_data$metric_mean, na.rm=TRUE),
                                map_data$metric_mean)
    } else {
      map_data$metric_mean <- ifelse(is.na(map_data$metric_mean),
                                min(map_data$metric_mean, na.rm=TRUE),
                                map_data$metric_mean)
    }
  }
  
  # map_data
  # 
  fill_transparency <- 1.0
  
  basemap <- ggplot()
  
  if(include_basemap!=FALSE){
    
    fill_transparency <- 0.6
    
    # https://stackoverflow.com/questions/52704695/is-ggmap-broken-basic-qmap-produces-arguments-imply-differing-number-of-rows
    basemap_files <- ggmap::get_stamenmap(bbox=b, 
                                zoom=ggmap::calc_zoom(b, adjust=as.integer(0)),
                                maptype=include_basemap)
    
    basemap_files <- ggmap_bbox(basemap_files)
    
    basemap <- ggmap(basemap_files)
  }
  
  o <- basemap + geom_sf_rast(
    geom_sf(data=map_data, 
            aes(fill=metric_mean, color=metric_mean),
            size=0.1,
            # color=NA,#"white"
            alpha=fill_transparency,
            inherit.aes = FALSE), dpi=300, dev=NULL) 
  if(include_borders==TRUE){
    o <- o + geom_sf(data = map_data %>% group_by(!!as.name(border_field)) %>% summarise(),
              fill = "transparent", 
              color = "#626462", #"gray20",
              size = 0.075,
              inherit.aes = FALSE
      )
  }

   #+
    # coord_sf(crs = 4326, #26945, #
    #          xlim = c(b['left'], b['right']),
    #          ylim = c(b['bottom'], b['top']),
    #          expand = TRUE)
  
  color_levels <- list(
    Min=weighted_metrics$metric_min,
    Lower=weighted_metrics$metric_lower,
    Threshold=metric_cutoff_level,
    Average=weighted_metrics$metric_mean,
    Median=weighted_metrics$metric_median,
    Upper=weighted_metrics$metric_upper,
    Max=weighted_metrics$metric_max
  )
  
  color_levels <- color_levels[order(unlist(color_levels),decreasing = F)] #as.numeric(sort.list(color_levels, decreasing=F))
  
  if(metric_name!="ner"){
    
    color_levels <- lapply(color_levels, function(x){return(max(min(x,weighted_metrics$metric_upper),
                                                                weighted_metrics$metric_lower))})
  }
  
  color_values <- scales::rescale(x=unlist(color_levels), to=c(0,1))
  
  breaks <- unlist(color_levels[c(
    # "Min",
    "Lower",
    "Threshold",
    "Average",
    "Median",
    "Upper"
    # "Max"
    )])
  
  
  
  # legend_colors <- c(
  # #   "#B22C2C",
  #   "#B22C2C",
  #                    "#B26F2C",
  #                    "#B2B22C",
  #                    "#2CB2B2",
  #                    "#2C6FB2"
  #   )
  legend_colors <- as.character(rev(wes_palette("Zissou1", 6, type = "continuous")))#[2:8]
  
  # legend_colors[2:3] <- legend_colors[1:2]
  # legend_colors[5:6] <- legend_colors[6:7]
  # 
  # legend_colors[7] <- "#2C2CB2"
  # legend_colors[1] <- "#B22C2C"
  
  legend_colors <- c("#B22C2C",legend_colors,"#2C2CB2")
  
  scale_limits <- c(min(unlist(color_levels)),max(unlist(color_levels)))
  
  if(flip_scale==TRUE){
    # color_levels <- rev(color_levels)
    # color_values <- rev(color_values)
    # breaks <- rev(breaks)
    legend_colors <- rev(legend_colors)
    # scale_limits <- rev(scale_limits)
  }
  
  if(keep_coordinates){
    o <- o + theme_classic()
  } else {
    o <- o + theme_map()
  }
  
  p <- o +
    # geom_polygon(data = spdf_fortified, 
    #              aes(fill = nb_equip, x = long, y = lat, group = group) , 
    #              size=0, alpha=0.9) +
    ## theme_void() + 
    # theme_classic() + 
    # ggsn::north(data=map_data, location="bottomright", symbol = 16, scale = .25) + 
    # ggsn::scalebar(data=map_data, location="bottomright") + 
    # scale_colour_identity(
    # scale_fill_identity() +
    scale_color_gradientn(
      colors = legend_colors,
      limits = scale_limits,
      values = color_values,
      na.value = "#B3B3B3",
      guide = "none",
      name=FALSE
    ) +
    scale_fill_gradientn(
      colors = legend_colors,
      limits = scale_limits,
      values = color_values,
      na.value = "#B3B3B3",
      breaks = breaks,
      labels = paste0(names(breaks),": ",to_percent(breaks)),
      guide = guide_colorbar(direction = legend_direction,
                             title.position = 'top',
                             label.theme = element_text(angle = ifelse(legend_direction == "horizontal",
                                                                       -45,
                                                                       0), 
                                                        # position=position_jitter(width=1,height=1),
                                                        hjust = 0,
                                                        vjust = 1#,
                                                        # size = 8
                                                        )),
      name = guide_name) +
    labs(
      title = chart_title,
      subtitle = chart_subtitle
    ) +
    theme(
      legend.position = legend_position,
      legend.background = element_rect(fill="#9e9e9e75"),
      # legend.background,
      legend.margin = margin(t = 3, r = 3, b = 10, l = 3, unit = "pt"),
      # legend.spacing,
      # legend.spacing.x,
      # legend.spacing.y,
      # legend.key,
      # legend.key.size = unit(1, "npc"),
      text  = element_text(size = 20),
      legend.key.height = unit((dev.size("in")[2] / 5), "inches"),
      legend.key.width = unit((dev.size("in")[1] / 40), "inches"),
      # legend.text  = element_text(size = 13),
      # legend.text = element_text(size = 5, color = "red"),
      legend.text.align = 0.0,
      # legend.title = element_text(size = 13),
      legend.title.align = 0.0,
      # legend.position,
      # legend.direction,
      legend.justification="center"
      # legend.box,
      # legend.box.just,
      # legend.box.margin,
      # legend.box.background,
      # legend.box.spacing
    ) +
    labs(
      x = NULL,
      y = NULL
    )
    # coord_sf(expand = FALSE) +
    # metR::scale_x_longitude(
    #   #xmin=-180,
    #   #xmax=180,
    #   #step=0.002
    # )+#breaks = c(33.5, 34, 34.5, 35, 35.5),
    #                    #labels = c("33.5", "34", "34.5", "35", "35.5")) +
    # metR::scale_y_latitude(
    #   #xmin=-90,
    #   #xmax=90,
    #   #step=0.002
    # ) + 
    # theme(axis.line = element_line(color="black", size = 2))
    #breaks = (c(-20, -19.5, -19, -18.5, -18,-17.5)),
                      # labels = c("20", "19.5", "19", "18.5", "18", "17.5"))
  #coord_map()
  if(include_compass){
    p <- p + ggsn::north(data=map_data, location=compass_location, symbol = 16, scale = .25)
  }
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
                            metric_cutoff_level=0,
                            metric_cutoff_label=NULL,
                            upper_quantile_view=1.0,
                            lower_quantile_view=0.0,
                            weighted_metrics_data=NULL,
                            chart_title=NULL,
                            chart_subtitle=NULL,
                            chart_caption=NULL,
                            x_label="Proportion of Households",
                            metric_long_name = NULL,#bquote(E[b]),
                            include_borders=TRUE,
                            border_field="tract_fips",
                            legend_position="right", #c(0.15, 0.125),
                            legend_direction="vertical",
                            flip_scale=TRUE,
                            include_basemap=FALSE,
                            keep_coordinates=FALSE,
                            infer_missing=FALSE
                            ){
  
  
  pie_chart <- pie_chart(clean_data=st_drop_geometry(clean_data))
  
  tree_chart <- tree_chart(clean_data=st_drop_geometry(clean_data))
  
  graph_data <- filter_graph_data(st_drop_geometry(clean_data), group_columns, metric_name)
  
  if(is.null(weighted_metrics_data)){
    weighted_metrics <- calculate_weighted_metrics(st_drop_geometry(clean_data), 
                                                   group_columns, 
                                                   metric_name, 
                                                   metric_cutoff_level, 
                                                   upper_quantile_view, 
                                                   lower_quantile_view)
  } else {
    weighted_metrics <- calculate_weighted_metrics(st_drop_geometry(weighted_metrics_data), 
                                                   group_columns, 
                                                   metric_name, 
                                                   metric_cutoff_level, 
                                                   upper_quantile_view, 
                                                   lower_quantile_view)
  }
  
  
    
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
    
    choropleth_chart <- choropleth_map(
      clean_data,
      group_columns=unique(c(group_columns,"GEOID")),
      metric_name,
      metric_label,
      metric_cutoff_level,
      metric_cutoff_label,
      upper_quantile_view,
      lower_quantile_view,
      chart_title,
      chart_subtitle,
      weighted_metrics,
      include_basemap=include_basemap,
      legend_position=legend_position,#c(0.15, 0.125),
      legend_direction=legend_direction,#"horizontal",
      include_compass=FALSE,
      compass_location="bottomright",
      metric_long_name=metric_long_name,
      include_borders=include_borders,
      border_field=border_field,
      flip_scale=flip_scale,
      keep_coordinates=keep_coordinates,
      infer_missing=infer_missing
    )
    
    dashboard <- patchwork::wrap_plots(
      choropleth_chart,
      violin_chart,
      density_chart,
      tree_chart,
      nrow=2,
      heights = c(2,1), 
      ncol=2,
      widths = c(3,1)
    )

  # }
  
  return(list("metrics"=weighted_metrics,
              "density"=density_chart,
              "choropleth"=choropleth_chart,
              "violin"=violin_chart,
              "pie"=pie_chart,
              "dashboard"=dashboard,
              "tree"=tree_chart
              ))
}


scatter_chart <- function(graph_data, 
                          metric_name, 
                          metric_label,
                          group_columns, 
                          metric_cutoff_level, 
                          metric_cutoff_label, 
                          chart_title, 
                          chart_subtitle,
                          dependent_variable
  
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
  # print(legend_title)
  
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
                      y=dependent_variable,
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
  scatter_plot <- chart + 
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
    # ggtitle("Monthly Electricity Cost vs. EROI") + 
    plot_spacer() + 
    # electricity_v_eroi_contour_plot + 
    theme(legend.justification = c(1, 1), legend.position = c(1, 1), legend.title=element_blank()) + 
    # mean_eroi + 
    coord_flip() + 
    theme_void() + 
    theme(legend.position = "none") + 
    labs(title = NULL, x=NULL, y=NULL) + 
    plot_layout(ncol = 2, nrow = 2, widths = c(4, 1), heights = c(1, 4))
}

pie_chart <- function(
    clean_data
){
  final_pie_chart <- clean_data %>% 
    pivot_longer(cols=c("electricity_spend","gas_spend","other_spend")) %>% 
    group_by(name) %>%
    summarise(value=sum(value*households, na.rm=TRUE), .groups = "keep") %>%
    ggplot(aes(x="", 
               y=value, 
               # weight=households, 
               fill=name)) + geom_bar(stat="identity", width=1) +
    coord_polar("y",start=0)
  return(final_pie_chart)
}

tree_chart <- function(
    clean_data
){
  final_spending_tree_chart <- clean_data %>% 
    pivot_longer(cols=c("electricity_spend","gas_spend","other_spend")) %>% 
    group_by(name) %>%
    summarise(value=sum(value*households)) %>% 
    mutate(
      pct=to_percent(value/sum(value))
    ) %>% 
    ggplot(aes(area = value, 
               fill = name,
               label = paste(name, pct, sep = "\n\n"),
    )) +
    geom_treemap() + 
    geom_treemap_text(colour = "white",
                      place = "centre",
                      size = 15) +
    theme(legend.position = "none")
    
    return(final_spending_tree_chart)
}