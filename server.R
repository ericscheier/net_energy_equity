#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {

    output$distPlot <- renderPlot({
      gwm <- calculate_weighted_metrics(filter_graph_data(clean_data,
                                                          c("geoid"),
                                                          metric_name),
                                        c("geoid"),
                                        metric_name,
                                        metric_cutoff_level=0,
                                        upper_quantile_view = 1,
                                        lower_quantile_view=0) %>% 
        mutate(geoid=as.character(geoid))
      
      gwm$metric_name <- metric_name
      
      selected_map_data <- inner_join(census_tracts_shp, gwm, by=c("GEOID"="geoid"))
      
      clean_top_metrics <- grouped_weighted_metrics(filter_graph_data(clean_data_ami_all, 
                                                                      group_columns=NULL,
                                                                      metric_name),# %>% 
                                                    #filter(!(state_abbr %in% c("HI","AK", NA))), 
                                                    group_columns=NULL, 
                                                    metric_name, 
                                                    metric_cutoff_level, 
                                                    upper_quantile_view=.995, 
                                                    lower_quantile_view=0.005)
      
      choropleth_chart <- choropleth_map(
        clean_data=selected_map_data,
        group_columns,
        metric_name,
        metric_label="%",
        metric_cutoff_level,
        metric_cutoff_label,
        upper_quantile_view,
        lower_quantile_view,
        chart_title=paste0("Energy Burdens in Selected Area"),
        chart_subtitle=NULL,
        weighted_metrics=clean_top_metrics,
        include_basemap = FALSE,
        include_compass = TRUE,
        metric_long_name = bquote(E[b]^n),
        include_borders=FALSE
      )
      
      final_map <- choropleth_chart
      print(final_map)
    })

})
