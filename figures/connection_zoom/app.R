library(shiny)
library(bslib)
library(tidyverse)
library(plotly)
library(usmap)
library(sf)
source('var_names.R')
source('load_gages.R')


# load data ---------------------------------------------------------------
states <- usmap::us_map(exclude = c('PR', 'AK', 'HI')) %>%
  st_transform(5070)

gage_loc <- all_gage_info %>%
  st_as_sf(coords = c('lon', 'lat'), crs = 4269) %>%
  st_transform(5070) %>%
  mutate(x = st_coordinates(geometry)[,1],
         y = st_coordinates(geometry)[,2])
trend_dat <- read_csv('data/metrics_trends.csv') %>%
  group_by(var) %>%
  mutate(sen_z = scale(sen, center = T)[,1]) %>%
  left_join(all_gage_info) %>%
  st_as_sf(coords = c('lon', 'lat'), crs = 4269) %>%
  st_transform(5070) %>%
  mutate(x = st_coordinates(geometry)[,1],
         y = st_coordinates(geometry)[,2])
cn_dat <- read_csv('data/cn_diffs.csv') %>%
  mutate(type = 'headwater') %>%
  st_as_sf(coords = c('lon', 'lat'), crs = 4269) %>%
  st_transform(5070) %>%
  mutate(x = st_coordinates(geometry)[,1],
         y = st_coordinates(geometry)[,2])
metric_dat <- read_csv('data/metrics_window3.csv', col_select = !contains('error_str')) %>%
  pivot_longer(!c(site_no, wateryear), names_to = 'var', values_to = 'val')

color_pal <- palette.colors(9, 'Set 1')

# ui ----------------------------------------------------------------------
ui <- page_fillable(
  navset_tab(id = 'tab',
             #gage selection tab
             nav_panel('Gage select',
                       layout_sidebar(
                         
                         #left sidebar
                         sidebar = sidebar(
                           position = 'left',
                           width = 300,
                           
                           #metric select
                           selectInput(
                             'metric',
                             label = 'Metric:',
                             choices = c('Select metric...' = '', metric_labels)
                           ),
                           #include outliers checkbox
                           checkboxInput(
                             'outliers',
                             label = span('Include outliers?', style = 'font-size:14px;'),
                             value = F
                           ),
                           #data source select
                           radioButtons(
                             'dat_source',
                             label = 'Select data:',
                             choices = c('Trends' = 'trends', 'Connections' = 'connections')
                           ),
                           #gage set select (in accordion)
                           accordion(
                             id = 'gages_sel_accordion',
                             open = T,
                             accordion_panel(
                               title = 'Select gages:',
                               checkboxGroupInput(
                                 'gages_sel',
                                 label = NULL,
                                 choiceNames = list(
                                   span('Headwaters'),
                                   span('Downstream (connections)'),
                                   span('Downstream (matches)')
                                 ),
                                 choiceValues = list('headwater', 'downstream_connected', 'downstream_matched'),
                                 selected = 'headwater'
                               )
                             )
                           )
                         ), #end of sidebar
                         
                         #Main area
                         #map plot
                         plotlyOutput('map',
                                      height = '50vh'),
                         
                         #ts plot
                         plotlyOutput('ts',
                                      height = '25vh'),
                       ) #end of tab layout
                      ), #end of selection tab
             nav_panel('Selection details',
                       layout_sidebar(
                         sidebar = sidebar(
                           position = 'left',
                           width = 300,
                           
                           textOutput('selected_gages'),
                           actionButton('update_details',
                                        label = 'Update')
                         ),
                         plotlyOutput('hydrograph')
                       )
                      )
  ) #end of all tabs
)


# server ------------------------------------------------------------------
server <- function(input, output) {
  
  #collapse gages sel if in connections mode
  observe({
    if(input$dat_source == 'trends') accordion_panel_open('gages_sel_accordion', T)
    if(input$dat_source == 'connections') accordion_panel_close('gages_sel_accordion', T)
  })
  
  
  # map plot ----------------------------------------------------------------
  
  #change map title when metric is changed
  map_title <- reactiveVal('Select a metric...')
  observeEvent(input$metric, ignoreInit = T, {
    map_title(names(metric_labels)[metric_labels == input$metric])
  })
  
  #base map that gets plotted at startup
  output$map <- renderPlotly({
    plot_ly(source = 'map') %>%
      add_sf(data = states, 
             type = 'scatter',
             color = I('grey90'),
             hoverinfo = 'skip') %>%
      add_sf(data = subset(trend_dat, var == 'Q_mean' & type == 'headwater'),
             type = 'scatter',
             color = I('black'),
             hoverinfo = 'skip') %>%
      layout(showlegend = F,
             title = list(text = isolate(map_title()),
                          yanchor = 'bottom'),
             margin = list(l = 0,
                           r = 0,
                           b = 0,
                           pad = 0),
             dragmode = 'pan') %>%
      config(displayModeBar = F,
             scrollZoom = T,
             doubleClick = F)
  })
  
  #update map when data selections change
  observe({
    #prep data
    if(input$dat_source == 'trends'){
      plot_dat <- trend_dat %>%
        filter(var == input$metric,
               type %in% input$gages_sel) %>%
        rename(val = sen,
               val_z = sen_z) %>%
        mutate(point_id = site_no) %>%
        mutate(hover_info = paste('ID:',site_no,
                                  '<br>Sen:',round(val, 3))) %>%
        filter(!is.na(val_z))
      
      bar_title <- 'Sen\'s Slope'
    }
    if(input$dat_source == 'connections'){
      plot_dat <- cn_dat %>%
        filter(var == input$metric) %>%
        rename(val = diff,
               val_z = diff_z) %>%
        mutate(point_id = connection_id) %>%
        mutate(hover_info = paste('ID:',connection_id,
                                  '<br>HW Sen:',round(hw_val, 3),
                                  '<br>DS Sen:',round(ds_val, 3),
                                  '<br>Diff:',round(val, 3))) %>%
        filter(!is.na(val_z))
      
      bar_title <- '|HW| - |DS| Slope'
    }
    if(!input$outliers) plot_dat <- filter(plot_dat, abs(val_z) <= 3)
    
    gage_symbols <- data.frame(type = plot_dat$type) %>%
      mutate(symbol_val = case_when(
        type == 'headwater' ~ 0,
        type == 'downstream_connected' ~ 1,
        type == 'downstream_matched' ~ 2
      ))
    
    #update map with new trace
    plotlyProxy('map') %>%
      plotlyProxyInvoke('addTraces',
                        list(x = plot_dat$x,
                             y = plot_dat$y,
                             type = 'scatter',
                             mode = 'markers',
                             marker = list(color = plot_dat$val,
                                           colorscale = 'RdBu',
                                           reversescale = T,
                                           cmid = 0,
                                           colorbar = list(title = bar_title,
                                                           xanchor = 'right',
                                                           x=1,
                                                           xpad = 0),
                                           symbol = gage_symbols$symbol_val
                             ),
                             hoverinfo = 'text',
                             text = plot_dat$hover_info,
                             customdata = plot_dat$point_id)
      ) %>%
      plotlyProxyInvoke('relayout',
                        list(title = list(text = map_title(),
                                          yanchor = 'bottom'))) %>%
      #delete the underyling trace
      plotlyProxyInvoke('deleteTraces',1) %>%
      plotlyProxyInvoke('moveTraces',-1,1)
  })
  
  
  # timeseries plot ---------------------------------------------------------
  #base timeseries plot when app is launched
  output$ts <- renderPlotly({
    
    plot_ly(source = 'ts') %>%
      layout(title = list(text = 'Select a point...',
                          yanchor = 'auto'),
             xaxis = list(title = list(text = 'Water Year'),
                          range = c(1981,2022)),
             yaxis = list(title = list(text = 'Select a metric...'),
                          range = c(0,1)),
             showlegend = T)
  })
  
  
  #get selected points
  point_sel_prev <- reactiveVal()
  point_sel <- reactiveVal()
  #update list of selected points on click
  observeEvent(event_data('plotly_click', source = 'map', priority = 'event'), {
    point_sel_prev(point_sel())
    point_sel_i <- event_data('plotly_click', source = 'map')$customdata
    point_sel_update <- c(point_sel(), point_sel_i)
    if(sum(duplicated(point_sel_update)) > 0){
      point_sel_update <- point_sel_update[point_sel_update != point_sel_i]
    }
    point_sel(point_sel_update)
    if(length(point_sel()) == 0) point_sel(NULL)
  })
  #clear selection on double click
  observeEvent(event_data('plotly_doubleclick', source = 'map', priority = 'event'), {
    point_sel_prev(point_sel())
    point_sel(NULL)
  })
  #clear selection when data source switches, and flag the switch so the trace
  #removal function knows which ones to remove
  dat_switch <- reactiveVal(F)
  observeEvent(input$dat_source, ignoreInit = T, {
    #only trigger if something is selected
    if(length(point_sel()) > 0){
      dat_switch(T)
      point_sel_prev(point_sel())
      point_sel(NULL)
    }
  })
  
  #update timeseries with new data selection
  observeEvent(point_sel(), ignoreNULL = F, {
    p <- plotlyProxy('ts')
    m <- plotlyProxy('map')
    #get data
    points_sel_prev <- point_sel_prev()
    points_sel <- point_sel()
    
    #if adding a point, add a trace
    if(length(points_sel) > length(points_sel_prev)){
      #in trends mode, add each timeseries individually
      if(input$dat_source == 'trends'){
        new_dat <- filter(metric_dat, var == input$metric & site_no == points_sel[length(points_sel)])
        new_gage_point <- new_dat$site_no
        plotlyProxyInvoke(p, 'addTraces',
                          list(x = new_dat$wateryear,
                               y = new_dat$val,
                               type = 'scatter',
                               mode = 'lines+markers',
                               marker = list(color = color_pal[length(points_sel)]),
                               line = list(color = color_pal[length(points_sel)]),
                               name = points_sel[length(points_sel)]))
      }
      #in connections mode, add the headwater and downstream timeseries together
      if(input$dat_source == 'connections'){
        cn_sel <- filter(connections, connection_id == points_sel[length(points_sel)])
        hw_dat <- filter(metric_dat, var == input$metric & site_no == cn_sel$headwater_id)
        ds_dat <- filter(metric_dat, var == input$metric & site_no == cn_sel$downstream_id)
        new_gage_point <- hw_dat$site_no
        plotlyProxyInvoke(p, 'addTraces',
                          list(x = hw_dat$wateryear,
                               y = hw_dat$val,
                               type = 'scatter',
                               mode = 'lines+markers',
                               marker = list(color = color_pal[length(points_sel)]),
                               line = list(color = color_pal[length(points_sel)]),
                               name = paste(points_sel[length(points_sel)], 'hw')))
        plotlyProxyInvoke(p, 'addTraces',
                          list(x = ds_dat$wateryear,
                               y = ds_dat$val,
                               type = 'scatter',
                               mode = 'lines+markers',
                               marker = list(color = color_pal[length(points_sel)],
                                             symbol = 1),
                               line = list(color = color_pal[length(points_sel)],
                                           dash = 'dot'),
                               name = paste(points_sel[length(points_sel)], 'ds')))
      }
      #highlight the selected gage on the map
      loc <- filter(gage_loc, site_no == new_gage_point) %>%
        mutate(symbol_val = case_when(
          type == 'headwater' ~ 100,
          type == 'downstream_connected' ~ 101,
          type == 'downstream_matched' ~ 102
        ))
      
      plotlyProxyInvoke(m, 'addTraces',
                        list(x = list(loc$x),
                             y = list(loc$y),
                             type = 'scatter',
                             mode = 'markers',
                             marker = list(color = color_pal[length(points_sel)],
                                           symbol = loc$symbol_val,
                                           size = 8),
                             hoverinfo = 'skip'))
    }
    
    #if removing a point, remove the trace
    if(length(points_sel) < length(points_sel_prev)){
      if(input$dat_source == 'trends') traces <- points_sel_prev
      #traces get added in pairs when selecting connections so need to remove both of them
      if(input$dat_source == 'connections') traces <- rep(points_sel_prev, each = 2)
      #need to swap these rules for when the data source was just switched
      if(dat_switch() == T){
        if(input$dat_source == 'connections') traces <- points_sel_prev
        if(input$dat_source == 'trends') traces <- rep(points_sel_prev, each = 2)
        dat_switch(F)
      }
      #delete the deselected traces
      trace_del <- which(!traces %in% points_sel)
      plotlyProxyInvoke(p, 'deleteTraces', trace_del)
    }
    
    #update chart title and y-axis name
    plotlyProxyInvoke(p, 'relayout',
                      list(title = list(text = paste(points_sel, collapse = ', ')),
                           yaxis = list(title = list(text = names(metric_labels)[metric_labels == input$metric]),
                                        autorange = T)))
    
    #remove the point highlights from the map
    map_trace_del <- which(!points_sel_prev %in% points_sel) + 1
    plotlyProxyInvoke(m, 'deleteTraces', map_trace_del)
  })
  
  #update timeseries when a new metric is chosen
  observeEvent(input$metric, {
    p <- plotlyProxy('ts')
    points_sel <- point_sel()
    #only trigger update if something is actually selected
    if(length(points_sel) > 0){
      #Grab the data for the new metric for all the selected points.
      #Place them in a list in order by their plotly trace to make the
      #update straightforward.
      if(input$dat_source == 'trends'){
        update_dat <- filter(metric_dat, var == input$metric & site_no %in% points_sel) %>%
          mutate(site_no = factor(site_no, levels = points_sel)) %>%
          group_by(site_no) %>%
          group_split(.)
      }
      if(input$dat_source == 'connections'){
        cns_sel <- filter(connections, connection_id %in% points_sel) %>%
          mutate(connection_id = factor(connection_id, levels = points_sel)) %>%
          arrange(connection_id) %>%
          pivot_longer(c(headwater_id, downstream_id), names_to = 'type', values_to = 'site_no')
        sites_sel <- factor(cns_sel$site_no, levels = cns_sel$site_no)
        update_dat <- filter(metric_dat, var == input$metric & site_no %in% sites_sel) %>%
          mutate(site_no = factor(site_no, levels = sites_sel)) %>%
          group_by(site_no) %>%
          group_split(.)
        update_dat <- filter(metric_dat, var == 'Q_mean' & site_no %in% sites_sel) %>%
          mutate(site_no = factor(site_no, levels = sites_sel)) %>%
          group_by(site_no) %>%
          group_split(.)
      }
      #update the data in each trace
      for(t in seq_along(update_dat)){
        trace_dat <- update_dat[[t]]
        plotlyProxyInvoke(p, 'restyle',
                          list(x = list(trace_dat$wateryear),
                               y = list(trace_dat$val)),
                          t)
      }
      #update the y axis
      plotlyProxyInvoke(p, 'relayout',
                        list(yaxis = list(title = list(text = names(metric_labels)[metric_labels == input$metric]),
                                          autorange = T)))
    }
  })
  
  # right sidebar stuff -----------------------------------------------------
  #open right sidebar when something is clicked
  # observe({
  #   toggle_sidebar(id = 'right_bar', open = !is.null(event_data('plotly_click', source = 'map')$customdata))
  # })
  
  #display right sidebar info
  output$text <- renderPrint({
    # # d <- event_data('plotly_click', source = 'map')$customdata
    old_points <- paste(point_sel_prev(), collapse = ',')
    new_points <- paste(point_sel(), collapse = ',')
    
    
    paste(map_title)
  })
  
}

shinyApp(ui = ui, server = server)


# point_dat <- filter(metric_dat, var == 'Q_mean' & site_no %in% c('01054200','01055000'))
# 
# p <- plot_ly() %>%
#   add_trace(data = point_dat,
#             type = 'scatter',
#             mode = 'lines+markers',
#             x = ~wateryear,
#             y = ~val,
#             name = ~site_no)
# pp <- plotly_build(p)
