server <- function(input, output, session) {
  output$map1 <- renderLeaflet({
    leaf_reefs
    })
  
  #create empty vector to hold all click ids
  selected <- reactiveValues(
    groups = vector()
    )
  
  output$plotlyGA <- renderPlotly({
    diseaseRisk_placeholder_plot(
      "Growth anomalies", 
      ga_forecast$Date
      )
    })
  
  output$plotlyWS <- renderPlotly({
    diseaseRisk_placeholder_plot(
      "White syndromes", 
      ws_forecast$Date
      )
    })
  
  observeEvent(
    input$map1_shape_click, 
    {
      if(input$map1_shape_click$group == "5km forecasts")
        {
        
        z <- subset(
          ga_forecast, 
          ID == input$map1_shape_click$id
          )
        
        z2 <- subset(
          ws_forecast, 
          ID == input$map1_shape_click$id
          )
        
        output$plotlyGA <- renderPlotly({
        diseaseRisk_plotly(
          z, 
          "Growth anomalies"
          )
          })
        
        output$plotlyWS <- renderPlotly({
          diseaseRisk_plotly(
            z2, 
            "White syndromes"
            )
          })
        }
      
      else if(input$map1_shape_click$group == "Management area forecasts")
        {
        
        z5 <- subset(
          ga_forecast_aggregated_to_management_zones, 
          PolygonID == input$map1_shape_click$id
          )
        
        z6  <- subset(
          ws_forecast_aggregated_to_management_zones, 
          PolygonID == input$map1_shape_click$id
          )
        
        output$plotlyGA <- renderPlotly({
          diseaseRisk_plotly(
            z5, 
            "Growth anomalies"
            )
          })
        
        output$plotlyWS <- renderPlotly({
          diseaseRisk_plotly(
            z6, 
            "White syndromes"
            )
          })
        }
      
      else if(input$map1_shape_click$group == "GBRMPA park zoning forecasts")
        {
        
        z3 <- subset(
          ga_forecast_aggregated_to_gbrmpa_park_zones, 
          PolygonID == input$map1_shape_click$id
          )
        
        z4 <- subset(
          ws_forecast_aggregated_to_gbrmpa_park_zones, 
          PolygonID == input$map1_shape_click$id
          )
        
        output$plotlyGA <- renderPlotly({
          diseaseRisk_plotly(
            z3, 
            "Growth anomalies"
            )
          })
        
        output$plotlyWS <- renderPlotly({
          diseaseRisk_plotly(
            z4, 
            "White syndromes"
            )
          })
        }
      })
  
  # Investigating scenarios outputs
  output$management_map <- renderLeaflet({
    leaf_scenarios
  })
  
  output$barplot <- renderPlotly({
    scenarios_placeholder_plot
    })
  
  #create empty vector to hold all click ids, not sure if this is needed anymore
  selected2 <- reactiveValues(
    groups = vector()
    )
  
  observeEvent(
    input$management_map_shape_click,
    {
      if(input$management_map_shape_click$group == "5km forecasts")
        {
        
        # GA tab
        mitigate <- subset(
          ga_scenarios, 
          ID == input$management_map_shape_click$id
          )
        
        baseVals <- subset(
          ga_scenarios_baseline_vals, 
          ID == input$management_map_shape_click$id
          )
        
        reactive_w_ga <- reactive({
          subset(
            mitigate, 
            Response == "Water quality" & 
              Response_level == input$wq_slider_ga
            )
          })
        
        reactive_f_ga <- reactive({
          subset(
            mitigate, 
            Response == "Fish abundance" & 
              Response_level == input$fish_slider_ga
            )
          })
        
        reactive_c_ga <- reactive({
          subset(
            mitigate, 
            Response == "Coral" & 
              Response_level == input$coral_slider_ga
            )
          })
        
        output$barplot_ga <- renderPlotly({
          mitigation_plot_fun(
            reactive_w_ga(), 
            reactive_f_ga(), 
            reactive_c_ga(), 
            round(baseVals$p*100)
            )
          })
        
        # output$table_ga <- renderDataTable(iris)
        
        output$chlA_value_ga <- renderText({
          paste0(
            "chl-a: baseline = ",
            round(baseVals$chla, 2),
            " mg/m<sup>3</sup>; adjusted = ",
            round(baseVals$chla - baseVals$chla * (input$wq_slider_ga/100), 2),
            " mg/m<sup>3</sup>"
          )
        })
        
        output$kd_value_ga <- renderText({
          paste0(
            "kd(490): baseline = ",
            round(baseVals$kd490, 2),
            " m<sup>-1</sup>; adjusted = ",
            round(baseVals$kd490 - baseVals$kd490 * (input$wq_slider_ga/100), 2),
            " m<sup>-1</sup>"
          )
        })
        
        output$fish_value_ga <- renderText({
          paste0(
            "baseline = ",
            round(baseVals$fish),
            " m<sup>2</sup>; adjusted = ",
            round(baseVals$fish + baseVals$fish * (input$fish_slider_ga/100)),
            " m<sup>2</sup>"
            )
          })
        
        output$corsize_value_ga <- renderText({
          paste0(
            "Median colony size: baseline = ", 
            round(baseVals$coral_size),
            " cm; adjusted = ",
            round(baseVals$coral_size + baseVals$coral_size * (input$coral_slider_ga/100)),
            " cm"
            )
          })
        
        output$corcov_value_ga <- renderText({
          paste0(
            "Host cover: baseline = ", 
            round(baseVals$host_cover),
            " %; adjusted = ",
            round(baseVals$host_cover + baseVals$host_cover * (input$coral_slider_ga/100)),
            " %"
            )
          })
        
        # WS tab
        mitigate <- subset(
          ws_scenarios, 
          ID == input$management_map_shape_click$id
          )
        
        baseVals <- subset(
          ws_scenarios_baseline_vals, 
          ID == input$management_map_shape_click$id
          )
        
        reactive_w_ws <- reactive({
          subset(
            mitigate, 
            Response == "Water quality" & 
              Response_level == input$wq_slider_ws
            )
          })
        
        reactive_f_ws <- reactive({
          subset(
            mitigate, 
            Response == "Fish abundance" & 
              Response_level == input$fish_slider_ws
            )
          })
        
        reactive_c_ws <- reactive({
          subset(
            mitigate, 
            Response == "Coral" & 
              Response_level == input$coral_slider_ws
            )
          })
        
        output$barplot_ws <- renderPlotly({
          mitigation_plot_fun(
            reactive_w_ws(), 
            reactive_f_ws(), 
            reactive_c_ws(), 
            round(baseVals$p*100)
            )
          })
        
        output$chlA_value_ws <- renderText({
          paste0(
            "chl-a: baseline = ", 
            round(baseVals$chla, 2),
            " mg/m<sup>3</sup>; adjusted = ",
            round(baseVals$chla - baseVals$chla * (input$wq_slider_ws/100), 2),
            " mg/m<sup>3</sup>"
            )
          })
        
        output$kd_value_ws <- renderText({
          paste0(
            "kd(490): baseline = ", 
            round(baseVals$kd490, 2),
            " m<sup>-1</sup>; adjusted = ",
            round(baseVals$kd490 - baseVals$kd490 * (input$wq_slider_ws/100), 2),
            " m<sup>-1</sup>"
            )
          })
        
        output$fish_value_ws <- renderText({
          paste0(
            "baseline = ", 
            round(baseVals$fish),
            " m<sup>2</sup>; adjusted = ",
            round(baseVals$fish + baseVals$fish * (input$fish_slider_ws/100)),
            " m<sup>2</sup>"
            )
          })
        
        output$corsize_value_ws <- renderText({
          paste0(
            "Median colony size: baseline = ", 
            round(baseVals$coral_size),
            " cm; adjusted = ",
            round(baseVals$coral_size + baseVals$coral_size * (input$coral_slider_ws/100)),
            " cm"
            )
          })
      
      output$corcov_value_ws <- renderText({
        paste0(
          "Host cover: baseline = ", 
          round(baseVals$host_cover),
          " %; adjusted = ",
          round(baseVals$host_cover + baseVals$host_cover * (input$coral_slider_ws/100)),
          " %"
          )
        })
      }
      
      else if(input$management_map_shape_click$group == "Management area forecasts")
      {
        
        # GA tab
        mitigate <- subset(
          ga_scenarios_baseline_vals_aggregated_to_management_zones, 
          PolygonID == input$management_map_shape_click$id
        )
        
        baseVals <- subset(
          ga_scenarios_baseline_vals_aggregated_to_management_zones, 
          PolygonID == input$management_map_shape_click$id
        )
        
        reactive_w_ga <- reactive({
          subset(
            mitigate, 
            Response == "Water quality" & 
              Response_level == input$wq_slider_ga
          )
        })
        
        reactive_f_ga <- reactive({
          subset(
            mitigate, 
            Response == "Fish abundance" & 
              Response_level == input$fish_slider_ga
          )
        })
        
        reactive_c_ga <- reactive({
          subset(
            mitigate, 
            Response == "Coral" & 
              Response_level == input$coral_slider_ga
          )
        })
        
        output$barplot_ga <- renderPlotly({
          mitigation_plot_fun(
            reactive_w_ga(), 
            reactive_f_ga(), 
            reactive_c_ga(), 
            round(baseVals$p*100)
          )
        })
        
        output$chlA_value_ga <- renderText({
          paste0(
            "chl-a: baseline = ",
            round(baseVals$chla, 2),
            " mg/m<sup>3</sup>; adjusted = ",
            round(baseVals$chla - baseVals$chla * (input$wq_slider_ga/100), 2),
            " mg/m<sup>3</sup>"
          )
        })
        
        output$kd_value_ga <- renderText({
          paste0(
            "kd(490): baseline = ",
            round(baseVals$kd490, 2),
            " m<sup>-1</sup>; adjusted = ",
            round(baseVals$kd490 - baseVals$kd490 * (input$wq_slider_ga/100), 2),
            " m<sup>-1</sup>"
          )
        })
        
        output$fish_value_ga <- renderText({
          paste0(
            "baseline = ",
            round(baseVals$fish),
            " m<sup>2</sup>; adjusted = ",
            round(baseVals$fish + baseVals$fish * (input$fish_slider_ga/100)),
            " m<sup>2</sup>"
          )
        })
        
        output$corsize_value_ga <- renderText({
          paste0(
            "Median colony size: baseline = ", 
            round(baseVals$coral_size),
            " cm; adjusted = ",
            round(baseVals$coral_size + baseVals$coral_size * (input$coral_slider_ga/100)),
            " cm"
          )
        })
        
        output$corcov_value_ga <- renderText({
          paste0(
            "Host cover: baseline = ", 
            round(baseVals$host_cover),
            " %; adjusted = ",
            round(baseVals$host_cover + baseVals$host_cover * (input$coral_slider_ga/100)),
            " %"
          )
        })
        
        # WS tab
        mitigate <- subset(
          ws_scenarios_aggregated_to_management_zones, 
          PolygonID == input$management_map_shape_click$id
        )
        
        baseVals <- subset(
          ws_scenarios_baseline_vals_aggregated_to_management_zones, 
          PolygonID == input$management_map_shape_click$id
        )
        
        reactive_w_ws <- reactive({
          subset(
            mitigate, 
            Response == "Water quality" & 
              Response_level == input$wq_slider_ws
          )
        })
        
        reactive_f_ws <- reactive({
          subset(
            mitigate, 
            Response == "Fish abundance" & 
              Response_level == input$fish_slider_ws
          )
        })
        
        reactive_c_ws <- reactive({
          subset(
            mitigate, 
            Response == "Coral" & 
              Response_level == input$coral_slider_ws
          )
        })
        
        output$barplot_ws <- renderPlotly({
          mitigation_plot_fun(
            reactive_w_ws(), 
            reactive_f_ws(), 
            reactive_c_ws(), 
            round(baseVals$p*100)
          )
        })
        
        output$chlA_value_ws <- renderText({
          paste0(
            "chl-a: baseline = ", 
            round(baseVals$chla, 2),
            " mg/m<sup>3</sup>; adjusted = ",
            round(baseVals$chla - baseVals$chla * (input$wq_slider_ws/100), 2),
            " mg/m<sup>3</sup>"
          )
        })
        
        output$kd_value_ws <- renderText({
          paste0(
            "kd(490): baseline = ", 
            round(baseVals$kd490, 2),
            " m<sup>-1</sup>; adjusted = ",
            round(baseVals$kd490 - baseVals$kd490 * (input$wq_slider_ws/100), 2),
            " m<sup>-1</sup>"
          )
        })
        
        output$fish_value_ws <- renderText({
          paste0(
            "baseline = ", 
            round(baseVals$fish),
            " m<sup>2</sup>; adjusted = ",
            round(baseVals$fish + baseVals$fish * (input$fish_slider_ws/100)),
            " m<sup>2</sup>"
          )
        })
        
        output$corsize_value_ws <- renderText({
          paste0(
            "Median colony size: baseline = ", 
            round(baseVals$coral_size),
            " cm; adjusted = ",
            round(baseVals$coral_size + baseVals$coral_size * (input$coral_slider_ws/100)),
            " cm"
          )
        })
        
        output$corcov_value_ws <- renderText({
          paste0(
            "Host cover: baseline = ", 
            round(baseVals$host_cover),
            " %; adjusted = ",
            round(baseVals$host_cover + baseVals$host_cover * (input$coral_slider_ws/100)),
            " %"
          )
        })
      }
      
      else if(input$management_map_shape_click$group == "GBRMPA park zoning forecasts")
      {
        
        # GA tab
        mitigate <- subset(
          ga_scenarios_baseline_vals_aggregated_to_gbrmpa_park_zones, 
          PolygonID == input$management_map_shape_click$id
        )
        
        baseVals <- subset(
          ga_scenarios_baseline_vals_aggregated_to_gbrmpa_park_zones, 
          PolygonID == input$management_map_shape_click$id
        )
        
        reactive_w_ga <- reactive({
          subset(
            mitigate, 
            Response == "Water quality" & 
              Response_level == input$wq_slider_ga
          )
        })
        
        reactive_f_ga <- reactive({
          subset(
            mitigate, 
            Response == "Fish abundance" & 
              Response_level == input$fish_slider_ga
          )
        })
        
        reactive_c_ga <- reactive({
          subset(
            mitigate, 
            Response == "Coral" & 
              Response_level == input$coral_slider_ga
          )
        })
        
        output$barplot_ga <- renderPlotly({
          mitigation_plot_fun(
            reactive_w_ga(), 
            reactive_f_ga(), 
            reactive_c_ga(), 
            round(baseVals$p*100)
          )
        })
        
        output$chlA_value_ga <- renderText({
          paste0(
            "chl-a: baseline = ",
            round(baseVals$chla, 2),
            " mg/m<sup>3</sup>; adjusted = ",
            round(baseVals$chla - baseVals$chla * (input$wq_slider_ga/100), 2),
            " mg/m<sup>3</sup>"
          )
        })
        
        output$kd_value_ga <- renderText({
          paste0(
            "kd(490): baseline = ",
            round(baseVals$kd490, 2),
            " m<sup>-1</sup>; adjusted = ",
            round(baseVals$kd490 - baseVals$kd490 * (input$wq_slider_ga/100), 2),
            " m<sup>-1</sup>"
          )
        })
        
        output$fish_value_ga <- renderText({
          paste0(
            "baseline = ",
            round(baseVals$fish),
            " m<sup>2</sup>; adjusted = ",
            round(baseVals$fish + baseVals$fish * (input$fish_slider_ga/100)),
            " m<sup>2</sup>"
          )
        })
        
        output$corsize_value_ga <- renderText({
          paste0(
            "Median colony size: baseline = ", 
            round(baseVals$coral_size),
            " cm; adjusted = ",
            round(baseVals$coral_size + baseVals$coral_size * (input$coral_slider_ga/100)),
            " cm"
          )
        })
        
        output$corcov_value_ga <- renderText({
          paste0(
            "Host cover: baseline = ", 
            round(baseVals$host_cover),
            " %; adjusted = ",
            round(baseVals$host_cover + baseVals$host_cover * (input$coral_slider_ga/100)),
            " %"
          )
        })
        
        # WS tab
        mitigate <- subset(
          ws_scenarios_aggregated_to_gbrmpa_park_zones, 
          PolygonID == input$management_map_shape_click$id
        )
        
        baseVals <- subset(
          ws_scenarios_baseline_vals_aggregated_to_gbrmpa_park_zones, 
          PolygonID == input$management_map_shape_click$id
        )
        
        reactive_w_ws <- reactive({
          subset(
            mitigate, 
            Response == "Water quality" & 
              Response_level == input$wq_slider_ws
          )
        })
        
        reactive_f_ws <- reactive({
          subset(
            mitigate, 
            Response == "Fish abundance" & 
              Response_level == input$fish_slider_ws
          )
        })
        
        reactive_c_ws <- reactive({
          subset(
            mitigate, 
            Response == "Coral" & 
              Response_level == input$coral_slider_ws
          )
        })
        
        output$barplot_ws <- renderPlotly({
          mitigation_plot_fun(
            reactive_w_ws(), 
            reactive_f_ws(), 
            reactive_c_ws(), 
            round(baseVals$p*100)
          )
        })
        
        output$chlA_value_ws <- renderText({
          paste0(
            "chl-a: baseline = ", 
            round(baseVals$chla, 2),
            " mg/m<sup>3</sup>; adjusted = ",
            round(baseVals$chla - baseVals$chla * (input$wq_slider_ws/100), 2),
            " mg/m<sup>3</sup>"
          )
        })
        
        output$kd_value_ws <- renderText({
          paste0(
            "kd(490): baseline = ", 
            round(baseVals$kd490, 2),
            " m<sup>-1</sup>; adjusted = ",
            round(baseVals$kd490 - baseVals$kd490 * (input$wq_slider_ws/100), 2),
            " m<sup>-1</sup>"
          )
        })
        
        output$fish_value_ws <- renderText({
          paste0(
            "baseline = ", 
            round(baseVals$fish),
            " m<sup>2</sup>; adjusted = ",
            round(baseVals$fish + baseVals$fish * (input$fish_slider_ws/100)),
            " m<sup>2</sup>"
          )
        })
        
        output$corsize_value_ws <- renderText({
          paste0(
            "Median colony size: baseline = ", 
            round(baseVals$coral_size),
            " cm; adjusted = ",
            round(baseVals$coral_size + baseVals$coral_size * (input$coral_slider_ws/100)),
            " cm"
          )
        })
        
        output$corcov_value_ws <- renderText({
          paste0(
            "Host cover: baseline = ", 
            round(baseVals$host_cover),
            " %; adjusted = ",
            round(baseVals$host_cover + baseVals$host_cover * (input$coral_slider_ws/100)),
            " %"
          )
        })
      }
      })
  
  # map historical data
  output$historical_data_map <- renderLeaflet({
    historicalMap
    })
  }
