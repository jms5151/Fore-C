# create plots for Fore-C shiny app -----------------------------------------------

# load libraries (and install if not already installed) 
if (!require("plotly")) install.packages("plotly"); library(plotly)

# function to show and hide confidence intervals ----------------------------------
button_info = list(
  list(
    active = -1, # not dropdown menu
    x = 0.2, # x location, if excluded, plots outside graph
    y = 1.0, # y location
    type = 'buttons',
    buttons = list(
      list(method = "restyle",
           args = list("visible", rep(list(TRUE), 29*2)), # first 29 = lines, second 29 = ribbons
           label = "Show CI"
           ),
      list(method = "restyle",
           args = list("visible", c(rep(list(TRUE), 29), rep(list(FALSE), 29))),
           label = "Hide CI"
           )
      )
    )
  )

# standard margins ----------------------------------------------------------------
plotly_margins <- list(
  l = 50,
  r = 20,
  b = 20,
  t = 60
)

# function to plot line graph of near-term forecasts -------------------------------
diseaseRisk_plotly <- function(df, titleName){
  plot_ly() %>%
    add_trace(data = df, 
              x = ~Date, 
              y = ~round(value*100), 
              split = ~cast, 
              type = 'scatter', 
              mode = 'lines',
              color = ~type,
              colors = c("Salmon", "Midnight blue"),
              text = ~paste("Date:", Date,
                            "<br>Disease risk:",
                            "<br>90th percentile", round(Upr*100), "%", # IQR90
                            "<br>75th percentile", round(value*100), "%", #IQR75
                            "<br>50th percentile", round(Lwr*100), "%" #IQR50
                            ),
              hoverinfo = "text") %>%
    add_ribbons(data = df, 
                x= ~Date, 
                split = ~cast, 
                ymin = ~round(Lwr*100), 
                ymax = ~round(Upr*100),
                color = ~type,
                colors = c("Salmon", "Midnight blue"),
                opacity=0.3,
                hoverinfo='skip') %>%
    layout(title = titleName,
           xaxis = list(showgrid = F, 
                        title = ""), 
           yaxis = list(showline = T, 
                        showgrid = F, 
                        range = c(0, 100),
                        title = "Disease risk"),
           font = list(size = 14),
           showlegend = FALSE,
           updatemenus = button_info, 
           margin = plotly_margins)      
}

# empty plot to display line graph before any pixel is selected -------------------
diseaseRisk_placeholder_plot <- function(titleName, dateRange) {
  plot_ly() %>%
    add_trace(x = ~range(dateRange), 
              y = 0, 
              type = 'scatter', 
              mode = 'lines') %>%
    layout(title = titleName,
           xaxis = list(showgrid = F, 
                        title = ""), 
           yaxis = list(showline = T, 
                        showgrid = F, 
                        range = c(0, 100),
                        title = "Disease risk"),
           font = list(size = 14),
           showlegend = FALSE,
           margin = plotly_margins)
}

# function to display barplots for change in disease risk with new conditions -----
mitigation_plot_fun <- function(w, f, c, p){
  plot_ly(
    data = w,
    x = ~Response,
    y = ~round(estimate*100),
    error_y = list(value = ~round(sd*100)),
    type = "bar",
    color = I("deepskyblue4")
    ) %>%
    add_trace(data = f, 
              y = ~round(estimate*100), 
              color = I("darkred")
              ) %>%
    add_trace(data = c, 
              y = ~round(estimate*100), 
              color = I("black")
              ) %>%
    add_trace(x = "Combined", 
              y = ~round(w$estimate*100 + f$estimate*100 + c$estimate*100), 
              error_y = list(value = ~round(w$sd*100 + f$sd*100 + c$sd*100)), 
              color = I("goldenrod1")) %>%
    layout(
      xaxis = list(showgrid = F, 
                   title = "",
                   categoryorder = "array",
                   categoryarray = ~c("Water quality",
                                      "Fish abundance",
                                      "Coral",
                                      "Combined"
                                      )
                   ), 
      yaxis = list(showline = T, 
                   showgrid = F, 
                   range = c(-100, 100),
                   title = paste0("Change in disease risk<br>(baseline = ", p, "%)")
                   ),
      font = list(size = 14),
      showlegend = FALSE) 
}

# empty barplot to display before any pixel or conditions is selected -------------
scenarios_placeholder_plot <- plot_ly(
  x = "Water quality",
  y = 0, 
  type = "bar", 
  color = I("deepskyblue4")
  ) %>%
  add_trace(x = "Fish abundance", 
            y = 0, 
            color = I("darkred")
            ) %>%
  add_trace(x = "Coral", 
            y = 0, 
            color = I("black")
            ) %>%
  add_trace(x = "Combined", 
            y = 0, 
            color = I("goldenrod1")
            ) %>%
  layout(
    xaxis = list(showgrid = F, 
                 title = "",
                 categoryorder = "array",
                 categoryarray = ~c("Water quality",
                                    "Fish abundance",
                                    "Coral",
                                    "Combined"
                                    )
                 ), 
    yaxis = list(showline = T, 
                 showgrid = F, 
                 range = c(-100, 100),
                 title = "Change in disease risk<br>(from current conditions)"
                 ),
    font = list(size = 14),
    showlegend = FALSE) 
