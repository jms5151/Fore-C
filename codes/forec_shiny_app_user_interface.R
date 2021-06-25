ui <- navbarPage(
  theme = shinytheme(
    "flatly"
    ), 
  collapsible = TRUE, 
  "", 
  id="nav",
  # Nowcasts and forecasts page
  tabPanel(
    "Coral disease predictions",
    leafletOutput(
      "map1"
      ) %>%
      withSpinner(
        color = spinColor
        ),
    hr(),
    fluidRow(
      column(
        6,
        plotlyOutput(
          "plotlyGA"
          ) %>%
          withSpinner(
            color = spinColor
            )
        ),
      column(
        6,
        plotlyOutput(
          "plotlyWS"
          ) %>% 
          withSpinner(
            color = spinColor
            )
        )
      ),
    absolutePanel(
      id = "controls",
      class = "panel panel-default",
      top = 450,
      left = 30,
      fixed = TRUE,
      draggable = FALSE,
      height = "auto",
      class = "dropdown",
      dropMenu(
        dropdownButton(
          icon = icon(
            'info'
            ),
          size = "xs"
          ),
        h3(
          strong(
            'Information'
            )
          ),
        h5(
          landing_page_info_txt
          )
        )
      )
    ),
  # Management scenarios page
  tabPanel(
    "Investigating scenarios", #HTML("Long-term mitigation<br/>potential"),
    leafletOutput(
      "management_map", 
      height = "300px"
      ) %>% 
      withSpinner(
        color = spinColor
        ),
    # set all slider colors at once, colors = deepskyblue4, dark red, black repeated\
    setSliderColor(
      c("#00688B", "#8B0000", "#000000", "#00688B", "#8B0000", "#000000"),
      c(1, 2, 3, 4, 5, 6)
      ),
    hr(),
    tabsetPanel(
      type = "tabs",
      tabPanel(
        "Growth anomalies",
        fluidRow(
          column(
            4,
            wellPanel(
              class = "dropdown",
              dropMenu(
                dropdownButton(
                  "Info2",
                  icon = icon(
                    'info'
                    ),
                  size = "xs"
                  ),
                h3(
                  strong(
                    'Information'
                    )
                  ),
                h5(
                  scenarios_page_info_txt
                  )
                ),
              span(
                h5(
                  strong(
                    "Targets:"
                    )
                  ),
                ),
              sliderInput(
                "wq_slider_ga",
                label = span(
                  h5(
                    strong(
                      "Water quality"
                      )
                    ),
                  tags$i(
                    h6(
                      htmlOutput(
                        "chlA_value_ga"
                        )
                      )
                    ),
                  tags$i(
                    h6(
                      htmlOutput(
                        "kd_value_ga"
                        )
                      )
                    ),
                  style = "color:#00688B",
                  div(
                    style = 'width:250px;',
                    div(
                      h6(
                        style ='float:left;', 
                        'Worse'
                        )
                      ),
                    div(
                      h6(
                        style = 'float:right;', 
                        'Better'
                        )
                      )
                    )
                  ),
                min = -100,
                max = 100,
                step = 20,
                post = " %",
                value = 0,
                width = "250px"
                ),
              bsTooltip(
                "wq_slider_ga",
                wq_hover_txt,
                placement = "bottom",
                trigger = "hover",
                options = NULL
                ),
              sliderInput(
                "fish_slider_ga",
                label = span(
                  h5(
                    strong(
                      "Herbivorous fish"
                      )
                    ),
                  tags$i(
                    h6(
                      htmlOutput(
                        "fish_value_ga"
                        )
                      )
                    ),
                  style = "color:#8B0000",
                  div(
                    style = 'width:250px;',
                    div(
                      h6(
                        style = 'float:left;', 
                        'Less'
                        )
                      ),
                    div(
                      h6(
                        style = 'float:right;', 
                        'More'
                      )
                    )
                  )
                ),
                min = -100,
                max = 100,
                step = 20,
                post = " %",
                value = 0,
                width = "250px"
              ),
              bsTooltip(
                "fish_slider_ga",
                fish_hover_txt,
                placement = "bottom",
                trigger = "hover",
                options = NULL
              ),
              sliderInput(
                "coral_slider_ga",
                label = span(
                  h5(
                    strong(
                      "Coral"
                    )
                  ),
                  tags$i(
                    h6(
                      textOutput(
                        "corsize_value_ga"
                      )
                    )
                  ),
                  tags$i(
                    h6(
                      textOutput(
                        "corcov_value_ga"
                      )
                    )
                  ),
                  style = "color:#000000",
                  div(
                    style = 'width:250px;',
                    div(
                      h6(
                        style = 'float:left;', 
                        'Less'
                      )
                    ),
                    div(
                      h6(
                        style = 'float:right;', 
                        'More'
                      )
                    )
                  )
                ),
                min = -100, 
                max = 100, 
                step = 20, 
                post = " %", 
                value = 0, 
                width = "250px"
              ),
              bsTooltip(
                "coral_slider_ga", 
                coral_hover_txt, 
                placement = "bottom", 
                trigger = "hover", 
                options = NULL
              ),
              style = "background: white"
            )
          ),
          column(
            8, 
            wellPanel(
              plotlyOutput(
                "barplot_ga"
              ),
              style = "background: white",
              # tableOutput(
              #   "table_ga"
              # )
            )
          )
        )
      ),
      tabPanel(
        "White syndromes",
        column(
          4, 
          wellPanel(
            span(
              h5(
                strong(
                  "Targets:"
                )
              ),
            ),
            sliderInput(
              "wq_slider_ws",
              label = span(
                h5(
                  strong(
                    "Water quality"
                  )
                ),
                tags$i(
                  h6(
                    htmlOutput(
                      "chlA_value_ws"
                    )
                  )
                ),
                tags$i(
                  h6(
                    htmlOutput(
                      "kd_value_ws"
                    )
                  )
                ),
                style = "color:#00688B",
                div(style = 'width:250px;',
                    div(
                      h6(
                        style = 'float:left;', 
                        'Worse'
                      )
                    ),
                    div(
                      h6(
                        style = 'float:right;', 
                        'Better'
                      )
                    )
                )
              ),
              min = -100, 
              max = 100, 
              step = 20, 
              post = " %", 
              value = 0, 
              width = "250px"
            ),
            bsTooltip(
              "wq_slider_ga", 
              wq_hover_txt, 
              placement = "bottom", 
              trigger = "hover", 
              options = NULL
            ),
            sliderInput(
              "fish_slider_ws",
              label = span(
                h5(
                  strong(
                    "Herbivorous fish"
                  )
                ),
                tags$i(
                  h6(
                    htmlOutput(
                      "fish_value_ws"
                    )
                  )
                ),
                style = "color:#8B0000",
                div(
                  style = 'width:250px;',
                  div(
                    h6(
                      style = 'float:left;', 
                      'Less'
                    )
                  ),
                  div(
                    h6(
                      style = 'float:right;', 
                      'More'
                    )
                  )
                )
              ),
              min = -100, 
              max = 100, 
              step = 20, 
              post = " %", 
              value = 0, 
              width = "250px"
            ),
            bsTooltip(
              "fish_slider_ga", 
              fish_hover_txt, 
              placement = "bottom", 
              trigger = "hover", 
              options = NULL
            ),
            sliderInput(
              "coral_slider_ws",
              label = span(
                h5(
                  strong(
                    "Coral"
                  )
                ),
                tags$i(
                  h6(
                    textOutput(
                      "corsize_value_ws"
                    )
                  )
                ),
                tags$i(
                  h6(
                    textOutput(
                      "corcov_value_ws"
                    )
                  )
                ),
                style = "color:#000000",
                div(
                  style = 'width:250px;',
                  div(
                    h6(
                      style = 'float:left;', 
                      'Less'
                    )
                  ),
                  div(
                    h6(
                      style = 'float:right;', 
                      'More'
                    )
                  )
                )
              ),
              min = -100, 
              max = 100, 
              step = 20, 
              post = " %", 
              value = 0, 
              width = "250px"
            ),
            bsTooltip(
              "coral_slider_ga", 
              coral_hover_txt, 
              placement = "bottom", 
              trigger = "hover", 
              options = NULL
            ),
            style = "background: white"
          )
        ),
        column(
          8, 
          wellPanel(
            plotlyOutput(
              "barplot_ws"
            ),
            style = "background: white"
          )
        )
      )
    )
  ),
  # Historical data page
  tabPanel(
    "Historical data",
    div(
      class = "outer",
      tags$head(
        includeCSS(
          "styles.css"
        )
      ),
      leafletOutput(
        "historical_data_map", 
        width="100%", 
        height="100%"
      )
    ),
    absolutePanel(
      id = "controls", 
      class = "panel panel-default",
      top = 80, 
      left = 60, 
      fixed = TRUE,
      draggable = FALSE, 
      height = "auto",
      class = "dropdown",
      dropMenu(
        dropdownButton(
          icon = icon(
            'info'
          ), 
          size = "xs"
        ),
        h3(
          strong(
            'Information'
          )
        ),
        h5(
          historical_data_txt1
        ),
        h5(
          historical_data_txt2
        ),
        h5(
          historical_data_txt3
        )
      )
    )
  ),
  # About the project page
  tabPanel(
    "About"
  )
)
