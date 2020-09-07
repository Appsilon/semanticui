tagList(
  div(class="ui center aligned header", "FIFA'19 Analysis by Leagues"),
  segment(
    selectInput("league", "Select league:", unique(fifa_data$League))
  ),
  segment(
    div(class = "ui three column stackable grid container",
        div(class = "column",
            custom_ui_message(textOutput("league_total_value"),
                              "Total League Value",
                        color = "orange",
                        icon_name = "euro sign")
        ),
        div(class = "column",
            custom_ui_message(textOutput("league_nr_players"),
                              "Number of Players",
                        color = "olive",
                        icon_name = "running")
        ),
        div(class = "column",
            custom_ui_message(textOutput("league_nr_teams"),
                              "Number of Teams",
                        color = "brown",
                        icon_name = "futbol")
        )
    ),
    div(class = "ui two column stackable grid container",
        div(class = "column",
          h1(textOutput("map_title")),
          plotlyOutput("map_nationality")
        ),
        div(class = "column",
            tabset( tabs = list(
              list(menu = "All Players", content = semantic_DTOutput("tab")),
              list(menu = "By Position", content = div(
                  selectInput("league_position", "Select position:", unique(fifa_data$Class)),
                  semantic_DTOutput("tab_position")
                )
              )
            ))
        )
    )
  )
)
