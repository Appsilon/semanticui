tagList(
  div(class="ui center aligned header", "FIFA'19 Analysis by Nationality"),
  segment(
    selectInput("nationality", "Select country:", unique(fifa_data$Nationality))
  ),
  segment(
    div(class = "ui three column stackable grid container",
        div(class = "column",
            custom_ui_message(textOutput("nationality_total_value"),
                              "Total Country Players Value",
                              color = "orange",
                              icon_name = "euro sign")
        ),
        div(class = "column",
            custom_ui_message(textOutput("nationality_nr_players"),
                              "Number of Players",
                              color = "olive",
                              icon_name = "running")
        ),
        div(class = "column",
            custom_ui_message(textOutput("nationality_nr_teams"),
                              "Number of Teams",
                              color = "brown",
                              icon_name = "futbol")
        )
    ),
    div(class = "ui two column stackable grid container",
        div(class = "column",
          h1("Players per League"),
          uiOutput("nat_per_league")
        ),
        div(class = "column",
            tabset( tabs = list(
              list(menu = "All Players", content = semantic_DTOutput("nat_tab")),
              list(menu = "By Position", content = div(
                selectInput("nat_position", "Select position:", unique(fifa_data$Class)),
                semantic_DTOutput("nat_tab_position")
              )
              ))
            )
        )
    )
  )
)
