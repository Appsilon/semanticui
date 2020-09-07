shiny::tagList(
  sidebar_layout(
    sidebar_panel(
      h2("Select some player"),
      uiOutput("search_players"),
    ),
    main_panel(
      div(class="ui center aligned header", "Player Analysis"),
      div(class = "ui two column stackable grid container",
        div(class = "column",
            uiOutput("selected_player")
        ),
        div(class = "column",
            segment(
              div(class="ui black ribbon label", "Skills"),
              plotlyOutput("player_radar")
            )
        )
      )
    )
  ),
  br(),
  sidebar_layout(
    sidebar_panel(
      h2("Select player for comparison"),
      uiOutput("search_comparison_player")
    ),
    main_panel(
      plotOutput("players_comparison_barplot")
    )
  ), br()
)
