server <- function(input, output, session) {
  choices <-fifa_data$Name
  output$search_players <- renderUI(
    search_selection_choices("search_result", choices, value = "L. Messi", multiple = F)
  )

  output$search_comparison_player <- renderUI(
    search_selection_choices("search_result_comparison", choices)
  )

  output$selected_player <- renderUI({
    validate(
      need(nchar(input[["search_result"]]) > 0, "Waiting...")
    )
    render_player_card(filter_player(input[["search_result"]]))
  })

  output$player_radar <- renderPlotly({
    validate(
      need(nchar(input[["search_result"]]) > 0, "Waiting...")
    )
    player_list <- filter_player(input[["search_result"]])
    plot_ly(
      type = 'scatterpolar',
      fill = 'toself'
    ) %>%
      add_trace(
        r = c(player_list$Speed, player_list$Power, player_list$Technic, player_list$Attack, player_list$Defence),
        theta = c('Speed', 'Power', 'Technic', 'Attack', 'Defence'),
        name = player_list$Name.Pos
      ) %>%
      layout(
        polar = list(
          radialaxis = list(
            visible = T,
            range = c(0,100)
          )))
  })

  output$players_comparison_barplot <- renderPlot({
    validate(
      need(nchar(input[["search_result_comparison"]]) > 0, "Select player for comparison")
    )
    player1_list <- filter_player(input[["search_result"]])
    player2_list <- filter_player(input[["search_result_comparison"]])
    bar_plot_compare_two(player1_list, player2_list)
  })
}
