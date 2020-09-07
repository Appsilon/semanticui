server <- function(input, output, session) {

  nat_data <- reactive({
    validate(
      need(input$nationality, "...")
    )
    fifa_data %>% filter(Nationality == input$nationality)
  })

  output$nat_tab <- renderDataTable({
    subset <- nat_data() %>%
      select(Name, Overall, Club, Value, Jersey.Number) %>%
      arrange(desc(Overall))
    semantic_DT(subset, options = list(bInfo = F, dom = "rtp"))
  })

  output$nat_tab_position <- renderDataTable({
    subset <- nat_data() %>%
      filter(Class == input$nat_position) %>%
      select(Name, Overall, Club, Value, Jersey.Number) %>%
      arrange(desc(Overall))
    semantic_DT(subset, options = list(bInfo = F, dom = "rtp"))
  })


  output$nationality_total_value <- renderText({
    get_total_value(nat_data())
  })

  output$nationality_nr_teams <- renderText({
    get_n_clubs(nat_data())
  })

  output$nationality_nr_players <- renderText({
    get_n_players(nat_data())
  })

  output$nat_per_league <- renderUI({

  })
}
