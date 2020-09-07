server <- function(input, output, session) {

  league_data <- reactive({
    validate(
      need(input$league, "...")
    )
    fifa_data %>% filter(League == input$league)
  })

  output$tab <- renderDataTable({
    subset <- league_data() %>%
      select(Name, Overall, Club, Value, Jersey.Number) %>%
      arrange(desc(Overall))
    semantic_DT(subset, options = list(bInfo = F, dom = "rtp"))
  })

  output$tab_position <- renderDataTable({
    subset <- league_data() %>%
      filter(Class == input$league_position) %>%
      select(Name, Overall, Club, Value, Jersey.Number) %>%
      arrange(desc(Overall))
    semantic_DT(subset, options = list(bInfo = F, dom = "rtp"))
  })


  output$league_total_value <- renderText({
    get_total_value(league_data())
  })

  output$league_nr_teams <- renderText({
    get_n_clubs(league_data())
  })

  output$league_nr_players <- renderText({
    get_n_players(league_data())
  })

  output$map_title <- renderText({
    paste("Nationality of The Players in", input$league)
  })
  output$map_nationality <-  renderPlotly({
    world_map <- map_data("world")

    summarized_players <- league_data() %>%
      mutate(Nationality = as.character(Nationality),
             Nationality = if_else(Nationality %in% "England", "UK", Nationality)) %>%
      count(Nationality, name = "Number of Player") %>%
      rename(region = Nationality) %>%
      mutate(region = as.character(region))

    numofplayers <- world_map %>%
      mutate(region = as.character(region)) %>%
      left_join(summarized_players,
                by = "region")

    ggplotly(
      ggplot(numofplayers, aes(long, lat, group = group))+
        geom_polygon(aes(fill = `Number of Player` ), color = "white", show.legend = FALSE)+
        scale_fill_viridis_c(option = "C")+
        theme_void()+
        labs(fill = "Number of Players"))

  })
}
