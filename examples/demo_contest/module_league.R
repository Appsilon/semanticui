source("utils.R")

uiLeague <- function(id, label = "Counter") {
  ns <- NS(id)
  tagList(
    div(class="ui center aligned header", "FIFA'19 Analysis by Leagues"),
    segment(
      selectInput(ns("league"), "Select league:", unique(fifa_data$League), selected = "Bundesliga")
    ),
    segment(
      div(class = "ui three column stackable grid container",
          div(class = "column",
              custom_ui_message(textOutput(ns("league_total_value")),
                                "Total League Value",
                                color = "orange",
                                icon_name = "euro sign")
          ),
          div(class = "column",
              custom_ui_message(textOutput(ns("league_nr_players")),
                                "Number of Players",
                                color = "olive",
                                icon_name = "running")
          ),
          div(class = "column",
              custom_ui_message(textOutput(ns("league_nr_teams")),
                                "Number of Teams",
                                color = "brown",
                                icon_name = "futbol")
          )
      ),
      div(class = "ui two column stackable grid container",
          div(class = "column",
              h1(textOutput(ns("map_title"))),
              plotlyOutput(ns("map_nationality"))
          ),
          div(class = "column",
              tabset(tabs = list(
                list(menu = "All Players", content = semantic_DTOutput(ns("tab"))),
                list(menu = "By Position", content = div(
                  selectInput(ns("league_position"), "Select position:", unique(fifa_data$Class)),
                  div(class="ui divider"),
                  semantic_DTOutput(ns("tab_position"))
                )
                )
              ))
          )
      )
    ), br()
  )
}

leagueServer <- function(id) {
  ns <- NS(id)
  moduleServer(id, function(input, output, session) {
    league_data <- reactive({
      validate(
        need(input$league, "Fetching data...")
      )
      toast("Wait while data is loading...", class = "warning", duration = 1.5)

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
        count(Nationality, name = "Number of Players") %>%
        rename(region = Nationality) %>%
        mutate(region = as.character(region))

      numofplayers <- world_map %>%
        mutate(region = as.character(region)) %>%
        left_join(summarized_players,
                  by = "region")

      ggplotly(
        ggplot(numofplayers, aes(long, lat, group = group))+
          geom_polygon(aes(fill = `Number of Players` ), color = "white", show.legend = FALSE)+
          scale_fill_viridis_c(option = "D")+
          theme_void() +
          labs(fill = "Number of Players"))
    })
  })
}
