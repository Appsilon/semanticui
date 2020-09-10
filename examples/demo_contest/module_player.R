source("utils.R")

uiPlayer <- function(id, label = "Counter") {
  ns <- NS(id)
  shiny::tagList(
    sidebar_layout(
      sidebar_panel(
        h2("Select some player"),
        uiOutput(ns("search_players")),
        br(),
        p("Here you can find a player and learn about his most important skills."),
        br(),
        h5("Scroll down to compare a player to another player!"),
        br(),
        div(class="ui divider"),
        h4("Quick Q&A about the dataset"),
        accordion(list(
          list(title = "How many players this dataset contains?",
               content = p(paste("There is data from", length(fifa_data$X), " of players from FIFA 2019."))),
          list(title = "How many leagues are included?",
               content = p(paste("It contains records from", length(unique(fifa_data$League)), "football leagues in Europe"))),
          list(title = "From how many countries and clubs?",
               content = p(paste("This data represents players from", length(unique(fifa_data$Nationality)),
                                 "countries and", length(unique(fifa_data$Club)), "clubs.")))
        ))
      ),
      main_panel(
        div(class="ui center aligned header", "Player Analysis"),
        div(class = "ui two column stackable grid container",
            div(class = "six wide column",
                uiOutput(ns("selected_player"))
            ),
            div(class = "ten wide column",
                segment(
                  div(class="ui black ribbon label", "Skills"),
                  plotlyOutput(ns("player_radar")),
                  plotlyOutput(ns("barplot"), height = "200px")
                )
            )
        )
      )
    ),
    br(),
    sidebar_layout(
      sidebar_panel(
        h2("Select player for comparison"),
        uiOutput(ns("search_comparison_player"))
      ),
      main_panel(
        plotOutput(ns("players_comparison_barplot"))
      )
    ), br(), br()
  )
}


playerServer <- function(id) {
  ns <- NS(id)
  moduleServer(id, function(input, output, session) {
    choices <-fifa_data$Name
    output$search_players <- renderUI(
      search_selection_choices(ns("search_result"), choices, value = "L. Messi", multiple = F)
    )

    output$search_comparison_player <- renderUI(
      search_selection_choices(ns("search_result_comparison"), choices)
    )

    player1 <- reactive({
      validate(
        need(nchar(input[["search_result"]]) > 0, "Loading...")
      )
      filter_player(input[["search_result"]])
    })

    output$selected_player <- renderUI({
      render_player_card(player1())
    })

    output$player_radar <- renderPlotly({
      plot_ly(
        type = 'scatterpolar',
        fill = 'toself'
      ) %>%
        add_trace(
          r = c(player1()$Speed, player1()$Power, player1()$Technic, player1()$Attack, player1()$Defence),
          theta = c('Speed', 'Power', 'Technic', 'Attack', 'Defence'),
          name = player1()$Name.Pos
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
      player1_list <- player1()
      player2_list <- filter_player(input[["search_result_comparison"]])
      barplot_compare_two(player1_list, player2_list)
    })

    output$barplot <- renderPlotly({
      ggplotly(barplot_player_skills(player1()))
    })
  }
  )
}
