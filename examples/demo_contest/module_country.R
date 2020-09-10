source("utils.R")

uiCountry <- function(id, label = "Counter") {
  ns <- NS(id)
  tagList(
    div(class="ui center aligned header", "FIFA'19 Analysis by Nationality"),
    segment(
      label("Select country"),
      uiOutput(ns("nationality_search"))
    ),
    segment(
      div(class = "ui three column stackable grid container",
          div(class = "column",
              custom_ui_message(textOutput(ns("nationality_total_value")),
                                "Total Country Players Value",
                                color = "orange",
                                icon_name = "euro sign")
          ),
          div(class = "column",
              custom_ui_message(textOutput(ns("nationality_nr_players")),
                                "Number of Players",
                                color = "olive",
                                icon_name = "running")
          ),
          div(class = "column",
              custom_ui_message(textOutput(ns("nationality_nr_teams")),
                                "Number of Teams",
                                color = "brown",
                                icon_name = "futbol")
          )
      ),
      div(class = "ui two column stackable grid container",
          div(class = "column",
              h1("Players per League"),
              uiOutput(ns("per_league"))
          ),
          div(class = "column",
              tabset(tabs = list(
                list(menu = "All Players", content = semantic_DTOutput(ns("tab"))),
                list(menu = "By Position", content = div(
                  selectInput(ns("position"), "Select position:", unique(fifa_data$Class)),
                  div(class="ui divider"),
                  semantic_DTOutput(ns("tab_position"))
                )
                ))
              )
          )
      )
    ), br()
  )
}

countryServer <- function(id) {
  ns <- NS(id)
  moduleServer(id, function(input, output, session) {
    COUNTRIES <- unique(fifa_data$Nationality)
    output$nationality_search <- renderUI(
      search_selection_choices(ns("nationality"), COUNTRIES, value = "Poland", multiple = F)
    )

    nat_data <- reactive({
      validate(
        need(input$nationality, "...")
      )
      fifa_data %>% filter(Nationality == input$nationality)
    })

    output$tab <- renderDataTable({
      subset <- nat_data() %>%
        select(Name, Overall, Club, Value, Jersey.Number) %>%
        arrange(desc(Overall))
      semantic_DT(subset, options = list(bInfo = F, dom = "rtp"))
    })

    output$tab_position <- renderDataTable({
      subset <- nat_data() %>%
        filter(Class == input$position) %>%
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

    output$per_league <- renderUI({
      Leagues <- unique(fifa_data$League)
      list_content <- lapply(Leagues, function(x) {
        list(header = x,
             description = get_n_players(nat_data() %>% filter(League == x)),
             img = paste0("imgs/", str_replace_all(tolower(iconv(x, "latin1", "ASCII", sub="")), " ", ""), ".png"))
      })
      div(class = "ui two column stackable grid container",
          div(class = "column",
              custom_image_list(list_content[seq(1, 4)])
          ),
          div(class = "column",
              custom_image_list(list_content[seq(5, length(list_content))])
          )
      )
    })
  })
}
