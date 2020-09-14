library(glue)
library(maps)
library(DT)
library(plotly)
library(ggpubr)
library(tidyverse)
library(tibble)
library(shiny)
library(shiny.router)
library(shiny.semantic)

source("module_player.R")
source("module_league.R")
source("module_country.R")

info_page <- div(
  div(class = "ui two column stackable grid container",
      div(class = "six wide column",
          img(src="imgs/FIFA_19_cover.jpg")
      ),
      div(class = "ten wide column",
          div(class="ui center aligned big header", "FIFA '19 demo app"),
          p("This app was created for the purposes of demonstrating ",
            a(tags$b("shiny.semantic"), href = "https://github.com/Appsilon/shiny.semantic"),
            " features in creating an interactive data visualization. This dashboard uses ",
            a("SoFifa", href = "https://sofifa.com/"), "data and has been inspired by ",
            "an amazing ", a("Fifa Shiny Dashboard", href = "https://ekrem-bayar.shinyapps.io/FifaDash/"), "."),
          p("Feel free to explore the various features of this application and ",
            "analyze your favourite FIFA'19 players."),
          div(class="ui center aligned", style = "text-align: center;",
              action_button("go_modal", "Learn more", class = "teal"),
              br(),br(),
            HTML('<iframe src="https://www.youtube.com/embed/zX0AV6yxyrQ" width="90%" height="300" frameborder="1"></iframe>')
          )
      )
  )
)

router <- make_router(
  route("index", info_page),
  route("player", uiPlayer("p1")),
  route("league", uiLeague("p2")),
  route("country", uiCountry("p3"))
)

server <- function(input, output, session) {
  # router pages
  router$server(input, output, session)#router(input, output) #
  playerServer("p1")
  leagueServer("p2")
  countryServer("p3")

  # modal
  observeEvent(input$go_modal, {
    create_modal(modal(
      id = "simple-modal",
      title = "Info about FIFA ' 91 shiny.semantic app",
      content = list(style = "background: lightblue",
        `data-custom` = "value",
        p( "The data for this app was taken from Kaggle.com, courtesy of ", a("SoFifa", href = "https://sofifa.com/"),
           ". The idea has been inspired by an amazing ",
           a("Fifa Shiny Dashboard", href = "https://ekrem-bayar.shinyapps.io/FifaDash/"), ".")
      ),
      p(tags$b("It was created by Dominik Krzemiński for Appsilon's shiny.semantic contest."))
    ))
  })
}


ui <- semanticPage(
  title = "FIFA'19 App",
  tags$head(
    tags$link(rel="stylesheet", href="style.css", type="text/css" )
  ),
  horizontal_menu(
    list(
      list(name = "Info", link = route_link("index"), icon = "world"),
      list(name = "Player's details", link = route_link("player"), icon = "running"),
      list(name = "By country", link = route_link("country"), icon = "globe europe"),
      list(name = "By league", link = route_link("league"), icon = "futbol outline")
    ), logo = "http://www.anjelsyndicate.org/wp-content/uploads/2018/06/Fifa-19.png"
  ),
  router$ui,#router_ui(),
  tags$footer("Created by dokato for Appsilon", align = "center", style = "
                        position:fixed;
                        bottom:0;
                        right:0;
                        left:0;
                        background:black;
                        color: white;
                        padding:10px;
                        box-sizing:border-box;
                        z-index: 1000;
                        text-align: center")
)


shinyApp(ui, server)
library(glue)
library(maps)
library(DT)
library(plotly)
library(ggpubr)
library(tidyverse)
library(tibble)
library(shiny)
library(shiny.router)
library(shiny.semantic)

source("module_player.R")
source("module_league.R")
source("module_country.R")

info_page <- div(
  div(class = "ui two column stackable grid container",
      div(class = "six wide column",
          img(src="imgs/FIFA_19_cover.jpg")
      ),
      div(class = "ten wide column",
          div(class="ui center aligned big header", "FIFA '19 demo app"),
          p("This app was created for the purposes of demotrating ", tags$b("shiny.semantic"),
            " features for creating interactive data visualization. This dashboard uses ",
            a("SoFifa", href = "https://sofifa.com/"), "data and has been inspired by ",
            "an amazing ", a("Fifa Shiny Dashboard", href = "https://ekrem-bayar.shinyapps.io/FifaDash/"), "."),
          p("Feel free to explore the various features of this application and ",
            "analyze your favourite FIFA'19 players."),
          div(class="ui center aligned", style = "text-align: center;",
              action_button("go_modal", "Learn more", class = "teal"),
              br(),br(),
            HTML('<iframe src="https://www.youtube.com/embed/zX0AV6yxyrQ" width="90%" height="300" frameborder="1"></iframe>')
          )
      )
  )
)

router <- make_router(
  route("index", info_page),
  route("player", uiPlayer("p1")),
  route("league", uiLeague("p2")),
  route("country", uiCountry("p3"))
)

server <- function(input, output, session) {
  # router pages
  router$server(input, output, session)#router(input, output) #
  playerServer("p1")
  leagueServer("p2")
  countryServer("p3")

  # modal
  observeEvent(input$go_modal, {
    create_modal(modal(
      id = "simple-modal",
      title = "Info about FIFA ' 91 shiny.semantic app",
      content = list(style = "background: lightblue",
        `data-custom` = "value",
        p( "The data for this app was taken from Kaggle.com, courtesy of ", a("SoFifa", href = "https://sofifa.com/"),
           ". The idea has been inspired by an amazing ",
           a("Fifa Shiny Dashboard", href = "https://ekrem-bayar.shinyapps.io/FifaDash/"), ".")
      ),
      p(tags$b("It was created by Dominik Krzemiński for Appsilon's shiny.semantic contest."))
    ))
  })
}


ui <- semanticPage(
  title = "FIFA'19 App",
  tags$link(rel="stylesheet", href="style.css", type="text/css" ),
  horizontal_menu(
    list(
      list(name = "Info", link = route_link("index"), icon = "world"),
      list(name = "Player's details", link = route_link("player"), icon = "running"),
      list(name = "By country", link = route_link("country"), icon = "futbol"),
      list(name = "By league", link = route_link("league"), icon = "futbol outline")
    ), logo = "http://www.anjelsyndicate.org/wp-content/uploads/2018/06/Fifa-19.png"
  ),
  router$ui,#router_ui(),
  tags$footer("Created by dokato for Appsilon (2020)", align = "center", style = "
                        position:fixed;
                        bottom:0;
                        right:0;
                        left:0;
                        background:black;
                        color: white;
                        padding:10px;
                        box-sizing:border-box;
                        z-index: 1000;
                        text-align: center")
)


shinyApp(ui, server)
