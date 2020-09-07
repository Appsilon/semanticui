#library(glue)
library(maps)
library(DT)
library(plotly)
library(ggpubr)
library(shiny)
library(shiny.router)
library(shiny.semantic)

source("utils.R")

fifa_data <- read.csv("fifa19_data.csv")
fifa_data$Photo <- fix_photos_link(fifa_data$Photo)
fifa_data$Club.Logo <- fix_club_link(fifa_data$Club.Logo)
fifa_data <- add_extra_columns_to_data(fifa_data)

router <- make_router(
  route("index", source("ui_player.R")$value, source("server_player.R")$value),
  route("country", source("ui_country.R")$value, source("server_country.R")$value),
  route("league", source("ui_league.R")$value, source("server_league.R")$value)
)

server <- function(input, output, session) {
  router(input, output)
}


ui <- semanticPage(
  horizontal_menu(
    list(
      list(name = "Player's details", link = route_link("index"), icon = "running"),
      list(name = "By country", link = route_link("country"), icon = "futbol"),
      list(name = "By league", link = route_link("league"), icon = "futbol outline"),
      list(name = "Info", link = "#", icon = "world")
    ), logo = "http://www.anjelsyndicate.org/wp-content/uploads/2018/06/Fifa-19.png"
  ),
  router_ui()
)


shinyApp(ui, server)
