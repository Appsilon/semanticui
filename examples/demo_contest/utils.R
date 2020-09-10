#' Fix photos link
#'
#' @param photo_vect character vector with links to photos at sofifa cdn
#'
#' @return vector with fixed links
fix_photos_link <- function(photo_vect){
  mm <- gregexpr("[0-9]+\\.png$", photo_vect)
  photo_id <- regmatches(photo_vect, mm)
  photo_id <- gsub("\\.png", "", photo_id)
  first_3 <- substr(photo_id, start=1, stop=3)
  last_3 <- substr(photo_id, start=4, stop=6)
  fixed_link <- paste0("https://cdn.sofifa.com/players/", first_3, "/", last_3, "/20_120.png")
  fixed_link
}

#' Fix club link
#'
#' @param club_vect character vector with links to photos at sofifa cdn
#'
#' @return vector with fixed links
fix_club_link <- function(club_vect){
  mm <- gregexpr("[0-9]+\\.png$", club_vect)
  photo_id <- regmatches(club_vect, mm)
  photo_id <- gsub("\\.png", "", photo_id)
  fixed_link <- paste0("https://cdn.sofifa.com/teams/", photo_id, "/light_60.png")
  fixed_link
}

#' Adds New Performance measures
#'
#' It creates Speed, Power, Technic, Attack and Defence columns
#'
#' @param fifa_data data.frame with fifa 19 data
#'
#' @return data.frame with fifa 19 data and extra columns
add_extra_columns_to_data <- function(fifa_data){
  fifa_data$Speed <- (fifa_data$Sprint.Speed + fifa_data$Agility + fifa_data$Acceleration) / 3
  fifa_data$Power <- (fifa_data$Strength + fifa_data$Stamina + fifa_data$Balance) / 3
  fifa_data$Technic <- (fifa_data$Ball.Control + fifa_data$Dribbling + fifa_data$Vision) / 3
  fifa_data$Attack <- (fifa_data$Finishing + fifa_data$Shot.Power + fifa_data$Long.Shots + fifa_data$Curve) / 4
  fifa_data$Defence <- (fifa_data$Marking + fifa_data$Standing.Tackle + fifa_data$Sliding.Tackle) / 3
  fifa_data
}

#' Filter (select) player
#'
#' @param player_name character with player name
#'
#' @return data.frame with player data
filter_player <- function(player_name) {
  as.list(fifa_data %>% filter(Name == player_name))
}

#' Render player card UI
#'
#' @param player_data data for a player, see \code{filter_player}
#'
#' @return div with player UI card
render_player_card <- function(player_data){
  if (is.na(player_data$photo_path))
    photo_path <- "imgs/empty_profile.png"
  else
    photo_path <- player_data$photo_path
  div(class = "ui fluid card",
      div(class = "content",
        div(class = "right floated", tags$b(player_data$Club)),
        tags$i(class = paste(tolower(player_data$Nationality), "flag")), player_data$Nationality),
      div(class = "image", img(src = photo_path)),
      div(class = "content",
        span(class = "right floated", icon("heart"), "Age:", player_data$Age),
        span(class = "left floated", h3(player_data$Name)),
        br(),br(),
        span(class = "left floated",
             p(icon("walking"), "Position: ", tags$b(player_data$Position), "(", player_data$Class, ")")
        ),
        span(class = "right floated",
             p(icon("arrow up"), "Height: ", tags$b(player_data$Height), "cm")
        ),
        br(),br(),
        span(class = "left floated",
          p(icon("weight"), "Weight: ", tags$b(player_data$Weight), "kg")
        ),
        span(class = "right floated",
          p(icon("shoe prints"), "Preferred foot: ", tags$b(player_data$Preferred.Foot))
        ),
        br(),br(),
        span(class = "left floated",
          p(icon("money bill"), "Value: ", tags$b(player_data$Value))
        ),
        span(class = "right floated",
          p(icon("hand holding usd"), "Wage: ", tags$b(player_data$Wage))
        ),
        br(), br(),
        span(class = "right floated",
          tags$a(class="ui teal huge label", icon("exclamation circle"),
                 "Score:", player_data$Overall)
        )
      )
  )
}

#' Extracts only skills of the player
#'
#' @param player data for a player, see \code{filter_player}
#'
#' @return data.frame with selected columns
select_player_skills <- function(player){
  select(data.frame(player), "Speed", "Finishing", "Dribbling", "Reactions",
         "Agility", "Stamina", "Jumping", "Shot.Power", "Aggression", "Balance",
         "Vision", "Penalties", "Ball.Control")
}

#' Compares skills of 2 players
#'
#' ... and presents results as a graph (vertical bar plot).
#'
#' @param player1 data for a player, see \code{filter_player}
#' @param player2 data for a player, see \code{filter_player}
#'
#' @return barplot
barplot_compare_two <- function(player1, player2) {
  p1 <- select_player_skills(player1)
  p2 <- select_player_skills(player2)

  diff_df <- p1-p2
  diff_df <- data.frame(t(diff_df))
  colnames(diff_df) <- "Score"
  diff_df$Score_lev <- factor(ifelse(diff_df$Score < 0, "worse", "better"),
                              levels = c("worse", "better"))
  diff_df <- tibble::rownames_to_column(diff_df, "names")

  ggbarplot(diff_df, x = "names", y = "Score",
            fill = "Score_lev",
            color = "white",
            palette = "jco",
            x.text.angle = 90,
            ylab = "Score",
            xlab = "Skill",
            legend.title = "Score",
            rotate = TRUE,
            title = paste(player2$Name,
                          paste(rep(" ", 70), collapse = " "),
                          player1$Name)
  )
}

#' Makes barplot with player skills
#'
#' @param player player list from fifa data
#'
#' @return barplot
barplot_player_skills <- function(player) {
  skill <- select_player_skills(player)
  nd<-tibble::rownames_to_column(as.data.frame(t(skill)), "skill")
  ggplot(data=nd, aes(x=skill, y=V1, fill=skill)) +
    labs(y = "Value", x = NULL) +
    geom_bar(stat="identity", show.legend = FALSE) + theme_minimal() +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
}

#' Get total values
#'
#' @param fifa_data subset of fifa_data for which this is calculated
#' @param raw_value if True it returns raw number, if false it returns
#' character formatted to billions
#'
#' @return either character of numeric with total
get_total_value <- function(fifa_data, raw_value = F) {
  total <- sum(fifa_data$Values)
  if (raw_value)
    return(total)
  else
    return(paste(round(sum(total / 1000000000), digits = 1), "B"))
}

#' Get number of clubs
#'
#' @param fifa_data subset of fifa_data for which this is calculated
#' @param raw_value if True it returns raw number, if false it returns
#' character formatted to billions
#'
#' @return either character of numeric with total
get_n_clubs <- function(fifa_data, raw_value = F) {
  length(unique(fifa_data$Club))
}

#' Get number of players
#'
#' @param fifa_data subset of fifa_data for which this is calculated
#' @param raw_value if True it returns raw number, if false it returns
#' character formatted to billions
#'
#' @return either character of numeric with total
get_n_players <- function(fifa_data, raw_value = F) {
  length(unique(fifa_data$Name))
}

#' Message Box UI
#'
#' @param head head value
#' @param content content value
#' @param icon_name name of the semantic icon
#' @param color character with colour name
#' @param size character with size
#'
#' @return div with fomantic message
custom_ui_message <- function(head, content, icon_name = "inbox",
                              color = "purple", size = "big") {
  div(class = glue::glue("ui icon {size} {color} message"),
    icon(icon_name),
    div(class = "content",
        div(class = "header", head),
        p(content)
    )
  )
}

#' Load the fifa 19 data
#'
#' @param path character with data path
#'
#' @return data.frame with fifa data
load_fifa_data <- function(path = "fifa19_data.csv") {
  fifa_data <- read.csv(path)
  #fifa_data$Photo <- fix_photos_link(fifa_data$Photo)
  #fifa_data$Club.Logo <- fix_club_link(fifa_data$Club.Logo)
  fifa_data <- add_extra_columns_to_data(fifa_data)
  fifa_data
}

#' Helper function to render list element
#'
#' @param header character with header element
#' @param description character with content of the list
#' @param icon_name character with optional icon
#'
#' @import shiny
list_element_with_image <- function(header = NULL, description = NULL, img_src = NULL) {
  div(class = "item",
      img(class="ui avatar tiny image", src = img_src),
      div(class = "content",
          div(class = "header", header),
          div(class = "description", h1(description)))
  )
}

#' Animated list
#'
#' @param content_list list of lists with fields: `header` and/or `description`,
#' `image`
#' @return div with animated list
custom_image_list <- function(content_list){
  div(class="ui middle aligned animated selection list",
      content_list %>% purrr::map(function(x) {
        if (is.null(x$header) && is.null(x$description))
          stop("content_list needs to have either header or description.")
        list_element_with_image(x$header, x$description, x$img)
      })
  )
}

fifa_data <- load_fifa_data()
