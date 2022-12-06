library(shiny)
library(DT)
library(tidyverse)
library(datateachr)
library(DT)
library(shinythemes)

ui <- fluidPage(theme=shinytheme("spacelab"),
  img(src = "game.png", height = 125, width = 245, align = "left"),
  img(src = "game.png", height = 125, width = 245, align = "right"),
  h1("Steam Games Video Game Data", align = "center"),
  br(),
  br(),
  br(),
  sidebarLayout(
    sidebarPanel(
      selectInput("genreInput", label = h3("Genre"),
                  choices = list("Action", "Accounting", "Adventure", "Animation & Modeling", "Audio Production", "Casual", "Design & Illustration", "Early Access", "Education", "Free to Play", "Indie", "Massively Multiplayer", "Nudity", "Photo Editing", "Racing", "RPG","Simulation", "Sports", "Strategy", "Utilities", "Video Production", "Violent", "Web Publishing"),
                  selected = 1),
      sliderInput("priceInput", "Original Price", 0, 800,
                  value = c(0, 800), pre = "$")
      ),
    mainPanel(
      textOutput("rows_number"),
      tags$head(tags$style("#rows_number{font-size: 22px}")),
      br(),
    navbarPage("Steam Games",
              tabPanel("About the App", p("The Steam Game Video Games app allows you to examine the average price of different video game genres published in different languages."),
                       p("This app is completely interactive. Use the Genre list to pick the genre you're interested in and use the Original Price slider to examine specific price ranges you're curious about."),
                       br(),
                       p("The Plot tab in the navigation bar will take you to an interactive bar plot that changes depending on your selected genre and price range."),
                       p("The Table tab in the navigation bar will take you to an interactive table that will show you more information on each game within your selected genre and price range."),
                       br(),
                       p("Start playing with the Genre choices and Original Price slider and watch what happens!"),
                       br(),
                       br(),
                       br(),
                       p("The data used for this app comes from the steam_games data set in R."),
                       p("To access the original data, simply load library(datateachr) and steam_games into your R console.")),
              tabPanel("Plot", plotOutput("steamgame_prices_barplot")),
              tabPanel("Table", DT::DTOutput("data_table")))
    )
  )
  )


server <- function(input, output) {

  sg_tidy <- steam_games %>%
    separate(languages, into = c("primary_language", "secondary_language"), sep = ",") %>%
    separate(genre, into = c("primary_genre", "secondary_genre"), sep = ",") %>%
    filter(!is.na(secondary_language)) %>%
    filter(!is.na(original_price)) %>%
    filter(!is.na(primary_genre))

  sg_tidiest = subset(sg_tidy, select = c(primary_language, secondary_language, original_price, discount_price, primary_genre))

  filtered_data <-
    reactive({sg_tidiest %>%
        filter(original_price > input$priceInput[1] &
                 original_price < input$priceInput[2] &
                 primary_genre == input$genreInput)
      })

  output$steamgame_prices_barplot <-
    renderPlot({
      filtered_data() %>%
        group_by(primary_genre, primary_language) %>%
        summarise(mean = mean(original_price)) %>%
      ggplot() +
        geom_bar(aes(x = primary_language, y = mean),
                 stat = "identity", col = "mediumorchid3", fill = "mediumorchid3") +
        ylab("Average Original Price ($)") +
        xlab("Language")
    })
  output$data_table <-
    DT::renderDataTable({
      DT::datatable(data = filtered_data(),
      options = list(scrollX = TRUE)
      )
      })
  output$rows_number <-
    renderText({
        paste("The number of results in this genre and price range is:", nrow(filtered_data()))
    })
  }

shinyApp(ui = ui, server = server)
