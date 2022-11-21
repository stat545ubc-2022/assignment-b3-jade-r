library(shiny)
library(DT)
library(tidyverse)
library(datateachr)


ui <- fluidPage(
  h1("Video Game Prices"),
  h2("Welcome to my shiny app!"),
  h5("This app lets you examine the average original price of different video game genres published in different languages."),
  h5("Go ahead and change the genre and/or the price range and see what happens!"),
  br(),
  sidebarLayout(
    sidebarPanel(
      selectInput("genreInput", label = h3("Genre"),
                  choices = list("Action", "Accounting", "Adventure", "Animation & Modeling", "Audio Production", "Casual", "Design & Illustration", "Early Access", "Education", "Free to Play", "Indie", "Massively Multiplayer", "Nudity", "Photo Editing", "Racing", "RPG","Simulation", "Sports", "Strategy", "Utilities", "Video Production", "Violent", "Web Publishing"),
                  selected = 1),
        #This feature allows those using the app to select the genre of video game that they are interested in, and both the plot and the table will update to reflect the average original price of the game for that genre and divide it up into the available languages in that genre. It will also change the overall number of results for that genre. This allows the user to examine specific genres of video games that they are interested in!
      sliderInput("priceInput", "Original Price", 0, 800,
                  value = c(0, 800), pre = "$")
        #This feature allows those using the app to change the price range of video games, so if they are specifically interested in how many Action games cost between $0 and $50 (for example), they can move the slider to this price range and the table, plot, and number of results will reflect the games within that specified price range. Without this feature, the user would have to count the number of games within their desired price range from the table, so this feature greatly simplifies the experience of my app.
      ),
    mainPanel(
      plotOutput("steamgame_prices_barplot"),
      br(),
      textOutput("rows_number"),
      br(),
      tableOutput("data_table")
    )
  )
  )


server <- function(input, output) {

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
                 stat = "identity", col = "darkblue", fill = "darkblue") +
        ylab("Average Original Price ($)") +
        xlab("Language")
    })
  output$data_table <-
    renderTable({
      filtered_data()
      })
  output$rows_number <-
    renderText({
        paste("The number of results in this genre and price range is:", nrow(filtered_data()))
    })
    #This feature allows the user to see how many video games there are within their specified genre and price range. This will update each time they change one or both of the filters, and it is useful for users who are interested in questions like "how many Action games cost less than $50?" There are many genres with thousands of games, so it would be impractical to expect the user to count the number of games from the table.
  }

shinyApp(ui = ui, server = server)
