# Packages ----
library(shiny) # Required to run any Shiny app
library(ggplot2) # For creating pretty plots
library(dplyr) # For filtering and manipulating data
library(agridat) # The package where the data comes from

# Loading data ----
Barley <- as.data.frame(beaven.barley)

# ui.R ----
ui <- fluidPage(
  titlePanel("Barley Yield"), # Add a title panel
  sidebarLayout( # Make the layout a sidebarLayout
    sidebarPanel(
      selectInput(
        inputId = "genotype", # Give the input a name "genotype"
        label = "1. Select genotype", # Give the input a label to be displayed in the app
        choices = unique(Barley$gen), selected = "a"
      ), # Create the choices that can be selected. e.g. Display "A" and link to value "a"
      selectInput(
        inputId = "colour",
        label = "2. Select histogram colour",
        choices = c("blue", "green", "red", "purple", "grey"), selected = "grey"
      ),
      sliderInput(
        inputId = "bin",
        label = "3. Select number of histogram bins",
        min = 1, max = 25, value = c(10)
      ),
      textInput(
        inputId = "text",
        label = "4. Enter some text to be displayed", ""
      )
    ), # Inside the sidebarLayout, add a sidebarPanel
    mainPanel(
      plotOutput("myhist"),
      tableOutput("mytable"),
      textOutput("mytext")
    ) # Inside the sidebarLayout, add a mainPanel
  )
)

# server.R ----
server <- function(input, output) {
  # using renderPlot() to wrap a ggplot() command
  output$myhist <- renderPlot(
    ggplot(Barley, aes(x = yield)) + # Create object called `output$plot` with a ggplot inside it
      geom_histogram(
        bins = input$bin, # Add a histogram to the plot
        fill = input$colour, # Make the fill color grey
        data = Barley[Barley$gen == input$genotype, ], # Use data from `Barley`
        colour = "black"
      ) # Outline the bins in black
  )

  output$mytext <- renderText(input$text)

  output$mytable <- renderTable(Barley %>%
    filter(gen == input$genotype) %>%
    summarise(
      "Mean" = mean(yield),
      "Median" = median(yield),
      "STDEV" = sd(yield),
      "Min" = min(yield),
      "Max" = max(yield)
    ))
}

# Run the app ----
shinyApp(ui = ui, server = server)
