library(shiny)

ui <- fluidPage(
  titlePanel("w-F Simulation: B ~ Gamma(2,1)"),
  sidebarLayout(
    sidebarPanel(
      numericInput("iter", "Number of Iterations:", 1000, min = 50, max = 1000000),
      strong("Note: The pink curve is the theoretical distribution for aGamma(2,1)",
        style = "color:hotpink"),
    ),
    mainPanel(
      plotOutput("hist")
    )
  )
)
server <- function(input, output, session) {
  output$hist <- renderPlot({
    rands <- rgamma(input$iter, shape = 2, scale =  1)
    hist(rands,
         freq = FALSE,
         col = "grey",
         main = "Histogram of B",
         xlab = "B",
         ylim = c(0, 0.4),
         xlim = c(0, 12))

    curve(dgamma(x, shape = 2, scale = 1),
          add = TRUE,
          col = "hotpink",
          lwd = 3)
  }, res = 96)
}
# Run the application
shinyApp(ui, server)
