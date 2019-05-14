
library(shiny)
library(ggplot2)

ui <- fluidPage(
  sliderInput(inputId = "num", 
              label = "Choose amount of coins", 
              value = 5, min = 2, max = 500),
  actionButton(inputId = "go", 
               label = "Flip Coins!"),
  plotOutput("hist")
)

server <- function(input, output) {
  data <- eventReactive(input$go, {
    heads <- vector()
    y <- 0
    for(i in 1:input$num){
      toss <- sample(c(0,1), 1)
      y <- y + toss
      heads[i] <- y}
    ggplot(data = data.frame(x = seq_along(heads), y = heads),aes(x,heads/1:length(heads))) +
      geom_line() + 
      scale_y_continuous(limits = c(0,1)) + 
      geom_hline(yintercept = 0.5, color = "green") + 
      labs(x = "coinflips", y = "ratio of heads") + 
      annotate("text", 
               label = paste(round((
                 heads/1:length(heads))[input$num]*100
               )
               ,"% heads", sep = ""),
               x = input$num - (input$num*0.1),y = 0.95, size = 6) +
      theme_classic() 
  })
  output$hist <- renderPlot({
    data()
  })
}

shinyApp(ui = ui, server = server)





