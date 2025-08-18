library(shiny)

ui <- fluidPage(
  titlePanel("코인 던지기 앱"),
  
  sidebarLayout(
    sidebarPanel(
      actionButton("go", "코인 던지기!")
    ),
    mainPanel(
      textOutput("result")
    )
  )
)

server <- function(input, output) {
  coin <- eventReactive(input$go, {
    sample(c("앞면", "뒷면"), 1)
  })
  
  output$result <- renderText({
    paste("결과:", coin())
  })
}

shinyApp(ui = ui, server = server)
