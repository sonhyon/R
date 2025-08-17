#랜덤 숫자 그래프 앱

library(shiny)

ui <- fluidPage(
  titlePanel("랜덤 데이터 그래프"),
  sidebarLayout(
    sidebarPanel(
      actionButton("go", "새 데이터 만들기")
    ),
    mainPanel(
      plotOutput("plot")
    )
  )
)

server <- function(input, output) {
  data <- eventReactive(input$go, {
    runif(10, 1, 100)
  })
  
  output$plot <- renderPlot({
    barplot(data(), col = "skyblue", main = "랜덤 숫자 그래프")
  })
}

shinyApp(ui, server)
