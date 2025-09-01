library(shiny)

ui <- fluidPage(
  titlePanel("랜덤 데이터 히스토그램"),
  
  sidebarLayout(
    sidebarPanel(
      sliderInput("num",
                  "샘플 개수 선택:",
                  min=10, max=1000, value=100)
    ),
    
    mainPanel(plotOutput("histPlot")
              )
  )
)

server <- function(input, output){
  output$histPlot <- renderPlot({
    data <- rnorm(input$num)
    hist(data, col = "skyblue", border="white",
         main = paste("랜덤 데이터 (n =", input$num, ")"), xlab="값")
  })
}

shinyApp(ui, server)
