library(shiny)

ui <- fluidPage(
  titlePanel("정규분포 2개 그래프"),
  
  sidebarLayout(
    sidebarPanel(
      h4("첫 번째 정규분포"),
      sliderInput("mean1", "평균1:", min=-10, max=10, value=-2, step=0.1),
      sliderInput("sd1", "표준편차1", min=0.1, max=5, value=1, step=0.1),
      
      h4("두 번째 정규분포"),
      sliderInput("mean2", "평균2:", min=-10, max= 10, value=2, step=0.1),
      sliderInput("sd2", "표준편차2:", min=0.1, max=5, value=1, step=0.1)
    ),
    
    mainPanel(plotOutput("distPlot"))
    )
  )

server <- function(input, output){
  output$distPlot <- renderPlot({
    x <- seq(-20, 20, length.out = 400)
    y1 <- dnorm(x, mean=input$mean1, sd = input$sd1)
    y2 <- dnorm(x, mean=input$mean2, sd = input$sd2)
    
    plot(x, y1, type = "l", lwd=2, col="blue",
         main = "정규분포 2개 비교",
         ylab = "밀도",
         xlab = "x")
    lines(x, y2, col="red", lwd=2)
    legend("topright", legend = c("분포1", "분포2"), col=c("blue", "red"), lwd=2)
  })
}

shinyApp(ui, server)
