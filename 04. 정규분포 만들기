library(shiny)

ui <- fluidPage(
  titlePanel("정규분포 만들기"),
  
  sidebarLayout(
    sidebarPanel(
      sliderInput("mean", "평균:", min=-10, max=10, value = 0, step = 0.1),
      sliderInput("sd", "표준편차:", min=0.1, max=5, value=1, step=0.1)
    ),
    
    mainPanel(
      plotOutput("distplot")
    )
  )
)

server <- function(input, output){
  output$distplot <- renderPlot({
    x <- seq(-20, 20, length.out = 400)
    y <- dnorm(x, mean = input$mean, sd=input$sd)
    
    plot(x, y, type = "l", lwd=2, col="blue",
         main = paste("정규분포 N(", input$mean, ",", input$sd, ")"),
         ylab = "밀도",
         xlab = "x")
  })
}

shinyApp(ui, server)
