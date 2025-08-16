library(shiny)

exchange_rates <- list(
  "USD (달러)" = 1300,
  "JYP (엔화)" = 9,
  "EUR (유로)" = 1450
)

ui <- fluidPage(
  titlePanel("간단한 환율 변환기"),
  
  sidebarLayout(
    sidebarPanel(
      numericInput("amount", "금액 입력 (KRW)", value = 10000, min = 0),
      selectInput("currency", "변환할 통화 선택", choices = names(exchange_rates))
    ),
    
    mainPanel(
      h3("변환 결과"),
      textOutput("result")
    )
  )
)

server <- function(input, output) {
  output$result <- renderText({
    rate <- exchange_rates[[input$currency]]
    converted <- input$amount / rate
    paste(input$amount, "KRW =", round(converted, 2), input$currency)
  })
}

shinyApp(ui, server)
