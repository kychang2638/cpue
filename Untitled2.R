

library(shiny)

rm(list=ls(all=TRUE))

cpue = read.table(file = "enviro_new.txt", header = T)

ui<-fluidPage(
  # 1.整體網頁佈局設計：大標題
   titlePanel("CPUE index 預測"),
    sidebarLayout(
      sidebarPanel(

        sliderInput("slider1", label = "Slider", min = 10, max = 40, value = 25,step = 0.1)
      ),
      mainPanel(plotOutput("distPlot"))
    )
  )

server <- function(input, output) {
  output$distPlot <- renderPlot({
      cpue$A1_5[12] =  input$slider1
      yy  <- 11.14400 - 0.23883 * cpue$A1_5 - 0.06013 * cpue$preAO_12 
      py = yy[12]
      cpue$lnCPUE[12] = py 
      aa = max(cpue$lnCPUE)
      bb = min(cpue$lnCPUE)
      plot(x=c(1:12), y = cpue$lnCPUE, ylim=c(bb-0.5,aa + 0.5),cex = 2)
      lines(cpue$lnCPUE,type="l")
    })
}

 shinyApp(ui=ui,server=server)