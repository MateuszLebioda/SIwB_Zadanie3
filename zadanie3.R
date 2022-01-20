library(shiny)
library(rpart)
library("rpart.plot")

dane <- read.csv("./TitanicMess.csv", header=T, sep=',')

ui <- fluidPage(
  titlePanel("Titanic"),
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId = "selectColumn", h3("Smiertelnosc a "),
                  choices = list("Wiek" = 1,
                                 "Klasa" = 2,
                                 "Czlonkowie rodziny" = 3,
                                 "Plec" = 4),
                  selected = 1)
    ),
    mainPanel(
      plotOutput(outputId = "distPlot")
      
    )
  ),
  sidebarLayout(
    sidebarPanel(
             checkboxGroupInput("checkGroup", 
                                h3("Szansa na przezycie "), 
                                choices = list("Wiek" = "Age",
                                               "Klasa" = "Pclass",
                                               "Czlonkowie rodziny" = "SibSp",
                                               "Plec" = "Sex"),
                                selected = "Sex"))
    ,mainPanel( plotOutput(outputId = "secPlot")
      
    )
  )
)

server <- function(input, output){
  output$distPlot <- renderPlot({
    if(input$selectColumn == 1){
      secCol <- dane$Age
    }else if(input$selectColumn == 2){
      secCol <- dane$Pclass
    }else if(input$selectColumn == 3){
      secCol <- dane$SibSp
    }else {
      secCol <- dane$Sex
    }
    
    barplot(table(dane$Survived, secCol), legend = c("Zginelo","Przezylo"))
  })
  
  output$secPlot <- renderPlot({
    
    if(length(input$checkGroup) >= 1){
      LT=dim(dane)[1]
      train_im<- dane[1:LT,c("Survived" , input$checkGroup)]
      model_dt<- rpart(Survived ~.,data=train_im, method="class")
      rpart.plot(model_dt)
    }

  })
}

shinyApp(ui = ui, server = server)

