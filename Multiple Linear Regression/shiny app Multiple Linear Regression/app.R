library(shiny)
library(shinythemes)
library(caTools)
library(ROCR)
library(plotly)
ui<-navbarPage(theme = shinytheme("united"),"Multiple Linear Regression",
               tags$head(
                   tags$style(HTML("
        @import url('https://fonts.googleapis.com/css2?family=Play&display=swap');
        @import url('https://fonts.googleapis.com/css2?family=Monda&display=swap');
        
      body{
        padding-bottom:60px;
        font-family:Monda;
        font-size:16px;
      }
      .tab-pane{
        margin-right:2%;
        margin-left:2%;
      }
      h1, h2, h3{
        color: orange;
        text-align:center;
        font-family:Play;
      }
      p{
        color: orange;
        font-family:Play;
      }
      h2{
        font-family:Play;
        font-size: 50px;
        text-align: left;
        padding-bottom:30px;
        padding-left:24px;
      }
      h4{
      
        font-family:Play;
        font-size: 30px;
        text-align: left;
        padding-top:24px;
      }
      
      h2::first-letter {
        font-size: 70px;
      }
      h3{
        font-size: 35px;
      }
      table, th, td {
        color: orange;
        }
      .col-sm-8{
        border-radius: 5px;
        border: 1px solid black;
        border-color:#b0b0b0;
        padding: 6px;
      }
      li{
        color: orange;
      }
      .well{
        border-radius: 10px;
      }
    "))
               ),
                        sidebarLayout(
                            sidebarPanel(
                                fileInput("file1","Upload your CSV",multiple = FALSE),
                                tags$hr(),
                                checkboxInput(inputId = 'header3', label = 'Header', value = FALSE),
                                radioButtons(inputId = 'sep3', label = 'Separator',
                                             choices = c(Comma=',',Semicolon=';',Tab='\t', Space=''), selected = ','),
                                uiOutput("var1111_select"),
                                uiOutput("var2222_select")  
                            ),
                            mainPanel(
                                tabsetPanel(
                                    tabPanel("Data Set",
                                             uiOutput("tb4")
                                    ),
                                    tabPanel("Multiple regration",
                                             verbatimTextOutput("lmr2")
                                    ),
                                    tabPanel("Visualization",
                                             plotOutput('lmr3')
                                    )
                                )
                            )
                        )
               )

server<-function(input,output) { 
    
    data3 <- reactive({
        file8 <- input$file1
        if(is.null(file8)){return()}
        tt1=read.table(file=file8$datapath, sep=input$sep3, header = input$header3)
    })  
    output$tb4 <- renderUI({
        tableOutput("table3")
    })
    output$table3 <- renderTable({
        data3()
    })
    
    output$var1111_select<-renderUI({
        selectInput("var1111_select","independent varible", choices =as.list(names(data3())),multiple = FALSE)
    })
    output$var2222_select<-renderUI({
        selectInput("var2222_select","dipendent varible", choices =as.list(names(data3())),multiple = TRUE)
    })
    
    output$lmr2<-renderPrint({
        
        f<-data3()
        
        y <- f[,input$var1111_select]
        x <- f[,input$var2222_select]
        
        if(is.vector(x))
        {
            x <- f[,input$var2222_select]
            logreg <- lm(y~x)
        }
        else
        {
            x <- f[,input$var2222_select]
            logreg <- lm(y~ . ,data = x)
        }
        print(summary(logreg))
        
    })
    
    output$lmr3 <- renderPlot({
        f<-data3()
        
        pairs(f[,input$var2222_select])
    })
    
    
    
}
shinyApp(ui=ui,server=server)
