library(shiny)
library(plotly)
library(ggplot2)
library(shinythemes)
ui<-navbarPage(
               theme = shinytheme("united"),"Simple Linear Regression",
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
                                fileInput("file5","Upload your CSV",multiple = FALSE),
                                tags$hr(),
                                checkboxInput(inputId = 'header2', label = 'Header', value = TRUE),
                                radioButtons(inputId = 'sep2', label = 'Separator',
                                             choices = c(Comma=','), selected = ','),
                                uiOutput("variable1"),
                                uiOutput("variable2")  
                            ),
                            mainPanel(
                              tabsetPanel(
                                tabPanel("Data",
                                         h3("Data from CSV File :"),
                                         uiOutput("tb3")
                                ),
                                tabPanel("Testing Homogenity& Variance",
                                         br(),
                                         verbatimTextOutput("lmr")
                                ),
                                tabPanel("Anova",
                                         br(),
                                         plotOutput('lmr1')
                                )

                                    
                                )
                            )
                        )
               )


server<-function(input,output) { 
    data <- reactive({
        file1 <- input$file
        if(is.null(file1)){return()}
        tt1=read.table(file=file1$datapath, sep=input$sep, header = input$header)
    })  
    output$tb1 <- renderUI({
        DT::dataTableOutput("table")
    })
    output$table <-  DT::renderDataTable({
        DT::datatable(data(), filter = 'top', options = list( autoWidth = TRUE))
    })
    
    
    
    data1 <- reactive({
        file4 <- input$file3
        if(is.null(file4)){return()}
        tt3=read.table(file=file4$datapath, sep=input$sep1, header = input$header1)
    })
    output$table1 <-  DT::renderDataTable({
        DT::datatable(data1(), filter = 'top', options = list( autoWidth = TRUE))
    })
    output$tb2 <- renderUI({
        DT::dataTableOutput("table1")
    })
    
    data2 <- reactive({
        file6 <- input$file5
        if(is.null(file6)){return()}
        tt1=read.table(file=file6$datapath, sep=input$sep2, header = input$header2)
    })  
    output$tb3 <- renderUI({
        tableOutput("table2")
    })
    
    output$table2 <-  renderTable({
        data2()
    })
    
    data3 <- reactive({
        file8 <- input$file7
        if(is.null(file8)){return()}
        tt1=read.table(file=file8$datapath, sep=input$sep3, header = input$header3)
    })  
    output$tb4 <- renderUI({
        DT::dataTableOutput("table3")
    })
    output$table3 <-  DT::renderDataTable({
        DT::datatable(data3(), filter = 'top', options = list( autoWidth = TRUE))
    })
    
    output$variable1<-renderUI({
        selectInput("variable1","Select Independent Variable", choices =as.list(names(data2())),multiple = FALSE)
    })
    output$variable2<-renderUI({
        selectInput("variable2","Select Dependent Variable", choices =as.list(names(data2())),multiple = FALSE)
    })
    
    
    output$lmr<-renderPrint({
        
        f<-data2()
        
        y <- f[,input$variable1]
        x <- f[,input$variable2]
        logreg <- lm(y~x)
        print(summary(logreg))
        
    })
    
    output$lmr1 <- renderPlot({
        f<-data2()
        
        plot(f[,input$variable1],f[,input$variable2]) 
    })
    
    
    data3 <- reactive({
        file8 <- input$file1
        if(is.null(file8)){return()}
        tt1=read.table(file=file8$datapath, sep=input$sep3, header = input$header3)
    })  
    output$tb4 <- renderUI({
        tableOutput("table3")
    })
    output$table3 <- renderTable({
        head(data3(),50)
    })
    
    output$variable3<-renderUI({
        selectInput("variable3","independent varible", choices =as.list(names(data3())),multiple = FALSE)
    })
    output$variable4<-renderUI({
        selectInput("variable4","dipendent varible", choices =as.list(names(data3())),multiple = TRUE)
    })
    
    output$lmr2<-renderPrint({
        
        f<-data3()
        
        y <- f[,input$variable3]
        x <- f[,input$variable4]
        
        if(is.vector(x))
        {
            x <- f[,input$variable4]
            logreg <- lm(y~x)
        }
        else
        {
            x <- f[,input$variable4]
            logreg <- lm(y~ . ,data = x)
        }
        print(summary(logreg))
        
    })
    
    output$lmr3 <- renderPlot({
        f<-data3()
        
        pairs(f[,input$variable4])
    })
    
}
shinyApp(ui=ui,server=server)
