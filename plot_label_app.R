## Libraries ---- 

library(shiny)
library(ggplot2)
library(ggrepel)


## UI ----
ui <- fluidPage(
  fileInput("txt_data", "Supply tab delimited .txt file of MAGeCK data"),
  fluidRow(column(3, selectInput("name", "Attribute name", choices = NULL)),
           column(3, selectInput("x", "LFC (X values)", choices = NULL)),
           column(3, selectInput("yn", "Negative Score (Y values)", choices = NULL)),
           column(3, selectInput("yp", "Positive Score (Y values)", choices = NULL))),
  fluidRow(column(3,actionButton("preview", "Show preview of data")),
           column(3,actionButton("plot", "Generate Scatterplot"))),
  tableOutput("head_data"),
  checkboxGroupInput("y_trans", "Transformations for y-axis:", c("log10", "reverse"), inline= TRUE),
  plotOutput("scatter", click = "plot_click", hover = "plot_hover"),
  tableOutput("nT_hover")
)


## SERVER ----

server <- function(input, output, session) {
  data <- reactive({
    req(input$txt_data)
    read.delim(file =input$txt_data$datapath)
  })

  output$head_data <- renderTable({
    req(input$preview)
    head(data())
  })

  observeEvent(data(), {
    choices <- colnames(data())
    updateSelectInput(inputId = "name", choices = c("Choose one" = "", choices))
    updateSelectInput(inputId = "x", choices = c("Choose one" = "", choices))
    updateSelectInput(inputId = "yn", choices = c("Choose one" = "", choices))
    updateSelectInput(inputId = "yp", choices = c("Choose one" = "", choices))
  })
  

  At_names <- reactive(input$name)
  At_x <- reactive(input$x)
  At_yn <- reactive(input$yn)
  At_yp <- reactive(input$yp)
  
  
  plot_LFC_data <- reactive({
    if ("log10" %in% input$y_trans){
      data.frame(ID = data()[,At_names()], 
                 LFC = data()[,At_x()],
                 Score = log10(ifelse(data()[,At_x()] < 0, data()[,At_yn()], data()[,At_yp()])))
    } else {
      data.frame(ID = data()[,At_names()], 
                 LFC = data()[,At_x()],
                 Score = ifelse(data()[,At_x()] < 0, data()[,At_yn()], data()[,At_yp()]))
    }
    
  })
  
  g <- reactive({
    if ("reverse" %in% input$y_trans) {
      ggplot(data=plot_LFC_data()) +
        geom_point(aes(x=LFC, y=Score), shape = 16, size = 3) +
        scale_y_continuous(trans = "reverse")
    } else {
      ggplot(data=plot_LFC_data()) +
        geom_point(aes(x=LFC, y=Score), shape = 16, size = 3)
    }
  })

  
  #On mouse hover, display Near Point data
  nearTable_h <- reactive({
    req(input$plot_hover)
    nearPoints(plot_LFC_data(), input$plot_hover)
  })
  
  output$nT_hover <- renderTable(nearTable_h())
  
  #Use NearPoints to add labels to plot on click
  
  id_lab <- reactive({
    req(input$plot_click)
    nearPoints(plot_LFC_data(), input$plot_click)[,1]
  })
  
  selected <- reactiveVal({
    vector()
  })
  

  observeEvent(input$plot_click,
               selected(c(id_lab(), selected()))  
  )
  #If new plot generated, reset selected info
  observeEvent(input$scatter,
               selected(vector()))
  
  
  output$scatter <- renderPlot({
    req(input$plot)
    if (length(selected() > 0)) {
      tf_labels <- ifelse(plot_LFC_data()$ID %in% selected(), TRUE, FALSE)
      g() + 
        geom_text_repel(aes(x=LFC, y=Score, label=ifelse(tf_labels, ID, '')), 
                        min.segment.length = 0, size = 3, max.overlaps = 15)
    } else {
      g()
    }
  })
  
  
}


## shinyApp ----

shinyApp(ui, server)
