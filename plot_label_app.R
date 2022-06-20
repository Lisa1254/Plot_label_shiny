## Libraries ---- 

library(shiny)
library(ggplot2)
library(ggrepel)

col_hex <- setNames(c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7", "#E6E6E6"), 
                    c("Black", "Orange", "Sky Blue", "Bluish Green", "Yellow", "Blue", "Red", "Purple", "Light Gray"))


## UI ----
ui <- fluidPage(
  fluidRow(column(5, fileInput("txt_data", "Supply tab delimited .txt file of MAGeCK data", width = "100%")),
           column(3, actionButton("preview", "Show preview of data"))),
  tableOutput("head_data"),
  fluidRow(column(3, selectInput("name", "Attribute name", choices = NULL)),
           column(3, selectInput("x", "LFC (X values)", choices = NULL))),
  
  fluidRow(column(3, selectInput("yn", "Negative Score (Y values)", choices = NULL)),
           column(3, selectInput("yp", "Positive Score (Y values)", choices = NULL)),
           column(3,checkboxGroupInput("y_trans", "Transformations for y-axis:", c("log10", "reverse")))),
  fluidRow(column(4, selectInput("col_m", "Main colour", choices = col_hex)),
           column(4, selectInput("col_a", "Accent colour", choices = col_hex))),
  actionButton("plot", "Generate Scatterplot"),
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

  
  plot_LFC_data <- reactive({
    req(input$plot)
    if ("log10" %in% input$y_trans){
      data.frame(ID = data()[,input$name], 
                 LFC = data()[,input$x],
                 Score = log10(ifelse(data()[,input$x] < 0, data()[,input$yn], data()[,input$yp])))
    } else {
      data.frame(ID = data()[,input$name], 
                 LFC = data()[,input$x],
                 Score = ifelse(data()[,input$x] < 0, data()[,input$yn], data()[,input$yp]))
    }
    
  })
  
  gp_data <- reactive({
    rep_len(c("Main", "Gp1"), nrow(plot_LFC_data()))
  })
  
  cols <- reactive({
    c("Main" = input$col_m, "Gp1" = input$col_a)
  })
  
  output$scatter <- renderPlot({
    req(input$plot)
    plot_gp_data <- plot_LFC_data()
    plot_gp_data$Col <- gp_data()
    if ("reverse" %in% input$y_trans) {
      ggplot(data=plot_gp_data) +
        geom_point(aes(x=LFC, y=Score, color = factor(Col)), shape = 16, size = 3) +
        scale_color_manual(values = cols()) +
        scale_y_continuous(trans = "reverse")
    } else {
      ggplot(data=plot_gp_data) +
        geom_point(aes(x=LFC, y=Score, color = factor(Col)), shape = 16, size = 3) +
        scale_color_manual(values = cols())
    }
  })
  
#  g <- reactive({
#    if ("reverse" %in% input$y_trans) {
#      ggplot(data=data.frame(cbind(plot_LFC_data(), Col = gp_data()))) +
#        geom_point(aes(x=LFC, y=Score, color=Col), shape = 16, size = 3) +
#        scale_y_continuous(trans = "reverse")
#    } else {
#      ggplot(data=data.frame(cbind(plot_LFC_data(), Col = gp_data()))) +
#        geom_point(aes(x=LFC, y=Score, color=Col), shape = 16, size = 3)
#    }
#  })
#, color = input$col_m
  
  #On mouse hover, display Near Point data
#  nearTable_h <- reactive({
#    req(input$plot_hover)
#    nearPoints(plot_LFC_data(), input$plot_hover)
#  })
  
#  output$nT_hover <- renderTable(nearTable_h())
  
  #Use NearPoints to add labels to plot on click
  
#  id_lab <- reactive({
#    req(input$plot_click)
#    nearPoints(plot_LFC_data(), input$plot_click)[,1]
#  })
  
#  selected <- reactiveVal({
#    vector()
#  })
  

#  observeEvent(input$plot_click,
#               selected(c(id_lab(), selected()))  
#  )
  #If new plot generated, reset selected info
#  observeEvent(input$scatter,
#               selected(vector()))
  
  
#  output$scatter <- renderPlot({
#    req(input$plot)
#    if (length(selected() > 0)) {
#      tf_labels <- ifelse(plot_LFC_data()$ID %in% selected(), TRUE, FALSE)
#      g() + 
#        geom_text_repel(aes(x=LFC, y=Score, label=ifelse(tf_labels, ID, '')), 
#                        min.segment.length = 0, size = 3, max.overlaps = 15)
#    } else {
#      g()
#    }
#  })
  
  
}


## shinyApp ----

shinyApp(ui, server)
