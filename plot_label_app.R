library(shiny)
library(ggplot2)

ui <- fluidPage(
  fileInput("txt_data", "Supply tab delimited .txt file of data"),
  fluidRow(column(3, selectInput("name", "Attribute name", choices = NULL)),
           column(3, selectInput("x", "X values", choices = NULL)),
           column(3, selectInput("y", "Y values", choices = NULL))),
  fluidRow(column(3, textOutput("name_choice")),
           column(3, textOutput("x_choice")),
           column(3, textOutput("y_choice"))),
  fluidRow(column(3,actionButton("preview", "Show preview of data")),
           column(3,actionButton("plot", "Generate Scatterplot"))),
  tableOutput("head_data"),
  plotOutput("scatter")
)

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
    updateSelectInput(inputId = "y", choices = c("Choose one" = "", choices))
  })
  

  At_names <- reactive(input$name)
  At_x <- reactive(input$x)
  At_y <- reactive(input$y)
  
  output$name_choice <- renderText({
    req(input$plot)
    paste0("Chosen name variable: ", At_names())
    })
  output$x_choice <- renderText({
    req(input$plot)
    paste0("Chosen name variable: ", At_x())
    })
  output$y_choice <- renderText({
    req(input$plot)
    paste0("Chosen name variable: ", At_y())
    })
  
  output$scatter <- renderPlot({
    req(input$plot)
    plot_frame <- data.frame(Name=data()[,At_names()], 
                             x_val=data()[,At_x()], 
                             y_val=data()[,At_y()])
    ggplot(data=plot_frame) +
      geom_point(aes(x=x_val, y=y_val), shape = 15, size = 3)
  })
  
}

shinyApp(ui, server)