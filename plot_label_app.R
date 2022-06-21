## Libraries ---- 

library(shiny)
library(purrr)
library(ggplot2)
library(ggrepel)

col_hex <- setNames(c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7", "lightgray"), 
                    c("Black", "Orange", "Sky Blue", "Bluish Green", "Yellow", "Blue", "Red", "Purple", "Light Gray"))

condPan01 <- function(number) {
  conditionalPanel(paste0("input.num_gps >=", as.numeric(number)),
                   fluidRow(column(3, textInput(paste0("gp", number), label = paste0("Group Name: "))),
                            column(3, selectInput(paste0("col", number), label = paste0("Colour for Group ", number),
                                                  choices = col_hex))))
}
map_conds <- map(c(1,2,3), condPan01)

## UI ----
ui <- fluidPage(
  titlePanel("Scatterplot with Custom Labels"),
  fluidRow(column(5, fileInput("txt_data", "Supply tab delimited .txt file", width = "100%")),
           column(3, checkboxInput("preview", "Show preview of data"))),
  conditionalPanel(
    condition = "input.preview == true",
    tableOutput("head_data")),
  
  fluidRow(column(3, selectInput("name", "Labels for data points", choices = NULL)),
           column(3, textInput("xlab", "Attribute name for x values", placeholder = "e.g. LFC")),
           column(3, selectInput("x", "X values", choices = NULL))),
  
  radioButtons("num_y", "Number of Inputs for Y-axis", choices = c("1", "2"), inline = TRUE),
  fluidRow(column(3, textInput("ylab", "Attribute name for y values", placeholder = "e.g. Score (p97i)")),
           conditionalPanel(condition = "input.num_y == 1",
                            column(3, selectInput("y1", "Y values", choices = NULL))),
           conditionalPanel(condition = "input.num_y == 2",
                            column(3, selectInput("yn", "Negative Score (Y values)", choices = NULL)),
                            column(3, selectInput("yp", "Positive Score (Y values)", choices = NULL)))),

  fluidRow(column(4, selectInput("col_m", "Main colour", choices = col_hex)),
           column(4,checkboxGroupInput("y_trans", "Transformations for y-axis:", c("log10", "reverse")))),
  
  sliderInput("num_gps", "How many accent groups?", min=0, max=3, step=1, value=0),
  map_conds,
  conditionalPanel("input.num_gps >=1",
                   radioButtons("current_gp", "Current group for labelling", choices = "none")),
  
  actionButton("plot", "Generate/Reset Scatterplot"),

  fluidRow(column(9, plotOutput("scatter", click = "plot_click", hover = "plot_hover", brush = "plot_brush")),
           column(3, tableOutput("nT_hover"))),
  
  downloadButton("dl_plot", "Save plot as pdf")
)


## SERVER ----

server <- function(input, output, session) {
  data <- reactive({
    req(input$txt_data)
    read.delim(file =input$txt_data$datapath)
  })

  output$head_data <- renderTable(head(data()), rownames = TRUE)
  

  observeEvent(data(), {
    choices <- colnames(data())
    updateSelectInput(inputId = "name", choices = c("Choose one" = "", c("Rownames", choices)))
    updateSelectInput(inputId = "x", choices = c("Choose one" = "", choices))
    if (input$num_y == "1") {
      updateSelectInput(inputId = "y1", choices = c("Choose one" = "", choices))
    } else if (input$num_y == "2") {
      updateSelectInput(inputId = "yn", choices = c("Choose one" = "", choices))
      updateSelectInput(inputId = "yp", choices = c("Choose one" = "", choices))
    }
  })

  observeEvent(input$num_y, {
    req(input$txt_data)
    choices <- colnames(data())
    if (input$num_y == "1") {
      updateSelectInput(inputId = "y1", choices = c("Choose one" = "", choices))
    } else if (input$num_y == "2") {
      updateSelectInput(inputId = "yn", choices = c("Choose one" = "", choices))
      updateSelectInput(inputId = "yp", choices = c("Choose one" = "", choices))
    }
  })

  y_vals <- reactive({
    req(input$plot)
    if (input$num_y == "1") {
      data()[,input$y1]
    } else if (input$num_y == "2"){
      ifelse(data()[,input$x] < 0, data()[,input$yn], data()[,input$yp])
    }
  })
  
  
  data_names <- reactive({
    req(input$plot)
    if (input$name == "Rownames") {
      rownames(data())
    } else {
      data()[,input$name]
    }
  })
  
  all_labels <- reactive(c(input$gp1, input$gp2, input$gp3)[1:input$num_gps])
  
  #Need to figure out how to better iterate or make a module/function for this
  observeEvent(input$gp1, {
    updateSelectInput(inputId = "col1", label = paste0("Colour for ", input$gp1))
    updateRadioButtons(inputId = "current_gp", choices =all_labels())
  })
  observeEvent(input$gp2,{
    updateSelectInput(inputId = "col2", label = paste0("Colour for ", input$gp2))
    updateRadioButtons(inputId = "current_gp", choices =all_labels())
  })
  observeEvent(input$gp3,{
    updateSelectInput(inputId = "col3", label = paste0("Colour for ", input$gp3))
    updateRadioButtons(inputId = "current_gp", choices =all_labels())
  })
  
  plot_LFC_data <- reactive({
    req(input$plot)
    if ("log10" %in% input$y_trans){
      data.frame(ID = data_names(), 
                 LFC = data()[,input$x],
                 Score = log10(y_vals()))
    } else {
      data.frame(ID = data_names(), 
                 LFC = data()[,input$x],
                 Score = y_vals())
    }
    
  })
  
  gp_data <- reactive({
    rep_len("Main", nrow(plot_LFC_data()))
  })
  
  cols <- reactive({
    c("Main" = input$col_m, "Gp1" = input$col1, "Gp2" = input$col2, "Gp3" = input$col3)[1:(input$num_gps+1)]
  })
  
  #### For download handler, need to contain all of this within a reactive instead, 
  #then in download Handler, use ggsave.
  make_scatter <- reactive({
    input$plot
    plot_gp_data <- plot_LFC_data()
    plot_gp_data$Gp <- gp_data()
    plot_gp_data$Gp <- ifelse(plot_gp_data$ID %in% selected1(), "Gp1", plot_gp_data$Gp)
    plot_gp_data$Gp <- ifelse(plot_gp_data$ID %in% selected2(), "Gp2", plot_gp_data$Gp)
    plot_gp_data$Gp <- ifelse(plot_gp_data$ID %in% selected3(), "Gp3", plot_gp_data$Gp)
    plot_gp_data$Gp <- factor(plot_gp_data$Gp)
    if ("reverse" %in% input$y_trans) {
      ggplot(data=plot_gp_data) +
        geom_point(aes(x=LFC, y=Score, color = Gp), shape = 16, size = 3) +
        scale_color_manual(name = "Groups", labels = c("NA", all_labels()), values = cols()) +
        geom_text_repel(aes(x=LFC, y=Score, label=ifelse(Gp=="Main", '', ID)), 
                        min.segment.length = 0, size = 3, max.overlaps = 15) +
        theme(panel.background = element_rect(fill = "white"), 
                 panel.border = element_blank(), axis.line = element_line()) +
        labs(y = input$ylab, x = input$xlab) +
        scale_y_continuous(trans = "reverse")
    } else {
      ggplot(data=plot_gp_data) +
        geom_point(aes(x=LFC, y=Score, color = Gp), shape = 16, size = 3) +
        scale_color_manual(name = "Groups", labels = c("NA", all_labels()), values = cols()) +
        geom_text_repel(aes(x=LFC, y=Score, label=ifelse(Gp=="Main", '', ID)), 
                        min.segment.length = 0, size = 3, max.overlaps = 15) +
        theme(panel.background = element_rect(fill = "white"), 
                 panel.border = element_blank(), axis.line = element_line()) +
        labs(y = input$ylab, x = input$xlab)
    }
  })
  
  output$scatter <- renderPlot({
    make_scatter()
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
  
  selected1 <- reactiveVal({
    vector()
  })
  selected2 <- reactiveVal({
    vector()
  })
  selected3 <- reactiveVal({
    vector()
  })
  
  observeEvent(input$plot_click, {
    gp_sel <- paste0("gp", as.character(which(all_labels() == input$current_gp)))
    switch(gp_sel,
           gp1 = selected1(c(id_lab(), selected1())),
           gp2 = selected2(c(id_lab(), selected2())),
           gp3 = selected3(c(id_lab(), selected3()))
    )
  })
  
#Add brushed points to selected
  brush_sel <- reactive({
    brushedPoints(plot_LFC_data(), input$plot_brush)[,1]
  })
  observeEvent(input$plot_brush,{
    gp_sel <- paste0("gp", as.character(which(all_labels() == input$current_gp)))
    switch(gp_sel,
           gp1 = selected1(c(brush_sel(), selected1())),
           gp2 = selected2(c(brush_sel(), selected2())),
           gp3 = selected3(c(brush_sel(), selected3()))
    )
  })
  
  #If new plot generated, reset selected info
  observeEvent(input$plot, {
    selected1(vector())
    selected2(vector())
    selected3(vector())
  })
  
  
}


## shinyApp ----

shinyApp(ui, server)
