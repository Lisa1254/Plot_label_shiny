
#Practice for using click button to add more groups

library(shiny)
library(purrr)

col_hex <- setNames(c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7", "lightgray"), 
                    c("Black", "Orange", "Sky Blue", "Bluish Green", "Yellow", "Blue", "Red", "Purple", "Light Gray"))


condPan01 <- function(number) {
  conditionalPanel(paste0("input.num_gps >=", as.numeric(number)),
                   fluidRow(column(3, textInput(paste0("gp", number), label = paste0("Group Name: "))),
                            column(3, selectInput(paste0("col", number), label = paste0("Colour for Group ", number),
                                                  choices = col_hex))))
}
map_conds <- map(c(1,2,3), condPan01)

#updateSelIn01 <- function(id) {
#  id_num <- substr(deparse(substitute(id)),9,9)
#  updateSelectInput(paste0("col", id_num), label = paste0("Colour for ", id))
#}


ui <- fluidPage(
  sliderInput("num_gps", "How many accent groups?", min=0, max=3, step=1, value=1),
  map_conds,
  conditionalPanel("input.num_gps >=1",
                   radioButtons("current_gp", "Current group for labelling", choices = "none")),
  
  actionButton("plot", "Generate/Reset Scatterplot"),
  verbatimTextOutput("testx"),
  verbatimTextOutput("testx2"),
  fluidRow(column(3, verbatimTextOutput("test1")),
           column(3, verbatimTextOutput("test2")),
           column(3, verbatimTextOutput("test3"))),
  fluidRow(column(9, plotOutput("scatter", click = "plot_click", hover = "plot_hover", brush = "plot_brush")),
           column(3, tableOutput("nT_hover")))
)

server <- function(input, output, session) {
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

  
  #PLOT
  plot_data <- reactive({
    req(input$plot)
    data.frame(ID = paste0("Gene_", LETTERS[1:26]),
               X_Val = rnorm(26),
               Y_Val = rnorm(26))
  })
  
  gp_data <- reactive({
    rep_len("Main", nrow(plot_data()))
  })
  
  cols <- reactive({
    c("Main" = "black", "Gp1" = input$col1, "Gp2" = input$col2, "Gp3" = input$col3)[1:(input$num_gps+1)]
  })

  output$scatter <- renderPlot({
    input$plot
    plot_gp_data <- plot_data()
    plot_gp_data$Gp <- gp_data()
    plot_gp_data$Gp <- ifelse(plot_gp_data$ID %in% selected1(), "Gp1", plot_gp_data$Gp)
    plot_gp_data$Gp <- ifelse(plot_gp_data$ID %in% selected2(), "Gp2", plot_gp_data$Gp)
    plot_gp_data$Gp <- ifelse(plot_gp_data$ID %in% selected3(), "Gp3", plot_gp_data$Gp)
    ggplot(data=plot_gp_data) +
      geom_point(aes(x=X_Val, y=Y_Val, color = factor(Gp)), shape = 16, size = 3) +
      scale_color_manual(values = cols()) +
      geom_text_repel(aes(x=X_Val, y=Y_Val, label=ifelse(Gp=="Main",'', ID)), 
                      min.segment.length = 0, size = 3, max.overlaps = 15) +
      theme(legend.position = "none", 
            panel.background = element_rect(fill = "white"), 
            panel.border = element_blank(), axis.line = element_line()) #+
      #labs(y = input$ylab, x = input$xlab)
  })
  
  #On mouse hover, display Near Point data
  nearTable_h <- reactive({
    req(input$plot_hover)
    nearPoints(plot_data(), input$plot_hover)
  })
  
  output$nT_hover <- renderTable(nearTable_h())
  
  #Use NearPoints to add labels to plot on click
  
  id_lab <- reactive({
    req(input$plot_click)
    nearPoints(plot_data(), input$plot_click)[,1]
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
    brushedPoints(plot_data(), input$plot_brush)[,1]
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

shinyApp(ui, server)