library(shiny)
library(shinyBS)

#Example from :
#https://ebailey78.github.io/shinyBS/docs/Collapses.html
'
shinyApp(
  ui =
    fluidPage(
      sidebarLayout(
        sidebarPanel(HTML("This button will open Panel 1 using updateCollapse."),
                     actionButton("p1Button", "Push Me!"),
                     selectInput("styleSelect", "Select style for Panel 1",
                                 c("default", "primary", "danger", "warning", "info", "success"))
        ),
        mainPanel(
          bsCollapse(id = "collapseExample", open = "Panel 2",
                     bsCollapsePanel("Panel 1", "This is a panel with just text ",
                                     "and has the default style. You can change the style in ",
                                     "the sidebar.", style = "info"),
                     bsCollapsePanel("Panel 2", "This panel has a generic plot. ",
                                     "and a \'success\' style.", plotOutput("genericPlot"), style = "success")
          )
        )
      )
    ),
  server =
    function(input, output, session) {
      output$genericPlot <- renderPlot(plot(rnorm(100)))
      observeEvent(input$p1Button, ({
        updateCollapse(session, "collapseExample", open = "Panel 1")
      }))
      observeEvent(input$styleSelect, ({
        updateCollapse(session, "collapseExample", style = list("Panel 1" = input$styleSelect))
      }))
    }
)
'

ui <- fluidPage(
  #Title
  titlePanel("Scatterplot with Custom Labels"),
  
  #Sidebar Panel for data inputs & customization, Main Panel for plot
  sidebarLayout(
    sidebarPanel(
      fileInput("txt_data", "Supply tab delimited .txt file", width = "100%"),
      #Option to preview data two ways
      checkboxInput("preview", "Show preview of data"),
      checkboxInput("summary", "Show summary of data"),
      
      #Select data point labels
      selectInput("name", "Labels for data points", choices = NULL),
      #Colour data points
      selectInput("col_m", "Main colour", choices = c("1", "2")),
      
      #Collapsable for y information
      bsCollapse(id = "collapseYs",
                 bsCollapsePanel("Y-axis Attributes",
                                 radioButtons("num_y", "Number of Inputs for Y-axis", choices = c("1", "2"), inline = TRUE),
                                 conditionalPanel(condition = "input.num_y == 1",
                                                  selectInput("y1", "Y values", choices = NULL)),
                                 conditionalPanel(condition = "input.num_y == 2",
                                                  column(6, selectInput("yn", "Negative Score", choices = NULL)),
                                                  column(6, selectInput("yp", "Positive Score", choices = NULL))),
                                 textInput("ylab", "Attribute name for y values", placeholder = "e.g. Score (log10)"),
                                 checkboxGroupInput("y_trans", "Transformations for y-axis:", c("log10", "reverse"))
                                 ) #end Y attributes
                 ) #end collapseYs
      
      
    ), #end sidebarPanel
    
    mainPanel(
      actionButton("plot", "Generate/Reset Scatterplot")
    ) #end mainPanel
    
  ) #end sidebarLayout
) #end ui


server <- function(input, output, session) {
  
}

shinyApp(ui, server)