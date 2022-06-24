library(shiny)
library(shinyBS)
ui <- navbarPage("Custom Scatterplots", id = "tabs",
                 tabPanel("Plot",
                          titlePanel("Scatterplot with Custom Labels"),
                          sidebarLayout(
                            sidebarPanel(
                              bsCollapse(id = "collapseData", open = "Source Data",
                                         bsCollapsePanel("Source Data",
                                                         #Source data
                                                         fileInput("txt_data", "Supply tab delimited .txt file", width = "100%"),
                                                         #Option to preview data two ways
                                                         checkboxInput("preview", "Show preview of data"),
                                                         checkboxInput("summary", "Show summary of data"),
                                                         style = "success"
                                         ), #end collapse panel Source Data
                                         bsCollapsePanel("Data Attributes",
                                                         #Select data point labels
                                                         selectInput("name", "Labels for data points", choices = NULL),
                                                         #Colour data points
                                                         selectInput("col_m", "Main colour", choices = col_hex),
                                                         style = "success") #end Data Attributes collapse panel
                              ), #end collapseData bsCollapse
                              ), #end sidebarPanel
                            mainPanel(
                              actionButton("plot", "Construct Plot", style = "color: #fff; background-color: #337ab7; border-color: #2e6da4")
                            )
                          ) #end sidebarLayout
                 ),
                 tabPanel("Help", "This is the help tab")
                 )


server <- function(input, output, session) {

}

shinyApp(ui, server)
